{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
module Say
    ( -- * Stdout
      say
    , sayString
    , sayShow
      -- * Stderr
    , sayErr
    , sayErrString
    , sayErrShow
      -- * Handle
    , hSay
    , hSayString
    , hSayShow
    ) where

import Control.Monad                   (join, void)
import Control.Monad.IO.Class          (MonadIO, liftIO)
import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as S8
import Data.IORef
import Data.Text                       (Text, pack)
import qualified Data.Text.Encoding    as TE
import Data.Text.Internal.Fusion       (stream)
import Data.Text.Internal.Fusion.Types (Step (..), Stream (..))
import GHC.IO.Buffer                   (Buffer (..), BufferState (..),
                                        CharBufElem, CharBuffer, RawCharBuffer,
                                        emptyBuffer, newCharBuffer,
                                        writeCharBuf)
import GHC.IO.Encoding.Types           (textEncodingName)
import GHC.IO.Handle.Internals         (wantWritableHandle)
import GHC.IO.Handle.Text              (commitBuffer')
import GHC.IO.Handle.Types             (BufferList (..), Handle__ (..))
import System.IO

#if MIN_VERSION_bytestring(0, 10, 4) && MIN_VERSION_text(1, 2, 0)
import qualified Data.ByteString.Builder as BB
import           Data.Monoid             (mappend)
#endif

-- | Send a 'Text' to standard output, appending a newline, and chunking the
-- data. By default, the chunk size is 2048 characters, so any messages below
-- that size will be sent as one contiguous unit. If larger messages are used,
-- it is possible for interleaving with other threads to occur.
--
-- @since 0.1.0.0
say :: MonadIO m => Text -> m ()
say = hSay stdout
{-# INLINE say #-}

-- | Same as 'say', but operates on a 'String'. Note that this will
-- force the entire @String@ into memory at once, and will fail for
-- infinite @String@s.
--
-- @since 0.1.0.0
sayString :: MonadIO m => String -> m ()
sayString = hSayString stdout
{-# INLINE sayString #-}

-- | Same as 'say', but for instances of 'Show'.
--
-- If your @Show@ instance generates infinite output, this will fail. However,
-- an infinite result for @show@ would generally be considered an invalid
-- instance anyway.
--
-- @since 0.1.0.0
sayShow :: (MonadIO m, Show a) => a -> m ()
sayShow = hSayShow stdout
{-# INLINE sayShow #-}

-- | Same as 'say', but data is sent to standard error.
--
-- @since 0.1.0.0
sayErr :: MonadIO m => Text -> m ()
sayErr = hSay stderr
{-# INLINE sayErr #-}

-- | Same as 'sayString', but data is sent to standard error.
--
-- @since 0.1.0.0
sayErrString :: MonadIO m => String -> m ()
sayErrString = hSayString stderr
{-# INLINE sayErrString #-}

-- | Same as 'sayShow', but data is sent to standard error.
--
-- @since 0.1.0.0
sayErrShow :: (MonadIO m, Show a) => a -> m ()
sayErrShow = hSayShow stderr
{-# INLINE sayErrShow #-}

-- | Same as 'say', but data is sent to the provided 'Handle'.
--
-- @since 0.1.0.0
hSay :: MonadIO m => Handle -> Text -> m ()
hSay h msg =
  liftIO $ join $ wantWritableHandle "hSay" h $ \h_ -> do
    case haOutputNL h_ of
      LF
        | fmap textEncodingName (haCodec h_) == Just "UTF-8" -> return viaUtf8
      nl -> do
        buf <- getSpareBuffer h_
        return $
          case nl of
            CRLF -> writeBlocksCRLF buf str
            LF   -> writeBlocksRaw  buf str
  -- Note that the release called below will return the buffer to the
  -- list of spares
  where
    str = stream msg

    viaUtf8 :: IO ()
#if MIN_VERSION_bytestring(0, 10, 4) && MIN_VERSION_text(1, 2, 0)
    viaUtf8 = BB.hPutBuilder h (TE.encodeUtf8Builder msg `mappend` BB.word8 10)
#else
    viaUtf8 = S8.hPut h (TE.encodeUtf8 msg `S.snoc` 10)
#endif

    getSpareBuffer :: Handle__ -> IO CharBuffer
    getSpareBuffer Handle__{haCharBuffer=ref, haBuffers=spare_ref} = do
        -- Despite appearances, IORef operations here are not a race
        -- condition, since we're already inside the MVar lock
        buf  <- readIORef ref
        bufs <- readIORef spare_ref
        case bufs of
            BufferListCons b rest -> do
                writeIORef spare_ref rest
                return (emptyBuffer b (bufSize buf) WriteBuffer)
            BufferListNil -> do
                new_buf <- newCharBuffer (bufSize buf) WriteBuffer
                return new_buf

    writeBlocksRaw :: Buffer CharBufElem -> Stream Char -> IO ()
    writeBlocksRaw buf0 (Stream next0 s0 _len) =
        outer s0 buf0
      where
        outer s1 Buffer{bufRaw=raw, bufSize=len} =
            inner s1 0
          where
            commit = commitBuffer h raw len
            inner !s !n =
              case next0 s of
                Done
                  | n + 1 >= len -> flush
                  | otherwise -> do
                    n1 <- writeCharBuf raw n '\n'
                    void $ commit n1 False{-no flush-} True{-release-}
                Skip s' -> inner s' n
                Yield x s'
                  | n + 1 >= len -> flush
                  | otherwise    -> writeCharBuf raw n x >>= inner s'
              where
                flush = commit n True{-needs flush-} False{-don't release-} >>= outer s

    writeBlocksCRLF :: Buffer CharBufElem -> Stream Char -> IO ()
    writeBlocksCRLF buf0 (Stream next0 s0 _len) =
        outer s0 buf0
      where
        outer s1 Buffer{bufRaw=raw, bufSize=len} =
            inner s1 0
          where
            commit = commitBuffer h raw len
            inner !s !n =
              case next0 s of
                Done
                  | n + 2 >= len -> flush
                  | otherwise -> do
                    n1 <- writeCharBuf raw n  '\r'
                    n2 <- writeCharBuf raw n1 '\n'
                    void $ commit n2 False{-no flush-} True{-release-}
                Skip s' -> inner s' n
                Yield '\n' s'
                  | n + 2 >= len -> flush
                  | otherwise    -> do
                      n1 <- writeCharBuf raw n  '\r'
                      n2 <- writeCharBuf raw n1 '\n'
                      inner s' n2
                Yield x s'
                  | n + 1 >= len -> flush
                  | otherwise    -> writeCharBuf raw n x >>= inner s'
              where
                flush = commit n True{-needs flush-} False{-don't release-} >>= outer s

    commitBuffer :: Handle -> RawCharBuffer -> Int -> Int -> Bool -> Bool
                 -> IO CharBuffer
    commitBuffer hdl !raw !sz !count flush release =
      wantWritableHandle "commitAndReleaseBuffer" hdl $
        commitBuffer' raw sz count flush release
{-# SPECIALIZE hSay :: Handle -> Text -> IO () #-}

-- | Same as 'sayString', but data is sent to the provided 'Handle'.
--
-- @since 0.1.0.0
hSayString :: MonadIO m => Handle -> String -> m ()
hSayString h = hSay h . pack
{-# INLINE hSayString #-}

-- | Same as 'sayShow', but data is sent to the provided 'Handle'.
--
-- @since 0.1.0.0
hSayShow :: (MonadIO m, Show a) => Handle -> a -> m ()
hSayShow h = hSayString h . show
{-# INLINE hSayShow #-}
