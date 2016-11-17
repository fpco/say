{-# LANGUAGE BangPatterns #-}
module Say
    ( -- * Stdout
      say
    , say'
      -- * Stderr
    , sayErr
    , sayErr'
      -- * Handle
    , hSay
    , hSay'
    ) where

import Control.Monad                   (void)
import Control.Monad.IO.Class          (MonadIO, liftIO)
import Data.IORef
import Data.Text                       (Text, pack)
import Data.Text.Internal.Fusion       (stream)
import Data.Text.Internal.Fusion.Types (Step (..), Stream (..))
import GHC.IO.Buffer                   (Buffer (..), BufferState (..),
                                        CharBufElem, CharBuffer, RawCharBuffer,
                                        emptyBuffer, newCharBuffer,
                                        writeCharBuf)
import GHC.IO.Handle.Internals         (wantWritableHandle)
import GHC.IO.Handle.Text              (commitBuffer')
import GHC.IO.Handle.Types             (BufferList (..), Handle__ (..))
import System.IO

-- | Send a 'Text' to standard output, appending a newline, and chunking the
-- data. By default, the chunk size is 2048 characters, so any messages below
-- that size will be sent as one contiguous unit. If larger messages are used,
-- it is possible for interleaving with other threads to occur.
--
-- @since 0.1.0.0
say :: MonadIO m => Text -> m ()
say = hSay stdout
{-# INLINE say #-}

-- | Same as 'say', but for instances of 'Show'.
--
-- If your @Show@ instance generates infinite output, this will fail. However,
-- an infinite result for @show@ would generally be considered an invalid
-- instance anyway.
--
-- @since 0.1.0.0
say' :: (MonadIO m, Show a) => a -> m ()
say' = hSay' stdout
{-# INLINE say' #-}

-- | Same as 'say', but data is sent to standard error.
--
-- @since 0.1.0.0
sayErr :: MonadIO m => Text -> m ()
sayErr = hSay stderr
{-# INLINE sayErr #-}

-- | Same as 'say'', but data is sent to standard error.
--
-- @since 0.1.0.0
sayErr' :: (MonadIO m, Show a) => a -> m ()
sayErr' = hSay' stderr
{-# INLINE sayErr' #-}

-- | Same as 'say', but data is sent to the provided 'Handle'.
--
-- @since 0.1.0.0
hSay :: MonadIO m => Handle -> Text -> m ()
hSay h msg = liftIO $ do
  (buf, nl) <- wantWritableHandle "hSay" h $ \h_ -> do
    buf <- getSpareBuffer h_
    return (buf, haOutputNL h_)
  case nl of
    CRLF -> writeBlocksCRLF buf str
    LF   -> writeBlocksRaw  buf str
  -- Note that the release called below will return the buffer to the
  -- list of spares
  where
    str = stream msg

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

-- | Same as 'say'', but data is sent to the provided 'Handle'.
--
-- @since 0.1.0.0
hSay' :: (MonadIO m, Show a) => Handle -> a -> m ()
hSay' h = hSay h . pack . show
{-# INLINE hSay' #-}
