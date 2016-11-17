module SaySpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Say
import Control.Monad (forM_)
import Data.Text (pack)
import qualified Data.Text.IO as T
import System.IO
import System.IO.Temp (withSystemTempFile)
import qualified Data.ByteString as S
import Data.List (nub)

encodings :: [TextEncoding]
encodings = [utf8, utf16le, utf32be, char8]

newlines :: [NewlineMode]
newlines = nub
    [ noNewlineTranslation
    , universalNewlineMode
    , nativeNewlineMode
    , NewlineMode CRLF CRLF
    ]

bufferings :: [BufferMode]
bufferings =
    [ NoBuffering
    , LineBuffering
    , BlockBuffering Nothing
    , BlockBuffering $ Just 10
    , BlockBuffering $ Just 2048
    , BlockBuffering $ Just 30000
    ]

alts :: [(String, Handle -> String -> IO ())]
alts =
    [ ("String", hPutStrLn)
    , ("Text", \h -> T.hPutStrLn h . pack)
    ]

spec :: Spec
spec = do
  forM_ encodings $ \encoding -> describe ("Encoding: " ++ show encoding) $
    forM_ newlines $ \newline -> describe ("Newline: " ++ show newline) $
    forM_ bufferings $ \buffering -> describe ("Buffering: " ++ show buffering) $
    forM_ alts $ \(altName, altFunc) -> describe ("Versus: " ++ altName) $ do
      let prepHandle h = do
            hSetEncoding h encoding
            hSetNewlineMode h newline
            hSetBuffering h buffering

          test str =
            withSystemTempFile "say" $ \fpSay handleSay ->
            withSystemTempFile "alt" $ \fpAlt handleAlt -> do
              forM_ [(handleSay, \h -> hSay h . pack), (handleAlt, altFunc)] $ \(h, f) -> do
                  prepHandle h
                  f h str
                  hClose h
              bsSay <- S.readFile fpSay
              bsAlt <- S.readFile fpAlt
              bsSay `shouldBe` bsAlt

      prop "matches" test

      forM_ [10, 20, 100, 1000, 2047, 2048, 2049, 10000] $ \size -> do
          it ("size: " ++ show size) $ test $ replicate size 'A'
