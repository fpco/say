import Gauge
import qualified System.IO as IO
import UnliftIO.Temporary
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Say

main :: IO ()
main = withSystemTempFile "say-bench" $ \_ h -> do
    defaultMain $ map (doSize h)
        [ 10
        , 100
        , 1000
        , 10000
        , 100000
        ]

doSize :: IO.Handle -> Int -> Benchmark
doSize h size = bgroup (show size)
    [ doString h "ascii" $ replicate size 'A'
    , doString h "non-ascii" $ replicate size 'א'
    ]

doString :: IO.Handle -> String -> String -> Benchmark
doString h name' str = bgroup name'
    [ doTest "string" IO.hPutStrLn str
    , doTest "Text: putStrLn" TIO.hPutStrLn text
    , doTest "Text: putStr" (\h t -> TIO.hPutStrLn h (T.snoc t '\n')) text
    , doTest "say" hSay text
    , doTest "sayString" hSayString str
    , doTest "BS: putStrLn + encodeUtf8" (\h t -> S8.hPutStrLn h (TE.encodeUtf8 t)) text
    , doTest "BS: putStr + encodeUtf8" (\h t -> S8.hPutStrLn h (S.snoc (TE.encodeUtf8 t) 10)) text
    ]
  where
    text = T.pack str

    doTest name f x = bench name $ whnfIO $ do
      f h x
      IO.hFlush h
