## say

Send textual messages to a `Handle` in a thread-friendly way.

[![Build Status](https://travis-ci.org/fpco/say.svg?branch=master)](https://travis-ci.org/fpco/say) [![Build status](https://ci.appveyor.com/api/projects/status/v628d8r2iq1kxfx5?svg=true)](https://ci.appveyor.com/project/snoyberg/say)

The motivation for this package is described in [a blog post on Haskell's
Missing Concurrency
Basics](http://www.snoyman.com/blog/2016/11/haskells-missing-concurrency-basics).
The simple explanation is, when writing a line of textual data to a `Handle` -
such as sending some messages t o ther terminal - we'd like to have the
following properties:

* Properly handle character encoding settings on the `Handle`
* For reasonably sized messages, ensure that the entire message is written in
  one chunk to avoid interleaving data with other threads
    * This includes the trailing newline character
* Avoid unnecessary memory allocations and copies
* Minimize locking
* Provide a simple API

On the last point: for the most part, you can make the following substitutions
in your API usage:

* Replace `putStrLn` with `say`
* Replace `print` with `sayShow`
* If you're using a `String` instead of `Text`, replace `putStrLn` with `sayString`

In addition, `sayErr`, `sayErrString` and `sayErrShow` work on
standard error instead, and `hSay`, `hSayString` and `hSayShow` work
on arbitrary `Handle`s.

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-6.23 runghc --package async --package say
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad            (forM_, void)
import Say                      (sayString)

worker :: Int -> IO ()
worker ident = forM_ [1..1000] $ \msg -> sayString $ concat
    [ "Hello, I am worker #"
    , show ident
    , ", and this is message #"
    , show msg
    ]

main :: IO ()
main = void $ mapConcurrently worker [1..100]
```
