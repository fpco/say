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
* Replace `print` with `say'`

In addition, `sayErr` and `sayErr'` work on standard error instead, and `hSay`
and `hSay'` work on arbitrary `Handle`s.
