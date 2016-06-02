**CAUTION: DO NOT USE THIS SOFTWARE**

The `cube-cotillion` library is a heavily Scotty-inspired framework
for writing services over SSH.

# Example

This example allows anyone to authenticate, and responds to two
commands, `greet` and `greet [name]`, with a short greeting.

~~~.haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid (mconcat)
import Network.CubeCotillion

main :: IO ()
main = do
  key <- loadKey "server-keys"
  cubeCotillion 8080 key $ do
    cmd "greet" $ do
      bs "Hello, world!\n"
    cmd "greet :name" $ do
      name <- param "name"
      bs $ mconcat ["Hello, ", name, "!\n"]
~~~

While running this service on localhost, we can connect to and
use it like so:

~~~
[gdritter@mu ~]$ ssh -p 8080 localhost greet
Hello, world!
[gdritter@mu ~]$ ssh -p 8080 localhost greet Eberhardt
Hello, Eberhardt!
~~~

# Why?

HTTP is often used as a protocol for exposing certain kinds of
services, but HTTP also lacks certain kinds of built-in features,
which are often reimplemented in various different ways: for
example, connection multiplexing, compression of conveyed
information, and user authentication and identity. All of these
are features trivially supported by the SSH protocol already.
Additionally, tools for working with SSH are ubiquitous, and
developers often already have existing SSH identities.

That doesn't necessarily mean that SSH is a great protocol to
use to build services on top of. I frankly don't _know_ if
that would be a good idea or not! That's why `cube-cotillion`
exists: to experiment with building these kinds of services
in a quick and easy way.

# Why The Name?

The design of the library is heavily inspired by the
lightweight Haskell web frameworks
[Scotty](http://hackage.haskell.org/package/scotty) and
[Spock](http://hackage.haskell.org/package/Spock), both
of which are named after Star Trek Characters. I figured
I should follow suit, and choose the name of one of my
[favorite Star Trip characters, too](https://www.youtube.com/watch?v=O2XOLoeBPEk).
