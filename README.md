Native Deamon for resque-telework
=================================

The original daemon for [github.com/gip/resque-telework](https://github.com/gip/resque-telework) was written in Ruby and loads the full Rails stack which is a waste of resource really. This is a new implementation of the daemon in Haskell. Compilation produces optimised code that uses up to 40 times less resources than the same implementation in Ruby!

The daemon is currently being tested and is in beta. It does not currently support the auto mode.

Dependencies
============

The first release builds with GHC 7.4.2 and haskell-platform. Additionally, the following modules need to be installed: hedis (Redis client) and aeson (JSON [en|de]coder) using Cabal.

