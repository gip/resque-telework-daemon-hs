Native Deamon for resque-telework
=================================

The original daemon for [github.com/gip/resque-telework](https://github.com/gip/resque-telework) was written in Ruby and loads the full Rails stack which is a waste of resource really. This is a new implementation of the daemon in Haskell. Compilation produces optimised code that uses 10 times less resources than the same implementation in Ruby.

The daemon is currently being tested and is in beta. 

