# Haskell Bindings
A full java binding using JNA. This binding requires Elektra installed on
the system to work.

## Usage
The bindings are built with cabal, Haskell's default build system. It needs a recent GHC version (>8),
cabal and C2HS, which generates the actual bindings out of a metacode and will be called during the build
automatically (Call `cabal install c2hs` to install it globally).

To build and test it call `cabal test`.
To open the bindings in ghci to play around with them call `cabal repl --ghc-options="-lelektra"`. 

## Limitations

- no binary keys
- some low-level methods are not supported as they don't fit into haskell nicely
