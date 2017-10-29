# Haskell Bindings
A full haskell binding using c2hs.

## Usage
The bindings are built with cabal, Haskell's default build system. It needs the 
GHC, cabal and C2HS, which generates the actual bindings out of a metacode and 
will be called during the build automatically (Call `cabal install c2hs` to 
install it globally).

To build and test it call `cabal test`.
To open the bindings in ghci to play around with them call
`cabal repl --ghc-options="-lelektra"`. 

## Limitations

- no binary keys
- functions which return a string value with a maximum size argument don't 
  include the size argument anymore, but instead allocate memory as required
  	- keyGetName behaves like keyName
  	- keyGetFullName doesn't has a size argument either
  	- keyGetBaseName behaves like keyBaseName
  	- keyUnescapedName returns a String, the null separators are written with \0
- BUILD_STATIC is currently not compatible with the haskell bindings