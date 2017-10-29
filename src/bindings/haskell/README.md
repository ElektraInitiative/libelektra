# Haskell Bindings
A full haskell binding using c2hs.

## Usage
The bindings are built with cabal, Haskell's default build system. It needs the 
GHC, cabal and C2HS, which generates the actual bindings out of a metacode and 
will be called during the build automatically.

There are some examples how the bindings can be used in
[examples](src/bindings/haskell/examples). Furthermore there exists the possibility 
to write [plugins](src/plugins/haskell/) using the haskell bindings.

To build and test it manually call `cabal test`. Otherwise the bindings will be 
installed along with elektra if the bindings are included in the build.

To open the bindings in ghci to play around with them call
`cabal repl --ghc-options="-lelektra"` in this folder. 

## Dependencies

- GHC (Glasgow Haskell Compiler) > 8.0.0 (older versions may work but are untested)
- cabal > 1.24.0.2  (older versions may work but untested, 2.0.0.0 works as well)

GHC and cabal can be either obtained using a package manager or downloaded directly
from [the Haskell homepage](https://www.haskell.org/platform/).

Before installing the remaining dependencies, make sure to update your cabal database
which contains the current list of haskell packages by invoking `cabal update`, this
is not done automatically.

- c2hs `cabal install c2hs` > 0.28.2 (older versions untested)
- hspec `cabal install hspec` (only if the tests are being built, v2.4.4 tested)
- QuickCheck `cabal install QuickCheck` (only if the tests are being built, v2.10.1 tested)

## Limitations

- no binary keys
- functions which return a string value with a maximum size argument don't 
  include the size argument anymore, but instead allocate memory as required
  	- keyGetName behaves like keyName
  	- keyGetFullName doesn't has a size argument either
  	- keyGetBaseName behaves like keyBaseName
  	- keyUnescapedName returns a String, the null separators are written with \0
- BUILD_STATIC is currently not compatible with the haskell bindings