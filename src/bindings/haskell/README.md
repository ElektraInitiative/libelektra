- infos =
- infos/author = Armin Wurzinger <e1528532@student.tuwien.ac.at>
- infos/status = experimental maintained
- infos/provides =
- infos/description =

# Haskell Bindings
A full haskell binding using c2hs.

## Usage
The bindings are built with cabal, Haskell's default build system. It needs the 
GHC, cabal and C2HS, which generates the actual bindings out of a metacode and 
will be called during the build automatically.

There are some examples how the bindings can be used in
[examples](examples/). Furthermore there exists the possibility 
to write [plugins](/src/plugins/haskell/) using the haskell bindings.

To build and test it manually call `cabal test` in <build_folder>/src/bindings/haskell.
Otherwise the bindings will be installed along with elektra if the bindings are included 
in the build, so usually this shouldn't be necessary to do.

To open the bindings in ghci to play around with them call
`cabal repl` in the <build_folder>/src/bindings/haskell. 
If the bindings have already been installed along with elektra, they should be 
registered in the global cabal database already. Therefore you can easily use 
them with `ghci`, by calling `:m Elektra.Key Elektra.KDB Elektra.KeySet Elektra.Plugin`
inside ghci.

## Dependencies

- GHC (Glasgow Haskell Compiler) > 8.0.0
- cabal > 1.24.0.2  (older versions may work but untested, 2.0.0.0 works as well)

GHC and cabal can be either obtained using a package manager or downloaded directly
from [the Haskell homepage](https://www.haskell.org/platform/). 

Before installing the remaining dependencies, make sure to update your cabal database
which contains the current list of haskell packages by invoking `cabal update`, this
is not done automatically.

- c2hs > 0.28.2 (older versions may work but are untested)
  c2hs is available in some package managers and should be installed from there in that case.
  If c2hs is not available in the package manager (e.g. homebrew on macOS does not include it),
  it can be installed via cabal by invoking `cabal update && cabal install c2hs`.

## Limitations

- no binary keys
- functions which return a string value with a maximum size argument don't 
  include the size argument anymore, but instead allocate memory as required
  	- keyGetName behaves like keyName
  	- keyGetFullName doesn't has a size argument either
  	- keyGetBaseName behaves like keyBaseName
  	- keyUnescapedName returns a String, the null separators are written with \0
- BUILD_STATIC is currently not compatible with the haskell bindings
