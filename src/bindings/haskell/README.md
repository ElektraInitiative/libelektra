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

- GHC (Glasgow Haskell Compiler) > 8.0.1 && < 8.4
- cabal > 1.24.0.2  (older versions may work but untested, 2.0.0.0 works as well)

GHC and cabal can be either obtained using a package manager or downloaded directly
from [the Haskell homepage](https://www.haskell.org/platform/). 

Before installing the remaining dependencies, make sure to update your cabal database
which contains the current list of haskell packages by invoking `cabal update`, this
is not done automatically. Then first ensure the following programs are available on
the PATH:

- c2hs (tested with 0.28.2)
- alex (tested with 3.2.4)
- happy (tested with 1.19.9)

All three depedendencies are available in some package managers and should be installed from 
there in that case. If one or all of them are not available in the package manager 
(e.g. homebrew on macOS does not include it), they can be installed via cabal by invoking 
`cabal update && cabal install c2hs && cabal install alex && cabal install happy`.

Next a cabal sandbox containing all the haskell dependencies has to be created. By 
default, it is assumed to be the folder `.cabal-sandbox` in the build directory.
In case you want to install them to a separate location this is supported as well,
the build system will consider the environment variable `HASKELL_SHARED_SANDBOX`.

First create the sandbox at a location of your choice by executing the command 
`cabal sandbox init --sandbox <path>`. This will create a new sandbox where the
dependencies will get installed into. Please note that the following commands have to
be executed in the *directory containing the sandbox directory specified above*,
otherwise cabal would install the dependencies globally instead. This is generally not
wanted to avoid conflicts and issues due to existing incompatible dependencies. So if you
created the sandbox with `cabal sandbox init --sandbox /home/user/elektra-cabal-sandbox`
the following install command has to be executed inside `/home/user/`.

Before configuring the project via cmake, make sure you have set the environment variable 
to the sandbox' location unless you are using the standard folder as described above.

In case you simply want to install the bindings, but no other haskell based plugins, 
install the following dependencies into the sandbox:

```
cabal install 'containers >=0.4 && <0.6' 'base >=4.7 && <5' 'directory >=1.1 && <1.4' \
'process >=1.2 && <1.7' 'filepath >=1.3 && <1.5' 'base -any' 'hspec -any' 'QuickCheck -any'
```

In case you want to install every haskell based plugin, use this command instead:

```
cabal install 'containers >=0.4 && <0.6' 'base >=4.7 && <5' 'directory >=1.1 && <1.4' \
'process >=1.2 && <1.7' 'filepath >=1.3 && <1.5' 'base -any' 'hspec -any' 'QuickCheck -any'
```

Now everything should be ready and awaits compilation.

## Limitations

- no binary keys
- functions which return a string value with a maximum size argument don't 
  include the size argument anymore, but instead allocate memory as required
  	- keyGetName behaves like keyName
  	- keyGetFullName doesn't has a size argument either
  	- keyGetBaseName behaves like keyBaseName
  	- keyUnescapedName returns a String, the null separators are written with \0
- BUILD_STATIC is currently not compatible with the haskell bindings
