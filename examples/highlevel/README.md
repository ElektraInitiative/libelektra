# High-level API Examples

This folder contains an example on how to use the high-level API.

The example is provided for CMake and pkg-config build systems, but you can use any build system you like, as long as you setup
your include directories and linked libraries correctly. The high-level API uses the same include directory as the rest of elektra,
and you need to link against at least `elektra-core`, `elektra-highlevel`, `elektra-kdb` and `elektra-ease`.

You should therefore use one of the following lines of linker flags:

```
-lelektra-highlevel -lelektra-kdb -lelektra-ease -lelektra-core

# or if Elektra was compiled with BUILD_FULL
-lelektra-full

# or if Elektra was compiled with BUILD_STATIC (see Limitations below)
-lelektra-static
```

## Setup

Before executing the example you need to run the following snippet. Otherwise the examples will fail, because no proper specification
was provided.

```sh
cd examples/highlevel
sudo kdb mount spec.ini 'spec:/sw/example/highlevel/#0/current' ni
sudo kdb import 'spec:/sw/example/highlevel/#0/current' ni < spec.ini
sudo kdb spec-mount '/sw/example/highlevel/#0/current'
```

## Execution

The example just prints its configuration if `/sw/example/highlevel/#0/current/print` is set to `1` (boolean true):

```sh
# If you want to print the configuration values:
kdb set /sw/example/highlevel/#0/current/print 1
```

. Otherwise it will just read the configuration and print a success message.

## Limitations

The pkg-config example will only work for the `BUILD_SHARED` and `BUILD_FULL` variants of Elektra.
To make pkg-config work with `BUILD_STATIC` you need to change the Makefile. You can use a C compiler for compilation, but you need to
use a C++ Compiler for linking and also need to link with `-ldbus-1` (depending on PLUGINS), `-lz`, `-lm` and `-pthread`.

Note also that in a real-world build you should be careful with using `-Wl,-rpath`. In most cases you should only use it for development
purposes and not in a release build. Therefore you should not use the Makefile provided in the pkg-config example for release builds.

The CMake example should always work, because CMake should detect the correct way of linking Elektra.
