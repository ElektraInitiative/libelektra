# High-level API Examples

This folder contains an example on how to use the high-level API.

The example is provided for CMake and pkg-config build systems, but you can use any build system you like, as long as you setup
your include directories and linked libraries correctly. The high-level API uses the same include directory as the rest of elektra,
and you need to link against at least `elektra`, `elektra-highlevel` and `elektra-kdb`.

## Setup

Before executing the example you need to run the following snippet. Otherwise the examples will fail, because no proper specification
was provided.

```sh
sudo kdb mount spec.ini spec/sw/example/highlevel/#0/current ni
kdb import spec/sw/example/highlevel/#0/current ni < spec.ini
sudo kdb spec-mount '/sw/example/highlevel/#0/current'
```

## Execution

The example just prints its configuration if `/sw/example/highlevel/#0/current/print` is set to `1` (boolean true):

```sh
# If you want to print the configuration values:
kdb set /sw/example/highlevel/#0/current/print 1
```

. Otherwise it will just read the configuration an print a success message.
