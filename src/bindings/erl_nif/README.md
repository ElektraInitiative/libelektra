# Erlang NIF bindings

This binding provides the possibility to access Elektra from Erlang.

## Module name

When compiling the module one needs to set a module name.
This is because the naming conventions differ between Erlang and Elixir.
The standard module name `Elixir.Elektra` is for an Elixir binding.

The module name can be set with the CMake option `ELEKTRA_NIF_MODULE_NAME`.

## Naming

All function names have been converted from camel case to snake case.
For instance, `ksNew` is available under the name `ks_new`.

## Differences to the C API and limitations

Due to the limitations of NIFs the signatures of the functions have not been translated faithfully and some functions are currently not implemented at all.

One major limitation is that any strings may only contain ASCII (ISO/IEC 8859-1 Latin 1) characters.

* `keyNew` takes a single argument which must be a valid key name
* `ksNew` takes a single argument which is the number of keys for which to pre-allocate memory

### Not implemented

* `keyVNew`
* `keyGetString`
* `keyGetBaseName`
* `ksVNew`
