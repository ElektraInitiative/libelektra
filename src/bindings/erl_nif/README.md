# Erlang NIF bindings

This binding provides the possibility to access Elektra from Erlang.

## Module name

The module name of the NIF module is `Elixir.Elektra.System`.

## Naming

All function names have been converted from camel case to snake case.
For instance, `ksNew` is available under the name `ks_new`.

## Differences to the C API and limitations

Due to the limitations of NIFs the signatures of the functions have not been translated faithfully and some functions are currently not implemented at all.

### Different arguments

- `keyNew` takes a single argument which must be a valid key name
- `ksNew` takes a single argument which is the number of keys for which to pre-allocate memory

### Not implemented

- `keyVNew`
- `keyGetString`
- `keyGetBaseName`
- `ksVNew`
