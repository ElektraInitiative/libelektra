# API based on use cases for `libelektra-core`

## Create `Key`

This use case is implemented as `keyNew()`.

## `Key` Name

The name of a `Key` is stored in two forms: escaped and unescaped.

The escaped from can be accessed via `keyName()` and the unescaped form via `keyUnescapedName()`.

Manipulating the name is possible via the `keySetName()`, `keySetBaseName()`, `keyAddName()` and `keyAddBaseName()` functions, which allow various kinds of manipulation.

## `Key` Namespace

This use case implemented via the `keyGetNamespace()` and `keySetNamespace()` functions.

## `Key` Value

The value of a `Key` can be accessed via `keyValue()` and `keyString()`.
It can be changed via `keySetString()` and `keySetBinary()`.

## `Key` Metadata

This use case is implemented as the `keyMeta()` function.

### `Key` Ordering

This use case is implemented as the `keyCmp()` function.

## `Key` Hierarchy

This use case is currently implemented via the `keyIsBelow()`, `keyIsBelowOrSame()` and `keyIsDirectlyBelow()` functions, instead of being a single function.

## Create `KeySet`

This use case is implemented as `ksNew()`.
Beyond the described use case, the `ksNew()` function also supports creating a `KeySet` with predefined contents.

## Insert `Key` into `KeySet`

This use case is implemented as `ksAppendKey()` and `ksAppend()`.

## Remove `Key` from `KeySet`

This use case is implemented via `ksLookup()` with the `KDB_O_POP` option.

## Direct lookup in `KeySet`

This use case is implemented via `ksLookup()`.

## Cascading Lookup in `KeySet`

This use case is implemented via `ksLookup()`.

## Index access to `KeySet`

This use case is implemented as `ksAtCursor()`.

## Cut `Key` hierarchy from `KeySet`

This use case is implemented as `ksCut()`.
