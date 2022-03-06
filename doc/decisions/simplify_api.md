# Simplify API

## Problem

According to src/libs/elektra/symbols.map the public core API has 124 symbols¹, which
is arguable too much for a key-value API which has as highest goal simplicity.

In particular following areas have many functions and are not simple:

- binary keys
- memory functions
- comparison functions
- error/warnings

The urgency of this decision is that API can be easily introduced later but we cannot
get rid of it after 1.0.

## Constraints

To not disturb main features such as:

-

## Assumptions

Binary data is not a core feature, if needed the plugin system can also work without (properly tagged) binary data in key sets.

## Considered Alternatives

- flags for binary data

## Decision

The exact API changes are not listed here, because it would just a long list that could just as easily be found in the git history.

However, the API changes follow these rules:

- _Remove_ all functions related to key metadata, except those listed below
- _Remove_ all functions related to keyset cursors, as well as `ksHead` and `ksTail`
- _Remove_/_Change_ all functions related to (binary) key values as described in ([Binary](binary.md))
- _Remove_ all functions that use a user-provided buffer to return keyname/value/etc.
- _Change_/_Add_ `keyMeta`/`keySetMeta` to directly read/write the metadata KeySet of a Key
- _Rename_ `keyGet*Size` to `key*Size`
- _Add_ `ksRemove` function to remove a Key at a specific index
- _Remove from public API_ all the helper functions that use `elektraMalloc` (e.g. `elektraFormat`), as well as all the helper functions that only enhance standard APIs with additional error checks (e.g. `elektraStrCmp`)
- _Remove from public API_ `ksCut`, `ksDeepDup`, `ksCopyInternal` and other functions that should never have been public

- _Remove_ all functions that can be replaced by others (e.g. `ksPop`)
- _Rename_ all functions to start with `elektra` as described in [Elektra Prefix](elektra_prefix.md), and use `KeySet` instead of `ks` (also applies to names above)

Afterwards the Elektra Core Public API would look as follows:

#### KDB

- kdbOpen
- kdbClose
- kdbGet
- kdbSet

#### Key

- keyNew
- keyVNew
- keyCopy
- keyClear
- keyDel
- keyIncRef
- keyDecRef
- keyGetRef
- keyLock
- keyIsLocked
- keyDup

#### Meta

- keyMeta

#### Tests

- keyCmp
- keyNeedSync
- keyIsBelow
- keyIsBelowOrSame
- keyIsDirectlyBelow

#### Name

- keyName
- keySetName
- keyAddName
- keyNameSize
- keyUnescapedName
- keyUnescapedNameSize
- keyBaseName
- keySetBaseName
- keyAddBaseName
- keyBaseNameSize
- keyGetNamespace
- keySetNamespace

#### Value

- keyValue
- keyValueSize
- ketSetValue
- keyString

#### KeySet

- ksNew
- ksVNew
- ksDup
- ksCopy
- ksIncRef
- ksDecRef
- ksGetRef
- ksClear
- ksDel
- ksGetSize
- ksAppendKey
- ksAppend
- ksLookup
- ksLookupByName
- ksSearch
- ksRemove

## Rationale

## Implications

## Related Decisions

- [Binary](binary.md)
- [Elektra Prefix](elektra_prefix.md)

## Notes

¹ the 124 symbols are (as found by @kodebach):

- 6 for the KDB stuff
- 6 for the plugin system
- 48 for Key
- 31 for KeySet
- 15 other helper functions
- The other 18 symbols are the public constants for the error API.
