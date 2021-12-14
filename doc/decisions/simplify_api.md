# Simplify API

## Problem

According to src/libs/elektra/symbols.map the public core API has 124 functions, which
is arguable too much for a key/value API which has as highest goal simplicity.

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

Remove/Refactor/Rename obsolete/not needed methods:
- keyClear
- keyGetRef
- keyRewindMeta
- keyNextMeta
- keyCurrentMeta
- keyCmp
- keyNeedSync
- keyIsBelow
- keyIsBelowOrSame
- keyIsDirectlyBelow
- keyIsBinary
- keyIsString
- keyName
- keyGetBaseNameSize
- keyGetBaseName
- keyValue
- keyString
- keyGetString
- keySetString
- keyGetBinary
- keySetBinary
- ksClear
- ksCut
- ksPop
- ksHead
- ksTail
- ksLookupByName

## Rationale

## Implications

## Related Decisions

## Notes
