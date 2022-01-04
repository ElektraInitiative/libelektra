# Simplify API

## Problem

According to src/libs/elektra/symbols.map the public core API has 124 functions, which
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

Remove:

- keyGetMeta (@kodebach)
- keySetMeta (@kodebach)
- keyRewindMeta
- keyNextMeta
- keyCurrentMeta
- keyCopyAllMeta
- keyCopyMeta
- keyGetBaseName
- keyGetBaseNameSize
- keyGetBinary (\*buffers.md)
- keyGetName
- keyGetNameSize
- ksHead
- ksTail
- ksCopyInternal
- keyClear (clarification with keyCopy needed)
- keyCompare (done)
- keyCompareMeta (done)
- keyIsBinary ([Binary](binary.md))
- keyIsString ([Binary](binary.md))
- keyGetString ([Binary](binary.md))
- keySetString ([Binary](binary.md))
- keyGetBinary ([Binary](binary.md))
- keySetBinary ([Binary](binary.md))

Rename:

- keyGetValueSize -> keyValueSize
- keyGetBaseNameSize -> keyBaseNameSize
- keyGetNameSize -> keyNameSize

Make private:

- elektraStrCaseCmp;
- elektraStrCmp;
- elektraStrDup;
- elektraStrLen;
- elektraStrNCaseCmp;
- elektraStrNCmp;
- elektraVFormat;
- ksDeepDup
- ksGetAlloc
- ksInit
- keyGetRef

Unclear:

- keyCmp
- ksPop
- keyNeedSync
- keyIsBelow
- keyIsBelowOrSame
- keyIsDirectlyBelow
- keyName
- keyGetBaseName
- ksClear
- ksCut
- keyGetUnescapedNameSize

## Rationale

## Implications

## Related Decisions

- [Binary](binary.md)

## Notes
