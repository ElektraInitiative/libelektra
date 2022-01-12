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

Remove:

- keyGetMeta (@kodebach)
- keySetMeta (@kodebach)
- keyRewindMeta
- keyNextMeta
- keyCurrentMeta
- keyCopyAllMeta
- keyCopyMeta
- keyGetBaseName
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
- ksCut (maybe later introduce ksFindHierarchy, ksRemoveRange, ksCopyRange)
- ksPop (replaced by `ksRemove (ks, ksGetSize (ks) - 1)`)
- keyIsBelow (merged with keyIsBelowOrSame and keyIsDirectlyBelow, see below)
- keyIsBelowOrSame (merged into keyIsBelow, see below)
- keyIsDirectlyBelow (merged into keyIsBelow, see below)

Rename:

- keyGetValueSize -> keyValueSize
- keyGetBaseNameSize -> keyBaseNameSize
- keyName -> keyEscapedName ([Escaped Name](escaped_name.md))
- keyGetNameSize -> keyEscapedNameSize
- keyGetUnescapedNameSize -> keyNameSize
- elektraKsPopAtCursor -> ksRemove (also need to make public)
- keyCmp -> keyCompareName

Merge:

- keyIsBelow: merge with keyIsBelowOrSame, keyIsDirectlyBelow
  ```c
  /**
   * Checks if @p other is below @p base.
   *
   * **Note**
   *   This function treats cascading key names similar to `ksLookup`.
   *   For example, /foo is treated as equal to user:/foo, system:/foo, etc.
   *
   * @retval 0 @p base and @p other have the same name
   * @retval 1 @p other is directly below @p base
   * @retval > 1 @p other is below @p base (but not directly)
   * @retval < 0 otherwise, i.e. @p other is above @p base,
   *             the keys do not have a common parent, or
   *             (at least) one of @p other and @p base is NULL
   */
  int keyIsBelow (const Key * base, const Key * other);
  ```
  - Replacement for old keyIsBelow: `keyIsBelow > 0`
  - Replacement for keyIsBelowOrSame: `keyIsBelow >= 0`
  - Replacement for keyIsDirectlyBelow: `keyIsBelow == 1`

Make private:

- elektraStrCaseCmp;
- elektraStrCmp;
- elektraStrDup;
- elektraStrLen;
- elektraStrNCaseCmp;
- elektraStrNCmp;
- elektraVFormat;
- elektraFormat;

Move/Add to `libelektra-operations` (actual name TBD):

- ksDeepDup
- ksGetAlloc
- ksInit
- keyGetRef
- keyNeedSync
- keyGetNextPart: new API

```c
/**
 * Iterate over the key name parts of @p key.
 *
 * @pre @p currentPart, must be `NULL` or point anywhere into the unescaped name of @p key
 *
 * This function defins the "next part" after @p currentPart as:
 * - the first part of @p key (after namespace), if `currentPart == NULL`
 * - `NULL`, if @p currentPart is the last part of @p key, i.e. `currentPart == keyBaseName (key)`
 * - the next part within @p key after the location @p currentPart points to
 *
 * To iterate over the parts of a key you can use:
 * @code{.c}
 * Key * k;
 * for (const char * part = keyGetNextPart (k, NULL); part != NULL; part = keyGetNextPart (k, part))
 * {
 *    // use part
 * }
 * @endcode
 *
 *
 * @returns the "next" (see above) key name part after @p currentPart.
 */
const char * keyGetNextPart (Key * key, const char * currentPart);
```

Unclear:

- ksClear: move to `libelektra-operations` or keep in `libelektra-core`?

Change internals:

- keyGetBaseNameSize: store base name size in `struct _Key` to optimize keyBaseName

## Rationale

## Implications

## Related Decisions

- [Binary](binary.md)

## Notes

¹ the 124 symbols are (as found by @kodebach):

- 6 for the KDB stuff
- 6 for the plugin system
- 48 for Key
- 31 for KeySet
- 15 other helper functions
- The other 18 symbols are the public constants for the error API.
