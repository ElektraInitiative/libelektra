# Iterating Keyname Parts

## Problem

While iterating over the parts of a keyname is easy for an experienced developer (just jump from `\0` to `\0` until you hit the name size), there is no good API that newcomers can use.

## Constraints

No changes to the underlying APIs, while maintaining reasonable performance.

## Assumptions

## Considered Alternatives

- Put the new function into `libelektra-core`.

## Decision

Add this new function to a separate library (name TBD):

```c
/**
 * Iterate over the key name parts of @p key.
 *
 * @pre @p currentPart, must be `NULL` or point anywhere into the unescaped name of @p key
 *
 * This function defines the "next part" after @p currentPart as:
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

## Rationale

The function is not required for a minimal API, but it is useful for people who don't know everything about the internals of Elektra.

## Implications

## Related Decisions

## Notes
