# `keyIsBelow`

## Problem

There are multiple very similar functions to check the "is below" relation between to keys:

- Replacement `keyIsBelow`
- Replacement `keyIsBelowOrSame`
- Replacement `keyIsDirectlyBelow`

## Constraints

Keep (at least) the same functionality

## Assumptions

## Considered Alternatives

## Decision

Merge `keyIsBelow` with `keyIsBelowOrSame` and `keyIsDirectlyBelow` to create this new API:

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

The replacements for the old functions are:

- old keyIsBelow: `keyIsBelow > 0`
- keyIsBelowOrSame: `keyIsBelow >= 0`
- keyIsDirectlyBelow: `keyIsBelow == 1`

## Rationale

## Implications

## Related Decisions

## Notes
