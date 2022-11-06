# Key Name

## Problem

Often a `Key` argument is used when you just need a key name.
This is because with a `Key` we know the name is valid and we get an unescaped name.
Using a `Key` here makes the API a bit confusing.

There could be a richer API for manipulating key names without relying on the escaped name (e.g. concatenating two full key names).
With the current situation, all these functions would need to be part of the API for a key. Adding such functions to the key API is certainly not minimal.

## Constraints

## Assumptions

## Considered Alternatives

- add separate `struct KeyName`

## Decision

Continue keeping 3 classes: `Key`, `KeySet` and `KDB`.

## Rationale

- A minimal `Key` ideally only consists of a key name and the goal is to keep `Key` small, so introducing `KeyName` would go the wrong direction.

## Implications

- Thus, operations working on key names, directly use `Key` as argument.
- The key name is actually, in every implementation, a plain string.
  This is also required, because implementations must use the same memory layout.

## Related Decisions

- [Null](../5_implemented/null.md)
- [Store the escaped and/or unescaped key name](../2_in_progress/store_name.md)

## Notes

Text partly copied from @kodebach https://github.com/ElektraInitiative/libelektra/pull/4201#pullrequestreview-840564988
