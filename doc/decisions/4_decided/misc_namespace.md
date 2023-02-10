# Namespace for miscellaneous data

## Problem

A `KeySet` is powerful data structure, which could be use for data other than configuration data.
Especially with the hashmap implementation, using a `KeySet` for generic data can be useful.
For this generic data we do not care about the namespace-related features.

However, because a `Key` cannot exist without a namespace, we need to choose a namespace.

## Constraints

- The outcome must be compatible with the ["Types of `KeySet`s" decision](../2_solutions_clear/keyset_types.md).

## Assumptions

- The ["Types of `KeySet`s" decision](../2_solutions_clear/keyset_types.md) leads to separate types of `KeySet`s that are restricted w.r.t. the namespaces they may contain.

## Considered Alternatives

### Use cascading `Key`s

We could use the existing `KEY_NS_CASCADING` for one of the types of `KeySet`s.

This may be confusing to users of the APIs.
The purpose of `KEY_NS_CASCADING` becomes muddied.
It is no longer just for lookup, but would now have a secondary purpose.

### Introduce separate `KEY_NS_MISC` namespace

We introduce a new `KEY_NS_MISC`, which is used exclusively for generic and miscellaneous data.
It implies a separate [type of `KeySet`](../2_solutions_clear/keyset_types.md).

This leaves `KEY_NS_CASCADING` for its original purpose, while still solving the issue with generic data.

## Decision

Introduce separate `KEY_NS_MISC` namespace.

In the unescaped form this will be `\x09` (byte with decimal value `9`).
In the escaped form it will be `misc:/`.

`KEY_NS_MISC` has a separate [type of `KeySet`](../2_solutions_clear/keyset_types.md).
Such a `KeySet` can only contain `KEY_NS_MISC` keys.
It may not be used for metadata or with `kdbGet`/`kdbSet`.

A cascading lookup is still allowed for such `KeySet`s.
However, the `KEY_NS_CASCADING` will simply be replaced with `KEY_NS_MISC` and then an exact name lookup is performed, as if the `KEY_NS_MISC` namespace was given directly.

## Rationale

Introducing a new namespace is much cleaner than reusing one of the existing ones.
It fits better with the separate [types of `KeySet`s](../2_solutions_clear/keyset_types.md).

## Implications

- Some code already uses cascading keys for general data.
  It must be updated to the new `KEY_NS_MISC`.
- The cascading logic for `ksLookup` needs to adapted for `KEY_NS_MISC`.

## Related Decisions

- ["Types of `KeySet`s"](../2_solutions_clear/keyset_types.md).

## Notes
