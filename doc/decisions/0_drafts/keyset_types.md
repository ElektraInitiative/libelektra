# Types of `KeySet`s

## Problem

A `KeySet` can be used in different ways.
Among these are at least:

- As originally intended, a set of configuration data
- Metadata of a `Key`
- General associative array data structure

Only KeySets with one kind of keys makes sense.
These types have slightly different requirements, and therefore need to be treated differently.
Currently keys of different types can be arbitrary intermixed, leading to many error variants and error code that is time consuming (full iteration is needed).

## Constraints

- API duplication just to separate types should be avoided
- API must be safe to use for each of the different types

## Assumptions

- The types are not different enough to warrant the overhead of entirely different APIs and data structures.

## Considered Alternatives

### No restrictions

Leave `KeySet` entirely unrestricted, so it can be used for each use case.

Sometimes there can be odd edge cases.
For example, what should a storage plugin do, if it receives a `KeySet` with both configuration data and metadata.

### Restriction based on first `Key`

To effectively create different kinds of `KeySet`s, we restrict which `Key`s can be inserted into a `KeySet` based on the first `Key` in a `KeySet`.

Any `Key` except cascading keys can be inserted into an empty `KeySet`.
Only `Key`s matching the type of a `KeySet` can be inserted into a non-empty `KeySet`.
The type of a `KeySet` is determined by the first `Key`, i.e. `ksAtCursor(ks, 0)`.
Transitively, this means the type of a `KeySet` is fixed when the first `Key` is inserted and when a `KeySet` is cleared, it becomes "type-less" again.

The different types of `KeySet` will be:

1. Metadata:
   Can only contain `Key`s with namespace `KEY_NS_META`.
2. General data:
   Can only contain `Key`s with namespace `KEY_NS_DATA`.
3. Config data:
   Can only contain a mix of `Key`s with namespace `KEY_NS_SPEC`, `KEY_NS_PROC`, `KEY_NS_DIR`, `KEY_NS_USER`, `KEY_NS_SYSTEM` and `KEY_NS_DEFAULT`.

Importantly, `Key`s with namespace `KEY_NS_CASCADING` can never be inserted into a `KeySet`.
This is a restriction that simplifies the logic for cascading lookups.
Instead the `KEY_NS_DATA` namespace should be used.

Cascading lookups always try the namespaces allowed in the type of `KeySet` in order, but never try `KEY_NS_SPEC`.
This means cascading lookups work for all types of `KeySet` the way a user would expect.

### Different structs and APIs

Extract the implementation of `KeySet` into an internal API (and possibly data structure).
Expose a different type and API for each of the use cases.

This creates unnecessary overhead and at least some code duplication, even if it is minimized by extracting the implementation.

### KeySets also have Namespaces

Add a `ksSetNamespace` API which allows to change the namespace of a KeySet.
This requires an alias `KEY_NS_CONFIG` to the namespaces `KEY_NS_SPEC`, `KEY_NS_PROC`, `KEY_NS_DIR`, `KEY_NS_USER`, `KEY_NS_SYSTEM` and `KEY_NS_DEFAULT`.

Only keys of the namespace set to `ksSetNamespace` can be added.
`ksSetNamespace` fails if called after keys were added.

This alternative conflicts with the constraint of minimal API.
## Decision

**Suggestion:** Go with "Restriction based on first `Key`"

## Rationale

Some kind of separation is necessary to avoid issues with e.g. `KeySet * keyMeta(Key * key)`
This way we avoid API/code duplication and still allow slightly different handling of the different types.

## Implications

- Some code needs to be updated from using cascading keys for general data to the new `KEY_NS_DATA`

## Related Decisions

## Notes
