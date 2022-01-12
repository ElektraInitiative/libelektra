# Creating KeySets

## Problem

The API that `libelektra-core` defines for creating KeySets must be possible to implement in other languages without complicated workarounds.
In particular, another implementation should not need to resort to adding a small amount of C to "bridge the gap".

## Constraints

- There should be a way to create a `KeySet *` containing an arbitrary number of keys in a single expression.

  This is very useful e.g. when initializing default configurations.

- Ideally, there should be a way to create a `KeySet *` directly from a `Key *[]`.

  A `Key *[]` can be created in a single expression `((Key *[]){ key1, key2, key3 })` and then sorted with a single `qsort`.
  Most other methods require repeated `ksAppendKey`, which essentially amounts to Insertion Sort instead of Quicksort.

## Assumptions

- Many languages have trouble with C's variadic arguments.
- Languages will provide a secondary library with more idiomatic APIs on top of `libelektra-core`.
  If necessary, this library will also use unstable APIs from `libelektra-core` and in particular access `struct _KeySet` directly.

## Considered Alternatives

- Provide `KeySet * ksFromArray (const Key ** keys, size_t size)` within `libelektra-core`.

  The separate size argument that is required in C could be avoided in most other languages.
  This API is also not entirely minimal.

## Decision

`libelektra-core` only provides the absolute minimal API to create a `KeySet *`:

```c
/**
 * Allocates a new empty keyset, with space reserved for @p alloc keys.
 */
Key * ksNew (size_t alloc);
```

Other libraries will provide APIs on top of this function.
These functions should be more idiomatic to their target language.

### Rust

For example, `libelektra-rust` (targeted at Rust) could provide:

```rust
trait KeySet {
    fn new<I: ExactSizeIterator<Item = Key>>(keys: I) -> Self;
}
```

In Rust it is possible to access the length of `keys`, therefore the issue of separate `size` arguments doesn't come up.

### C

For C we simply add the `ksFromArray` API that was considered as an alternative to
`libelektra-operations`:

```c
/**
 * Creates a KeySet with the first @p size Keys from @p keys
 */
KeySet * ksFromArray(const Key ** keys, size_t size);
```

Additionally, we will add a `ksNew` macro that makes it easier to use this API:

```c
#define ksNew(...) ksFromArray((const Key *[]){__VA_ARGS__}, sizeof((const Key *[]){__VA_ARGS__})/sizeof(Key*))
```

## Rationale

It is very hard to find a C API that works well in every language, but is still useful for creating keys.
It is much easier, to provide good APIs for a single language and build those on top of a very basic generic API.

This decision also guides Elektra towards the goal of a minimal core API.

See also Assumptions, Constraints and Considered Alternatives above.

## Implications

- When using `libelektra-core` exclusively, you cannot create a `KeySet *` with a single expression.

## Related Decisions

- [Creating Keys](creating_keys.md)

## Notes

TODO: decide the exact C API in `libelektra-operations`
