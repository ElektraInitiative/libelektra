# Constructor Functions

## Problem

The constructor functions for creating Keys and KeySets must be easy to be invoked from other languages without complicated workarounds.
In particular, another implementation should not need to resort to adding a small amount of C to "bridge the gap".

## Constraints

- Constructor functions should use as little resources as feasible.
  In particular, the number of `malloc`s and `memcpy` should be kept in check.
  Temporary allocations during key creation should be avoided.
- Using the escaped name in the constructor function should be avoided, since the unescaping process can be expensive.

## Assumptions

- The constructor functions from `libelektra-core` will not or only very rarely be used directly by users.
- For all languages supported by Elektra (including C), there is secondary library on top of `libelektra-core` that provides more idiomatic APIs.
- Creating a key with a name, no value and no metadata and then calling `keySetValue`/`keySetMeta` does not create significant overhead compared to creating the key with value and metadata directly.

  > **Note:** The current `keyNew` just calls `keySetValue`/`keySetMeta` internally.

## Considered Alternatives

- Create a constructor function based on heavy use of arrays and `NULL` terminators
- A single function that takes `char * name`, `void * value` and `KeySet * meta` for Keys or a `Key **` for KeySets.
  Sizes would need to be passed as separate arguments.

## Decision

`libelektra-core` only provides the absolute minimal API to create a `Key *` or `KeySet *`:

```c
/**
 * Allocates a new key and returns it.
 * The key will be in the namespace @p ns.
 *
 * If `name == NULL`, the returned key is the root key of @p ns and @p nameSize is ignored.
 * If `nameSize == 0`, the returned key is the root key of @p ns and @p name is ignored.
 * Otherwise, the name of the key within the namespace will be @p name of length @p nameSize (including null terminator).
 * Therefore, the returned key `k` fullfills:
 * `keyNameSize (k) == nameSize + 2`
 * (one byte for the namespace, and one for the separator between namespace and the rest of the name)
 *
 *
 * The key will have no value and no metadata.
 */
Key * elektraKeyNew (elektraNamespace ns, const char * name, size_t nameSize);

/**
 * Allocates a new keyset with space reserved for at least @p keyCount keys.
 * If `keyCount == 0`, the keyset will be empty (with no space for keys reserved) and @p keys is ignored.
 * If `keys == NULL`, the keyset will be empty with space reserved for at least @p keyCount keys.
 * Otherwise the keyset will contain all `keys[i]` for any `0 <= i < keyCount` where `keys[i] != NULL`.
 * In other words, `NULL`s within @p keys will be ignored and if @p keys contains more than @p keyCount keys
 * the additional ones will also be ignored.
 */
Key * elektraKeySetNew (Key ** keys, size_t keyCount);
```

Other libraries will provide APIs on top of these functions.
These functions should be more idiomatic to their target language.

### Language Examples

There are some examples of what the language specific APIs could look like in [another file](constructor_functions_examples.md).

## Rationale

It is very hard to find a C API that works well in every language, but is still useful for creating keys.
It is much easier, to provide good APIs for a single language and build those on top of a very basic generic API.

This decision also guides Elektra towards the goal of a minimal core API.

## Implications

- When using `libelektra-core` exclusively, you cannot create a `Key *` with a single expression.

## Related Decisions

- [Creating KeySets](creating_keysets.md)

## Notes

TODO: decide the exact C API in `libelektra-operations`
