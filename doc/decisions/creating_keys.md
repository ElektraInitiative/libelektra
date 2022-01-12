# Creating Keys

## Problem

The API that `libelektra-core` defines for creating Keys must be possible to implement in other languages without complicated workarounds.
In particular, another implementation should not need to resort to adding a small amount of C to "bridge the gap".

## Constraints

- There should be a way to create a `Key *` with name, value and metadata in a single expression.

  This is needed to allow creating a full `KeySet *` with a single expression, which in turn is very useful e.g. when initializing default configurations.

- Using the escaped name in the constructor function should be avoided, since the unescaping process can be expensive.
- Ideally, creating a `Key *` without metadata should use at most 3 `malloc`s and 2 `memcpy` to:

  1. `malloc` to allocate the `struct _Key`.
  2. `malloc` to allocate the key name and `memcpy` to initialize it.
  3. `malloc` to allocate the key value and `memcpy` to initialize it.
     This constraint is not absolute, but it should be a goal as long as it doesn't complicate the API to much.

## Assumptions

- Many languages have trouble with C's variadic arguments.
- Languages will provide a secondary library with more idiomatic APIs on top of `libelektra-core`.

  If necessary, this library will also use unstable APIs from `libelektra-core` and in particular access `struct _Key` directly.

## Considered Alternatives

- Create a constructor function based on heavy use of arrays and `NULL` terminators
- A simple function that takes `char * name`, `void * value` and `KeySet * meta`.

  The size of the name and value would have to be passed separately (a potential source of errors).

## Decision

`libelektra-core` only provides the absolute minimal API to create a `Key *`:

```c
/**
 * Allocates a new key with the name "/" and returns it.
 *
 * The key will have no value and no metadata.
 */
Key * keyCreate (void);
```

Other libraries will provide APIs on top of this function.
These functions should be more idiomatic to their target language.

### Rust

For example, `libelektra-rust` (targeted at Rust) could provide:

```rust
trait Key {
    fn new(ns: Namespace, name: &[u8]) -> Self;
    fn set_value(&mut self, value: &[u8]) -> Self;
    fn set_meta(&mut self, name: &[u8], value: &[u8]) -> Self;
}
```

In Rust it is possible to access the length of a `&[u8]` (otherwise similar to a `uint8_t *`), therefore the issue of separate `size` arguments doesn't come up.

### C

For C it is a bit more complicated.
`libelektra-operations` could provide an API based on variadic arguments:

```c
// must be NULL, because everything else could in theory be valid char * for a name part
#define KEY_NAME_END NULL

keyNew (KEY_NS_USER, "foo", "bar", KEY_NAME_END,
        KEY_VALUE_STRING, "value",
        KEY_META, keyNew (KEY_NS_META, "type", KEY_NAME_END, KEY_VALUE_STRING, "string", KEY_END),
        KEY_META, keyNew (KEY_NS_META, "other", KEY_NAME_END, KEY_VALUE_BINARY, &mystruct, sizeof(mystruct), KEY_END),
        KEY_END);
```

However, this would require multiple `memcpy` to initialize the key name.

Alternatively, macros could be used extensively to hide the size arguments from (most) users:

```c
keyNew(NS_USER, keyNameEmpty)
keyNew(NS_USER, keyName("foo", "bar"))
keyNewS(NS_USER, keyName("foo", "foo"), "value")
keyNewV(NS_USER, keyName("foo", "foo", "bar"), "value", 3)
keyNewSM(NS_USER, keyName("foo", "baz"), "value", keyMeta(keyName("type"), "string"), keyMeta(keyName("other"), "aaa"))
keyNewVM(NS_USER, keyName("foo", "qux"), "value", 4, keyMeta(keyName("type"), "string"))
```

The definitions of the macros are complicated, but this would expand to something like:

```c
ksNew(
    keyBuild(NS_USER, ((struct KeyName){ NULL, 0 }), NULL, 0, NULL),
    keyBuild(NS_USER, ((struct KeyName){"foo" "\0" "bar", 0 + strlen("bar") + 1 + strlen("foo" "\0" "bar")}), NULL, 0, NULL),
    keyBuild(NS_USER, ((struct KeyName){"foo" "\0" "foo", 0 + strlen("foo") + 1 + strlen("foo" "\0" "foo")}), "value", strlen("value"), NULL),
    keyBuild(NS_USER, ((struct KeyName){"foo" "\0" "foo" "\0" "bar", 0 + strlen("bar") + 1 + strlen("foo" "\0" "bar") + 1 + strlen("foo" "\0" "foo" "\0" "bar")}), "value", 3, NULL),
    keyBuild(NS_USER, ((struct KeyName){"foo" "\0" "baz", 0 + strlen("baz") + 1 + strlen("foo" "\0" "baz")}), "value", strlen("value"), ksNew(
        keyBuild(NS_META, ((struct KeyName){"type", 0 + strlen("type")}), "string", strlen("string"), NULL),
        keyBuild(NS_META, ((struct KeyName){"other", 0 + strlen("other")}), "aaa", strlen("aaa"), NULL)
    )),
    keyBuild(NS_USER, ((struct KeyName){"foo" "\0" "qux", 0 + strlen("qux") + 1 + strlen("foo" "\0" "qux")}), "value", 4, ksNew(
        keyBuild(NS_META, ((struct KeyName){"type", 0 + strlen("type")}), "string", strlen("string"), NULL)
    ))
)
```

> **Note:** For the definition of `ksNew` see [Creating KeySets](creating_keysets.md).

The `keyName` macro only works for literal strings, so if the name is not known at compile time, you'd need to use `keyBuild` directly:

```c
struct KeyName {
    const char * name; // unecaped name, contains '\0' chars
    size_t size;
};

Key * keyBuild(elektraNamespace ns, struct KeyName name, const void * value, size_t vSize, const KeySet * meta);
```

## Rationale

It is very hard to find a C API that works well in every language, but is still useful for creating keys.
It is much easier, to provide good APIs for a single language and build those on top of a very basic generic API.

This decision also guides Elektra towards the goal of a minimal core API.

See also Assumptions, Constraints and Considered Alternatives above.

## Implications

- When using `libelektra-core` exclusively, you cannot create a `Key *` with a single expression.

## Related Decisions

- [Creating KeySets](creating_keysets.md)

## Notes

TODO: decide the exact C API in `libelektra-operations`
