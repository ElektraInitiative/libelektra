### Constructor Functions: Language examples

This is not a decision.
This file contains additional information for [Constructor Functions](constructor_functions.md).

## Rust

For example, `libelektra-rust` (targeted at Rust) could provide:

```rust
trait Key {
    fn new(ns: Namespace, name: &[u8]) -> Self;
    fn set_value(&mut self, value: &[u8]) -> Self;
    fn set_meta(&mut self, name: &[u8], value: &[u8]) -> Self;
}
```

In Rust it is possible to access the length of a `&[u8]` (otherwise similar to a `uint8_t *`), therefore the issue of separate `size` arguments doesn't come up.

## C

For C it is a bit more complicated.
`libelektra-operations` could provide an API based on variadic arguments:

```c
// KEY_NAME_END must be NULL, because everything else could in theory be valid char * for a name part

elektraKeyCreate (KEY_NS_USER, "foo", "bar", KEY_NAME_END,
        KEY_VALUE_STRING, "value",
        KEY_META, elektraKeyCreate (KEY_NS_META, "type", KEY_NAME_END, KEY_VALUE_STRING, "string", KEY_END),
        KEY_META, elektraKeyCreate (KEY_NS_META, "other", KEY_NAME_END, KEY_VALUE_BINARY, &mystruct, sizeof(mystruct), KEY_END),
        KEY_END);
```

However, this would require multiple `memcpy` to initialize the key name.

Alternatively, macros could be used extensively to hide the size arguments from (most) users:

```c
Key * k1 = elektraKeyCreate (NS_USER, elektraKeyNameEmpty);
Key * k2 = elektraKeyCreate (NS_USER, elektraKeyName("foo", "bar"));
Key * k3 = elektraKeyCreateS (NS_USER, elektraKeyName("foo", "foo"), "value");
Key * k4 = elektraKeyCreateV (NS_USER, elektraKeyName("foo", "foo", "bar"), "value", 3);
Key * k5 = elektraKeyCreateSM (NS_USER, elektraKeyName("foo", "baz"), "value", elektraKeyCreateM(elektraKeyName("type"), "string"), elektraKeyCreateM(elektraKeyName("other"), "aaa"));
Key * k6 = elektraKeyCreateVM (NS_USER, elektraKeyName("foo", "qux"), "value", 4, elektraKeyCreateM(elektraKeyName("type"), "string"));
```

The definitions of the macros are complicated, but this would expand to something like:

```c
Key * k1 = elektraKeyBuild(NS_USER, ((struct KeyName){ NULL, 0 }), NULL, 0, NULL);
Key * k2 = elektraKeyBuild(NS_USER, ((struct KeyName){"foo" "\0" "bar", 0 + strlen("bar") + 1 + strlen("foo" "\0" "bar")}), NULL, 0, NULL);
Key * k3 = elektraKeyBuild(NS_USER, ((struct KeyName){"foo" "\0" "foo", 0 + strlen("foo") + 1 + strlen("foo" "\0" "foo")}), "value", strlen("value") + 1, NULL);
Key * k4 = elektraKeyBuild(NS_USER, ((struct KeyName){"foo" "\0" "foo" "\0" "bar", 0 + strlen("bar") + 1 + strlen("foo" "\0" "bar") + 1 + strlen("foo" "\0" "foo" "\0" "bar")}), "value", 3, NULL);
Key * k5 = elektraKeyBuild(NS_USER, ((struct KeyName){"foo" "\0" "baz", 0 + strlen("baz") + 1 + strlen("foo" "\0" "baz")}), "value", strlen("value") + 1, ksNew(
                elektraKeyBuild(NS_META, ((struct KeyName){"type", 0 + strlen("type")}), "string", strlen("string") + 1, NULL),
                elektraKeyBuild(NS_META, ((struct KeyName){"other", 0 + strlen("other")}), "aaa", strlen("aaa") + 1, NULL)
        ));
Key * k6 = elektraKeyBuild(NS_USER, ((struct KeyName){"foo" "\0" "qux", 0 + strlen("qux") + 1 + strlen("foo" "\0" "qux")}), "value", 4, ksNew(
                elektraKeyBuild(NS_META, ((struct KeyName){"type", 0 + strlen("type")}), "string", strlen("string") + 1, NULL)
        ));
```

The `elektraKeyName` macro only works for literal strings, so if the name is not known at compile time, you'd need to use `elektraKeyBuild` directly:

```c
struct KeyName {
    const char * name; // unescaped name, contains '\0' chars
    size_t size;
};

Key * elektraKeyBuild(elektraNamespace ns, struct KeyName name, const void * value, size_t vSize, const KeySet * meta);
```

However, here we can also add a secondary function `elektraKeyBuildFromParts`, which would build the name from parts with multiple `memcpy` (something that cannot be avoided with non-literal strings):

```c
Key * elektraKeyBuildFromParts(elektraNamespace ns, const char ** nameParts, const void * value, size_t vSize, const KeySet * meta);
```

This could be use with non-literal strings like so:

```c
elektraKeyBuildFromParts(NS_USER, ((const char*[]) { "foo", bar, "baz" }), "value", sizeof("value"), NULL);
```

For KeySets it is a lot simpler, we simply add this macro:

```c
#define elektraKsCreate(...) elektraKsNew((Key *[]){__VA_ARGS__}, sizeof((Key *[]){__VA_ARGS__})/sizeof(Key*))
```

This allows use to simply write:

```c
KeySet * ks = elektraKsCreate(k1, k2, k3);
```
