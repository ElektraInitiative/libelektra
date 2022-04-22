### Constructor Functions: Language examples

This is not a decision.
This file contains additional information for [Constructor Functions](constructor_functions.md).

## Rust

For example, `libelektra-rust` (targeted at Rust) could provide:

```rust
trait KeyBuilder {
    fn new(ns: Namespace, name: &[u8]) -> Self;
    fn set_value(&mut self, value: &[u8]) -> Self;
    fn set_meta(&mut self, name: &[u8], value: &[u8]) -> Self;
    fn build(self) -> Key;
}
```

In Rust it is possible to access the length of a `&[u8]` (otherwise similar to a `uint8_t *`), therefore the issue of separate `size` arguments doesn't come up.
The builder pattern makes it possible to implement `build` without calling `elektraKeyNew` and instead allocating everything in one go (if benchmarks show this is more performant).

Similarly, for keysets we could provide:

```rust
trait KeySet {
    fn new (keys: &[Key]) -> Self;
}
```

## C

For C it is a bit more complicated.
There are many possible APIs that improve upon `elektraKeyNew`.
However, it is somewhat difficult to design such an API, while fulfilling the 3 main goals:

1. Must be performant
2. Creating keys with arbitrary name, value and metadata in a single expression should be (easily) possible.
3. Variadic arguments should be avoided; they make it more difficult to use arguments that aren't compile-time-static.

The following is an attempt to showcase such an API:

The idea revolves around a public `struct` and a `static inline` function.

```c
// holds a string (with potential zero bytes) of `size` length
// if `ns == ELEKTRA_NS_NONE`, the `name` and `size` are directly useable in struct _Key
// otherwise:
//  - `size` is the length of `name` including the terminating zero byte, but without a namespace
//  - `name` does not contain a namespace
//  - the full name is constructed from `ns` and `name`
struct ElektraKeyName {
    size_t size;
    const char * name;
    ElektraNamespace ns;
};

static inline struct ElektraKeyName elektraKeyName(ElektraNamespace ns, const char * parts[])
{
    struct ElektraKeyName k;
    k.ns = ELEKTRA_NS_NONE;

    k.size = 2;
    for (size_t i = 0; parts[i] != NULL; ++i)
    {
        k.size += strlen(parts[i]) + 1;
    }

    char * name = malloc(k.size);
    name[0] = ns;
    name[1] = 0;
    k.size = 2;
    for (size_t i = 0; parts[i] != NULL; ++i)
    {
        size_t s = strlen(parts[i]);
        memcpy(name + k.size, parts[i], s);
        name[k.size + s] = 0;
        k.size += s + 1;
    }
    k.name = name;
    return k;
}
```

The above would be part of a header file.
This allows us to define a `elektraKeyCreate` function like this:

```c
Key * elektraKeyCreate(struct ElektraKeyName name, void * value, size_t valueSize, KeySet * meta)
```

All of this may seem odd at first, but combining the arguments for namespace, name and size of name into a struct, allows us to create them via macros (which makes the syntax nicer).

```c
#define ELEKTRA_BUILD_NAME(ns, ...) (elektraKeyName(ns, (const char *[]){__VA_ARGS__,NULL}))
#define ELEKTRA_KEY_NAME(_ns, _name, _size) ((struct ElektraKeyName){.size = _size, .name = _name, .ns = _ns})
#define ELEKTRA_LITERAL_NAME(_ns, _name) (ELEKTRA_KEY_NAME(_ns, _name, sizeof(""_name)))
```

All in all the effect is that `elektraKeyCreate` can be called in many different ways:

```c
struct ElektraKeyName y = ELEKTRA_BUILD_NAME(ELEKTRA_NS_CASCADING, "abc", "def", "xyz", "123", "456", "ghi", "foo", "lll", "kyt");
Key * k = elektraKeyCreate(y, NULL, 0, NULL);

Key * k1 = elektraKeyCreate(ELEKTRA_BUILD_NAME(ELEKTRA_NS_CASCADING, "abc", "def"), NULL, 0, NULL);

Key * k2 = elektraKeyCreate(ELEKTRA_LITERAL_NAME(ELEKTRA_NS_CASCADING, "foo\0bar"), NULL, 0, NULL);

const char *x = "foo\0bar";
Key * k3 = elektraKeyCreate(ELEKTRA_KEY_NAME(ELEKTRA_NS_CASCADING, x, 8), NULL, 0, NULL);

const char *a = "foo";
const char *b = "bar";
Key * k4 = elektraKeyCreate(ELEKTRA_BUILD_NAME(ELEKTRA_NS_CASCADING, "abc", a, b, "def"), NULL, 0, NULL);
```

Crucially when the `static inline elektraKeyName` function is used, we directly use the `elektraMalloc`ed buffer.
This in turn makes it possible for the compiler (with `-O3`) to do lots of constant folding and (in theory) fully calculate the key name at compile-time in many instances.
After inlining and constant folding, only the `malloc` with a precomputed size and a (vectorized) `memcpy` from static memory to the heap would remain.

This is of course just theory, but at least `clang` version 13.0.1 seems to achieve this.
The results with other compilers need to be investigated further.

Note also, that only the creation of the key name is inlined.
The rest is still hidden behind a layer of abstraction, so this does not leak the layout of `struct _Key` to the API consumer.
Inlining the allocation of the key name is not a problem, because the structure of the key name is already public API.
