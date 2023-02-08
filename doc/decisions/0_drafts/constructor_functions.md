# Constructor Functions for `Key` and `KeySet`

## Problem

The structs for `Key` and `KeySet` are opaque, i.e., only the `typedef`s are part of the public headers, the actual `struct` definitions are in a private header.
Because of that, `libelektra-core` must provide a way to construct a new `Key` or `KeySet`.

However, since both `Key` and `KeySet` are rather complex structures consisting of multiple parts (name, value, metadata, and collection of `Key`s respectively), it is not straightforward to create the best API for these functions.

## Constraints

- Elektra has many bindings.
  The constructor functions must be callable from all these bindings, with no additional effort.
  In particular, that means the constructors must not use macros.
- Constructor functions should use as little resources as feasible.
  `malloc` and `memcpy` calls should be kept to a minimum and temporary allocations should be avoided.
- In accordance with the ["Namespace and Name of Keys" decision](../4_decided/keyname.md), the constructor functions must not use the escaped name.
  Only the unescaped name may be used, but the namespace may be passed as a separate parameter, if this has benefits.

## Assumptions

- There will be additional [builder functions](../0_drafts/builder_functions.md) built on top of the constructor functions.
  These builder functions will live outside `libelektra-core`, but user code is expected to use builders rather than calling constructors directly.
- Sometimes, it may be desirable to have a function that can construct a `Key`, including name, value and metadata, or even whole `KeySet` in a single call.
  We assume these cases overlap almost entirely, with the cases where we expect builder functions to be used.
  Therefore, it doesn't matter much, if the constructor functions don't allow this can of `Key`/`KeySet` construction and extra calls to e.g., `keySetValue` are needed.

## Considered Alternatives

### No Arguments

The constructor functions are just specialized allocators:

```c
Key * keyNew (void);
KeySet * ksNew (void);
```

While this would work decently well for `ksNew` (don't allocate array, only allocate array on first `Key` insertion), there is an issue for `keyNew`.
A `Key` must have a name.
Therefore, `keyNew` must set some default name in the `Key` it returns.

Even if there was a suitable default name, it would still be wasteful, since in many (almost all) cases, the name will soon be replaced via a `keySetName` call.

A clear advantage of this option is that the very simple API means the functions are callable without issue from basically any language.

### Minimal Arguments

Instead of taking no arguments at all, the constructor functions take the minimal number of arguments:

```c
// ns is separate to allow usage of ELEKTRA_NS_* constants
// size is needed because name can contain \0
Key * keyNew (ElektraNamespace ns, const char * name, size_t size);
KeySet * ksNew (size_t alloc);
```

This solves the default name issue for `keyNew`.
For `ksNew` there was no issue and the `alloc` parameter isn't strictly speaking needed, but it can still be helpful.
For example, if the caller knows they will insert 100 keys, the can call `ksNew (100)` to avoid later allocations to resize the array in the `KeySet`.

The API is also still simple enough that it can be called from any binding.

#### Sidenote: Bundle struct

Depending on the rest of the `libelektra-core` API, it may make sense to use a public struct to bundle the arguments of `keyNew`:

```c
typedef struct {
  ElektraNamespace ns;
  const char * name;
  size_t size;
} KeyName;

Key * keyNewStruct (KeyName name);
// called as e.g.: keyNew ((KeyName){.ns = ELEKTRA_NS_SYSTEM, .name = "foo\0bar", .size = 7 })
Key * keyNewStructAlt (const KeyName * name);
// called as e.g.: keyNew (&(KeyName){.ns = ELEKTRA_NS_SYSTEM, .name = "foo\0bar", .size = 7 })
```

Whether the struct would be passed by value or as a pointer also depends on the rest of the API.

An important distinction between `Key` and `KeyName` in this solution is that `Key` can be seen as more of class, while `KeyName` is just a bundle of fields.
That is why `Key` is opaque and `KeyName` would be public.
The fields of a `Key` are implementation details, but `KeyName` is just a kind of alias for its fields.

### Common Arguments

Lots of `Key`s will have a value from the moment they are created, e.g., `meta:/` keys are rarely created without a value.
Therefore, it might make sense if `keyNew` took an optional (=nullable) argument for the value of the key:

```c
typedef struct {
  const void * value;
  size_t size;
} KeyValue;

Key * keyNew (const KeyName * name, const KeyValue * value);
// called as e.g.:
// keyNew ((KeyName){.ns = ELEKTRA_NS_SYSTEM, .name = "foo\0bar", .size = 7 }, NULL)
// keyNew ((KeyName){.ns = ELEKTRA_NS_SYSTEM, .name = "foo\0bar", .size = 7 }, (KeyValue){.value = "1234", .size = 5})
```

> **Note**: Because this solution is much easier with the bundle structs, we use them here.
> It would work without them as well, but we'd need two optional arguments (pointer and size) for the value.
> Similarly, passing the bundle struct by value would mean you have to pass `(KeyValue){ .value = NULL, .size = 0 }` instead of just `NULL`.

For `KeySet`, we can pass a list of `Key *` to initialize the `KeySet` with:

```c
// every variadic argument must be a Key *, the last argument must be NULL
KeySet * ksNew (size_t alloc, ...);
```

This API is easy to call in C:

```c
KeySet * ks = ksNew (3, key1, key2, key3, NULL);
```

However, many languages for which we provide bindings can't use variadic arguments.
So it would make more sense to have a function like this only in a C-specific library (`libelektra-lowlevel-c`), and have the version from above in `libelektra-core`.

An alternative would be to use an array argument:

```c
// last element of `keys` must be NULL
KeySet * ksNew (size_t alloc, Key * keys[]);
```

This is easier to call from other languages, but it's slightly more cumbersome in C:

```c
KeySet * ks = ksNew (3, (Key*[]){key1, key2, key3, NULL});
```

The array parameter also has some other advantages.
We could copy it with a single `memcpy` and use something like `qsort`, instead of copying the `Key *` one by one from the variadic arguments.
Also, a `Key *[]` provides more type safety compared to a variadic arguments.

### Full Arguments

For `ksNew` the above solution already uses the full set of arguments to initialize a `KeySet` fully.

For `Key` we'd also need to take metadata.

> **Note**: This solution is described only for completeness's sake.
> We assume that ["builder functions"](../0_drafts/builder_functions.md) exist outside of `libelektra-core`.
> With this solution those builders would be superfluous.

#### Variadic Arguments

This could be done by using a system of variadic arguments, called like so:

```c
// system:/foo/bar with value 1234 and metadata: meta:/type=long, meta:/length/min=4
keyNew (ELEKTRA_NS_SYSTEM, "foo", "bar", NULL, KEY_VALUE, 5, "1234", KEY_META, "type", NULL, "long", KEY_META, "length", "min", NULL, 4, NULL);
// system:/foo/bar with value 1234 and no metadata
keyNew (ELEKTRA_NS_SYSTEM, "foo", "bar", NULL, KEY_VALUE, 5, "1234", NULL);
// system:/foo/bar with no value and no metadata
keyNew (ELEKTRA_NS_SYSTEM, "foo", "bar", NULL, NULL);
```

However, as discussed above, such functions are hard to call from many other languages.
You also loose type safety in C and the function is not particularly intuitive to use.
To emphasize this last point, consider that the signature for the above `keyNew` would likely be:

```c
Key * keyNew (ElektraNamespace ns, ...);
```

Things can be slightly improved, by passing the keynames as a single string, but it still a bad API, with lost of potential for misuse.

#### Metadata Array

A better option would be to build on the bundle structs option from above, by adding an new struct for metadata:

```c
typedef struct {
  // no namespace, because that is always ELEKTRA_NS_META
  const char * name;
  size_t nameSize;
  KeyValue value;
} KeyMeta;

Key * keyNew (const KeyName * name, const KeyValue * value, const KeyMeta metadata[]);
```

This could be called as:

```c
// system:/foo/bar with value 1234 and metadata: meta:/type=long, meta:/length/min=4
keyNew (
  &(KeyName){.ns = ELEKTRA_NS_SYSTEM, .name = "foo\0bar", .size = 8 },
  &(KeyValue){.value = "1234", .size = 5},
  (KeyMeta[]){
    {.name = "type", .nameSize = 5, .value = {.value = "long", .size = 5}},
    {.name = "length\0min", .nameSize = 11, .value = {.value = "4", .size = 2}},
  });
```

## Decision

**Suggestion:** Minimal Arguments

## Rationale

- The "Minimal Arguments" solution can still be considered minimal, while the more complex solutions are not really minimal anymore.
- For `libelektra-core` "Minimal Arguments" is enough.
  The more complex APIs can be provided as [builder functions](../0_drafts/builder_functions.md) in other libraries.

## Implications

- You need an extra library beyond `libelektra-core` to construct `Key`s and `KeySet`s in a single call.

## Related Decisions

- [Namespace and Name of Keys](../4_decided/keyname.md)
- [Builder Functions for `Key` and `KeySet`](../0_drafts/builder_functions.md)

## Notes
