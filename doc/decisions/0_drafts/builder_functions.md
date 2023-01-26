# Builder Functions for `Key` and `KeySet`

## Problem

The structs for `Key` and `KeySet` are opaque, i.e., only the `typedef`s are part of the public headers, the actual `struct` definitions are in a private header.
Because of that, `libelektra-core` must provide a way to construct a new `Key` or `KeySet`.

Both `Key` and `KeySet` are rather complex structures consisting of multiple parts (name, value, metadata, and collection of `Key`s respectively), and `libelektra-core` is supposed to be minimal.
It therefore makes sense to provide additional functions outside `libelektra-core` that make building `Key`s and `KeySet`s easier.

We call these functions "builder functions" compared to the ["constructor functions"](../0_drafts/constructor_functions.md) in `libelektra-core`.

## Constraints

- If the builder functions are in a non-language specific library, they must be directly callable from all languages for which Elektra provides bindings.
- In accordance with the ["Namespace and Name of Keys" decision](../3_decided/keyname.md), the builder functions must not use the escaped name.
  Only the unescaped name may be used, but the namespace may be passed as a separate parameter, if this has benefits.
  Additionally, it might make sense to take the parts of a keyname as separate arguments.

## Assumptions

## Considered Alternatives

### Language Agnostic

> **Note**: This is basically, the "Full Arguments" solution from the [constructor functions decision](../0_drafts/constructor_functions.md).

```c
Key * keyBuild (const KeyName * name, const KeyValue * value, const KeyMeta metadata[]);

// allocates space for at least `alloc` keys
// inserts all keys from `keys` until it finds a NULL pointer, `keys` must contain at least one NULL
KeySet * ksBuild (size_t alloc, const Key * keys[]);
```

This could be called as:

```c
int a = 7;

ksBuild(
  3,
  (Key*[]) {
    keyBuild (
      &(KeyName){.ns = ELEKTRA_NS_SYSTEM, .name = "foo\0bar", .size = 8 },
      &(KeyValue){.value = "1234", .size = 5},
      (KeyMeta[]){
        {.name = "type", .nameSize = 5, .value = {.value = "long", .size = 5}},
        {.name = "length\0min", .nameSize = 11, .value = {.value = "4", .size = 2}},
      }
    ),
    keyBuild (
      &(KeyName){.ns = ELEKTRA_NS_SYSTEM, .name = "foo", .size = 4 },
      &(KeyValue){.value = &a, .size = sizeof(a)},
      NULL
    ),
    keyBuild (&(KeyName){.ns = ELEKTRA_NS_SYSTEM, .name = "baz", .size = 4 }, NULL, NULL)
    NULL
  }
);
```

One neat benefit of this solution is that these simple functions can be called from any language.

### Variadic Arguments

```c
Key * keyNew (ElektraNamespace ns, ...);
```

This could be called as:

```c
// system:/foo/bar with value 1234 and metadata: meta:/type=long, meta:/length/min=4
keyNew (ELEKTRA_NS_SYSTEM, "foo", "bar", NULL, KEY_VALUE, 5, "1234", KEY_META, "type", NULL, "long", KEY_META, "length", "min", NULL, 4, NULL);
// system:/foo/bar with value 1234 and no metadata
keyNew (ELEKTRA_NS_SYSTEM, "foo", "bar", NULL, KEY_VALUE, 5, "1234", NULL);
// system:/foo/bar with no value and no metadata
keyNew (ELEKTRA_NS_SYSTEM, "foo", "bar", NULL, NULL);
```

Compared to the language agnostic version, this loose type safety and doesn't really provide any advantages.
Separating the keyname parts into separate arguments could also be done in the language agnostic version, by using an array `const char * parts[]` inside the `KeyName` struct.

### Macros and Bindings

Some macros could make the language agnostic version above more ergonomic in C.
Bindings in other languages, could also provide wrappers around the `keyBuild` and `ksBuild` functions.
This could for example, take language-native array types as arguments to avoid passing sizes as separate arguments.

In C this could look like this:

```c
#define KS_BUILD(...) (ksBuild (0, (Key *[]){__VA_ARGS__, NULL}))
#define KEY_NAME(ns_, name_) (KeyName){.ns = (ns_), .name = (name_), .size = sizeof (name_)}

#define KEY_VALUE_STRING(s) (KeyValue){.value = (s), .size = strlen((s))}
#define KEY_VALUE_PTR(v) (KeyValue){.value = &(v), .size = sizeof((v))

#define KEY_META_STRING(name_, value_) (KeyMeta){.name = (name_), .nameSize = sizeof (name_), .value = { .value = (value_), .size = strlen(value_) }}
```

This can be used like so:

```c
int a = 7;

KS_BUILD(
  keyBuild (
    &KEY_NAME (ELEKTRA_NS_SYSTEM, "foo\0bar"),
    &KEY_VALUE_STRING ("1234"),
    (KeyMeta[]){
      KEY_META_STRING("type", "long"),
      KEY_META_STRING("length\0min", "4"),
    }
  ),
  keyBuild (
    &KEY_NAME (ELEKTRA_NS_SYSTEM, "foo"),
     &KEY_VALUE_PTR(a),
    NULL
  ),
  keyBuild (&KEY_NAME (ELEKTRA_NS_SYSTEM, "baz"), NULL, NULL)
);
```

## Decision

## Rationale

## Implications

-
-
-

## Related Decisions

- []()
- []()
- []()

## Notes
