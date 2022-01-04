# DESIGN

This document describes the design of Elektra's C-API and provides hints for
binding writers. It is not aimed at plugin writers, since it does not
talk about the implementation details of Elektra.

Elektra [aims](GOALS.md) to fulfill the following design principles:

1. To make the API future-proof so that it can remain compatible and stable
   over a long period of time,
2. to make it hard to use the API the wrong way by making it simple & robust, and
3. to make the API easy to use for programmers reading and writing
   configuration.

The C-API is suitable to be reimplemented, also in non-C-languages, like Rust.
Elektra provides a full-blown architecture to support configuring systems, and
the C-API is the core of this endeavour.

## Data Structures

The `Key`, `KeySet` and `KDB` data structures are defined in
`kdbprivate.h` to allow ABI compatibility.
This means, it is not possible to put one of Elektra’s data structures
on the stack. You must use the memory management facilities mentioned
in the next section.

## Memory Management

Elektra provides functions that create and free data.
For example after you call:

```c
KDB * kdbOpen();
```

you need to use:

```c
int kdbClose(KDB *handle);
```

to get rid of the resources again. The second function may also shut
down connections. Therefore, it must be called before the end of a program.

```c
Key *keyNew(const char *keyName, ...);
int keyDel(Key *key);

KeySet *ksNew(int alloc, ...);
int ksDel(KeySet *ks);
```

In the above pairs, the first function reserves the necessary amount
of memory. The second function frees the allocated data segment. There
are more allocations happening, but they are invisible to the user of
the API and happen implicitly within any of these 3 classes:
`KDB`, `Key` and `KeySet`.

Key names and values cannot be handled as easy without helper libraries,
because Elektra does not provide a string library. The function

```c
const void *keyValue(const Key *key);
```

returns a value. You are not allowed to change the returned value.
The life time is bound to the `Key`.
The function

```c
ssize_t keyValueSize(const Key *key);
```

gives the length of the value in bytes.

## Variable Arguments

The constructors for `Key` and `KeySet` take a variable sized list of
arguments. They can be used as an alternatives to the various `keySet*`
methods and `ksAppendKey`. With them you are able to generate any `Key`
or `KeySet` with a single C-statement. This can be done
programmatically by the plugin `c`.

To just retrieve a key, use

```c
Key *k = keyNew("/", KEY_END);
```

To obtain a `keyset`, use

```c
KeySet *k = ksNew(0, KS_END);
```

Alternatively pass a list as described in the documentation.
The idea of these variable arguments is, that one function call
can create any `KeySet`. For binding writers `keyVNew` might be
useful.

## Off-by-one

We avoid off-by-one errors by starting all indices with 0, as
usual in C. The size returned by the `*GetSize` functions
(`keyGetValueSize`, `keyGetCommentSize` and `keyGetOwnerSize`) is
exactly the size you need to allocate. So if you add 1 to it, too much
space is allocated, but no error will occur.

The same is true for `elektraStrLen` which also already has the
null byte included.

## Minimal Set

`kdb.h` contains a minimal set of functions to fully work with a key
database. The functions are implemented in `src/libs/elektra` in ANSI C.

Useful extensions are available in [further libraries](/src/libs).

## Return Values

Elektra’s function share common error codes. Every function must return
`-1` on error, if its return type is integer (like `int`, `ssize_t`). If
the function returns a pointer, `0` (`NULL`) will indicate an error.

Elektra uses integers for the length of C strings, reference counting, `KeySet` length and internal `KeySet` allocations.

The interface always accepts `ssize_t` and internally uses `size_t`,
which is able to store larger numbers than `ssize_t`.

The real size of C strings and buffers is limited to `SSIZE_MAX`.
When a string exceeds that limit `-1` or a `NULL` pointer (see above)
must be returned.

The following functions return an internal string:

```c
const char *keyName(const Key *key);
const char *keyBaseName(const Key *key);
```

and in the case that `keyIsBinary(key)==0`:

```c
const void *keyValue(const Key *key);
```

does so, too. If in any of the functions above `key` is a `NULL`
pointer, then they also return `NULL`.

If there is no string you will get back `""`, that is a pointer to the
value `'\0'`. The function to determine the size will return `1` in
that case. That means that an empty string – nothing except the NULL
terminator – has size `1`.

This is not true for `keyValue` in the case of binary data, because the
value `'\0'` in the first byte is perfectly legal binary data.
`keyGetValueSize` may also return `0` for that reason.

## Error Handling

For `KDB` functions the user does not only get the return value but
also a more elaborate error information, including an error message,
in the metadata of the `parentKey` or `errorKey`. Furthermore, it is
also possible to get warnings, even if the calls succeeded.

Using different error categories, the user of the API can have suitable
reactions on specific error situations. Additional information about
error handling is available [here](/doc/dev/error-handling.md).

Elektra does not set `errno`. If a function you call sets `errno`, make
sure to set it back to the old value again.

## Naming

All function names begin with their class name, e.g. `kdb`, `ks` or
`key`. We use capital letters to separate single words (CamelCase).
This leads to short names, but might be not as readable as separating
names by other means.

_Get_ and _Set_ are used for getters/and setters. We use _Is_ to ask
about a flag or state and _Needs_ to ask about state related to
databases. For allocation/deallocation we use C++ styled names (e.g
`*New`, `*Del`).

Macros and Enums are written in capital letters. Flags start with
`KDB_`, namespaces with `KEY_NS_` and macros with `ELEKTRA_`.

Data structures start with a capital letter for every part of the word:

- `KDB` ... Key Data Base Handle
- `KeySet` ... Key Set
- `Key` ... Key

We use singular for all names.

Function names not belonging to one of the three classes use the
prefix `elektra*`.

## const

Wherever possible functions should use the keyword `const` for
parameters. The API uses this keyword for parameters, to show that a
function does not modify a `Key` or a `KeySet`, e.g.:

```c
const char *keyName(const Key *key);
const char *keyBaseName(const Key *key);
const void *keyValue(const Key *key);
const char *keyString(const Key *key);
const Key  *keyGetMeta(const Key *key, const char* metaName);
```

The reason behind this is, that the above functions – as their name
suggest – only retrieve values. The returned value must not be modified
directly.

## Design Guidelines Checklist

On potential changes of the API/ABI as detected by the
[`build server`](https://build.libelektra.org/job/libelektra/job/master/),
please make sure the API has been reviewed according to the
following 2 checklists:

## Checklist for overall API

### Consistency

- [ ] Consistent naming schemes for enums, macros, typedefs and functions
- [ ] Same things are named the same and included in [Glossary](/doc/help/elektra-glossary.md)
- [ ] Different things are named differently
- [ ] The order of arguments should be consistent across similar functions

### Structural Clarity

- [ ] Functions with similar functionality have the same prefix

### Compatibility

- [ ] All bindings have been updated to reflect the new API and work properly

### Extensibility

- [ ] New API is easily extensible with additional functionality
- [ ] Components only depend on each other if needed

## Checklist for each function

There are several checklists for functions, depending on the language in which
the function is written:

- [C](/scripts/api_review/template.c.md)
- [Rust](/scripts/api_review/template.rs.md)
- [Java](/scripts/api_review/template.java.md)
