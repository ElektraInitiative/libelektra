# DESIGN #

This document describes the design of the C-API and provides hints for
binding writers. It is not aimed at plugin writers, since it does not
talk about the implementation details of Elektra.

Elektra follows two design principles:

1. Make it hard to use the API the wrong way, and
2. aim towards an easy to use API for programmers reading and writing
   configuration.

Elektra's data structures are optimized to get, set and lookup values
easily and fast.

The idea is, that the KDB API is not only implemented by Elektra.
Elektra provides a full blown architecture to really support modern
Linux Systems, but comes with some overhead. This document describes
the `KDB` API. It also contains some hints about Elektra-specific
conventions.

## Data Structures ##

The `Key`, `KeySet` and `KDB` data structures are defined in
`kdbprivate.h` to remain ABI compatible when one of them is changed.
This means, it is not possible to put one of Elektra's data structures
on the stack. You must use the memory management facilities mentioned
in the next section.

## Memory Management ##

Elektra manages memory itself. This means, a programmer is not allowed
to use free on data, which was not allocated by himself. This avoids
situation where the programmer forgets to free data, and makes the API
more beginner-friendly. In addition to that, `elektraMalloc` and free
must use the same libc version. `elektraMalloc` in a library linked
against another libc, but freed by the application could lead to hard
to find bugs.

Some calls that create data, have an opposite call that frees this
data. For example after you call:

	KDB * kdbOpen();

you need to use:

	int kdbClose(KDB *handle);

to get rid of the resources again. The second function may also shut
down connections. Therefore it really must be called at the end of a
program.

	Key *keyNew(const char *keyName, ...);
	int keyDel(Key *key);

	KeySet *ksNew(int alloc, ...);
	int ksDel(KeySet *ks);

In the above pairs, the first function uses `elektraMalloc` to reserve
the necessary amount of memory. The second function frees the allocated
data segment. There are more occurrences of `elektraMalloc`, but they
are invisible to the user of the API and happen implicitly within any
of these 3 classes: `KDB`, `Key` and `KeySet`.

Names, values, and comments cannot be handled as easy, because Elektra
does not provide a string library. There are 2 ways to access the
mentioned attributes. We show these methods here, using the comment
attribute as an example. The function

	char *keyString(const Key *key);

just returns a string. Your are not allowed to change the returned string.
The function

	ssize_t keyGetValueSize(const Key *key);

shows how long the string is for the specified key. The returned value
also specifies the minimum buffer size that `keyGetString` will
reserve for the copy of the key. The return value can be directly
passed to `elektraMalloc`.

	ssize_t keyGetString(const Key *key, char *returnedValue, size_t maxSize);

writes the comment in a buffer maintained by you.

## Variable Arguments ##

The constructors for `Key` and `KeySet` take a variable sized list of
arguments. They can be used as an alternatives to the various `keySet*`
methods and `ksAppendKey`. With them you are able to generate any `Key`
or `KeySet` with a single C-statement. This can be done
programmatically by the plugin `c`.

To just retrieve a key, use

	Key *k = keyNew(0);

To obtain a `keyset`, use

	KeySet *k = ksNew(0, KS_END);

The macros `va_start` and `va_end` will not be used then. Alternatively
pass a list as described in the documentation.

## Off-by-one ##

We avoid Off-by-one errors by starting all indizes with 0, as
usual in C. The size returned by the `*GetSize` functions
(`keyGetValueSize`, `keyGetCommentSize` and `keyGetOwnerSize`) is
exactly the size you need to allocate. So if you add 1 to it, too much
space is allocated, but no error will occur.

The same is true for `elektraStrLen` which also already has the
null byte included.

## Minimal Set ##

`kdb.h` contains a minimal set of functions to fully work with a key
database. The functions are implemented in `src/libs/elektra` in ANSI C.

## Value, String or Binary ##

Sometimes people confuse the terms “value”, “string” and “binary”:

- Value is just a name which does not specify if data is stored as
  string or in binary form.

- A string is a char array, with a terminating `'\0'`.

- Binary data is stored in a array of type void, and not terminated by
  `'\0'`.

See also [the glossary](/doc/help/elektra-glossary.md) for further
terminology.

In Elektra `char*` are used as null-terminated strings, while `void*`
might contain `0`-bytes:

	const void *keyValue(const Key *key);

does not specify whether the returned value is binary or a string. The
function just returns the pointer to the value. When `key` is a string
(check with `keyIsString`) at least `""` will be returned. See section
“Return Values” to learn more about common values returned by Elektra's
functions. For binary data a `NULL` pointer is also possible to
distinguish between no data and `'\0'`.

	ssize_t keyGetValueSize(const Key *key);

does not specify whether the key type is binary or string. The function
just returns the size which can be passed to `elektraMalloc` to hold
the entire value.

	ssize_t keyGetString(const Key *key, char *returnedString, size_t maxSize);

stores the string into a buffer maintained by you.

	ssize_t keySetString(Key *key, const char *newString);

sets the null terminated string value for a certain key.

	ssize_t keyGetBinary(const Key *key, void *returnedBinary, size_t maxSize);

retrieves binary data which might contain `'\0'`.

	ssize_t keySetBinary(Key *key, const void *newBinary, size_t dataSize);

sets the binary data which might contain `'\0'`. The length is given
by `dataSize`.

## Return Value ##

Elektra's function share common error codes. Every function must return
`-1` on error, if its return type is integer (like `int`, `ssize_t`). If
the function returns a pointer, `0` (`NULL`) will indicate an error.
This behaviour can't be used for functions that return integers, since
`0` is a valid size and can also be used to represent the boolean value
`false`.

Elektra uses integers for the length of C strings, reference counting, `KeySet` length and internal `KeySet` allocations.

The interface always accepts `size_t` and internally uses `size_t`,
which is able to store larger numbers than `ssize_t`.

The real size of C strings and buffers is limited to `SSIZE_MAX` which
must be checked in every function. When a string exceeds that limit
`-1` or a `NULL` pointer (see above) must be returned.

The following functions return an internal string:

	const char *keyName(const Key *key);
	const char *keyBaseName(const Key *key);
	const char *keyOwner(const Key *key);
	const char *keyComment(const Key *key);

and in the case that `keyIsBinary(key)==0`:

	const void *keyValue(const Key *key);

does so too. If in any of the functions above `key` is a `NULL`
pointer, then they also return `NULL`.

If there is no string you will get back `""`, that is a pointer to the
value `'\0'`. The function to determine the size will return `1` in
that case. That means that an empty string – nothing except the NULL
terminator – has size `1`.

This is not true for `keyValue` in the case of binary data, because the
value `'\0'` in the first byte is perfectly legal binary data.
`keyGetValueSize` may also return `0` for that reason.

## Error Handling ##

Elektra does not set `errno`. If a function you call sets `errno`, make
sure to set it back to the old value again.

Additional information about error handling is available
[here](/doc/help/elektra-error-handling.md).

## Naming ##

All function names begin with their class name, e.g. `kdb`, `ks` or
`key`. We use capital letters to separate single words (CamelCase).
This leads to short names, but might be not as readable as separating
names by other means.

*Get* and *Set* are used for getters/and setters. We use *Is* to ask
about a flag or state and *Needs* to ask about state related to
databases. For allocation/deallocation we use C++ styled names (e.g
`*New`, `*Del`).

Macros and Enums are written in capital letters. Options start with
`KDB_O`, errors with `KDB_ERR`, namespaces with `KEY_NS` and key types
with `KEY_TYPE`.

Data structures start with a capital letter for every part of the word:

	KDB ... Key Data Base Handle
	KeySet ... Key Set
	Key ... Key

We use singular for all names.

Function names not belonging to one of the three classes are Elektra
specific. They use the prefix `elektra*`. They will always be Elektra
specific and won't be implemented by other KDB implementations.

## const ##

Wherever possible functions should use the keyword `const` for
parameters. The API uses this keyword for parameters, to show that a
function does not modify a `Key` or a `KeySet`. We do not use `const`
for return values, except for the following functions:

	const char *keyName(const Key *key);
	const char *keyBaseName(const Key *key);
	const char *keyComment(const Key *key);
	const void *keyValue(const Key *key);
	const char *keyString(const Key *key);
	const Key  *keyGetMeta(const Key *key, const char* metaName)

The reason behind this is, that the above functions – as their name
suggest – only retrieve values. The returned value must not be modified
directly.
