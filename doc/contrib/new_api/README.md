# PREVIEW: New `libelektra-core` API 2023

> **Warning**: The files in this folder are intended to be only temporary.
>
> **DO NOT LINK TO THESE FILES FROM THE OUTSIDE.**

## Core API

### Note on names

From the [use cases](/doc/usecases/core/README.md) we know that there is a collection structure which contains elements with a name and value (and metadata).

The current API uses these names for those concepts:

- Collection: `KeySet`
- Element: `Key`
- Name of element: `char *`
- Value of element: —

The API proposed below uses (to keep as much as possible from above):

- Collection: `ElektraKeyset`
- Element: `ElektraKey`
- Name of element: `ElektraKeyname`
- Value of element: `ElektraKeyvalue`

Another (maybe less confusing) option would be:

- Collection: `ElektraSet` or `ElektraMap`
- Element: `ElektraEntry`
- Name of element: `ElektraKey` or `ElektraName`
- Value of element: `ElektraValue`

### Note on public structs

The public structs `ElektraKeyname` and `ElektraKeyvalue` defined below are not problematic or limiting in terms of forward compatibility.
This is because, they essentially define what a "keyname" and a "key value" are for Elektra.
That is why they are used both as part of the public API **and** in the definitions of the `*Cow` structs, which define how stuff is actually stored.
Any change to these structs would mean changing that definition and therefore would be major breaking change, even if the structs where not public.

### Public Headers

See [`core/public.h`](core/public.h) (may be split into multiple files in actual code)

### Private Headers

See [`core/private.h`](core/private.h) (may be split into multiple files in actual code)

### Usage Examples

See code in [`core/examples.c`](core/examples.c).

> **Note**: The examples compile with `gcc -c examples.c -Wall -pedantic`, but they do not link, since there is no implementation of the `elektra*` functions yet.

For completeness, here are some more example of unsafe and illegal operations.
These operations compile and "work" without reporting errors at runtime, but will cause various problems.
An "illegal" operation, immediately break something and will cause wrong results, segfaults, etc. down the line.
An "unsafe" operation, does not immediately break anything, but it is still an incorrect use of the API and often creates new "unsafe" or even "illegal" operations for the future.

```c
ElektraKey * k1 = elektraKeyNew (&(ElektraKeyname){
    .ns = ELEKTRA_NS_SYSTEM,
    .name = "foo\0bar\0baz",
    .size = 14
});
ElektraKeyset * ks1 = elektraKeysetNew (8);
elektraKeyInsertAndRelease (ks1, k1);

ElektraKey * i1 = elektraKeysetGet (ks1, 0);

// WARNING USAFE OPERATIONS
elektraKeyUnlockName (i1); // name can now be modified even though k1 is still part of ks1

// WARNING ILLEGAL OPERATIONS

// because of the elektraKeyUnlockName, this is now an ILLEGAL operation, we are changing the name of a key that is part of a keyset
// without elektraKeyUnlockName it would be safe, and elektraKeySetName would return an error
elektraKeySetName (i1, name2);


// having too many elektraKeyRelease is also an IILEGAL operation
// here we've already given up our references to these keys, so calling elektraKeyRelease again means we're releasing somebody else's reference
// in this case this will even lead to the memory of k1 being freed and ks1 containing invalid pointers, because we're releasing the reference to k1 that is held by ks1
elektraKeyRelease (k1);
```

## API outside `libelektra-core`

Where exactly these APIs will live is not yet determined, but it will not be in `libelektra-core`.

### Names

See [`names/public.h`](names/public.h)

#### Usage Examples

See [`names/examples.c`](names/examples.c)

> **Note**: The examples compile with `gcc -c examples.c -Wall -pedantic`, but they do not link, since there is no implementation of the `elektra*` functions yet.

### Cut

See [`cut/public.h`](cut/public.h).

> **Note:** The header contains the definitions as well, but in actual code these would be in separate `.c` files.

### Builders

See [`builders/public.h`](builders/public.h).

> **Note:** The header contains the definitions as well, but in actual code these would be in separate `.c` files.

#### Usage Examples

See [`builders/examples.c`](builders/examples.c).

> **Note**: The examples compile with `gcc -c examples.c -Wall -pedantic`, but they do not link, since there is no implementation of the `elektra*` functions yet.
