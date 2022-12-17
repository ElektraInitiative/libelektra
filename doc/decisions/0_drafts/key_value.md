# `Key` value API

## Problem

A `Key` has a value, that much is clear.
From the ["`Key` value types" decision](../3_decided/key_value_types.md) we know that all values are opaque byte-sequences of a known length.

The current API (at the time of writing), does not match the ["`Key` value types" decision](../3_decided/key_value_types.md) and needs to be redesigned.
The aim of this decision is not to solve a specific problem with the current API, but instead to answer the general question:
How should the API for accessing and modifying the value of a `Key` look?

A simple solution to make the API match the ["`Key` value types" decision](../3_decided/key_value_types.md) would be to just remove the parts that are string-typed and keep the binary parts.
While that is simple, it is in no way clear that this would be a good solution.
Therefore, we need to explore and compare our options.

## Constraints

- All values are opaque byte-sequences because of the ["`Key` value types" decision](../3_decided/key_value_types.md)

## Assumptions

## Considered Alternatives

### Read-only values

A simple way to define an API would be to make everything read-only.
A `Key` can be created with a certain value and that's it.
If you want a different value, create a new `Key`.

That would certainly simplify some parts of the implementation, but it would be major inconvenience for the caller.
Also allocating entirely new `Key`s just to change values, could lead to unnecessary, short-lived heap-allocations.

### Direct access

The next simplest way to define the API, is to give the caller full direct access.

The caller creates a `Key` with a `void *` and a `size_t`.
Afterwards, the caller can access and change them at any time in any way they want to.

Basically, a `Key` would not have ownership of its value.
It would just have a reference to it.

This solution could easily lead to various lifetime problems.
By not owning the value, the `Key` cannot be responsible for freeing it.

This option would also break the current COW implementation and might make COW entirely impossible.

### Buffers provided by caller

To fix the lifetime problems from above, a `Key` should own its value.

That means when a `Key` is created with a `void *` and a `size_t`, `libelektra-core` does a `memcpy` (or similar) to create a copy of the data.
Changing the value works the same way.

When the caller wants access to the value again, they must provide a suitably sized buffer as a `void *` and `libelektra-core` will do a `memcpy` to give a copy of the value to the caller.

This solves all lifetime problems, because `Key` fully owns its value and never even shares a reference with anyone.
However, the `memcpy` on access is totally unnecessary when we just want to read the value.

### Semi-direct access

To avoid the unnecessary `memcpy` from above, the API should be asymmetric.
A `Key` owns its value, but it also provides read-only references to the caller.

So when creating a `Key` or changing its value, `libelektra-core` does a `memcpy`.
But when the caller wants access to the value, `libelektra-core` simply returns a `const` pointer.
The caller can read the value, but cannot change it.
If they want a copy they can modify, the caller must do a `memcpy` themselves.

However, by omitting the `memcpy` for reading values, the value inside a `Key` is no longer immutable.
Take this example:

```c
void keySetValue (Key * key, const void * value, size_t size)
{
    // NOT REAL CODE, but contains something similar to:
    memcpy(key->value, value, size);
}

const void * keyGetValue (const Key * key)
{
    return key->value;
}

typedef struct {
    int a;
    char * b;
} data;

void foo (Key * key) {
    char arr[] = "test";
    data f = {.a = 1, .b = arr };

    keySetValue (key, &f, sizeof(f));

    const data * fPtr = keyGetValue (key);

    // fPtr->a = 7;   // ERROR: assignment of member 'a' in read-only object
    fPtr->b[0] = 'b'; // works fine, b is not const and C doesn't propagte the const from fPtr to members

    return 0;
}
```

This may become a problem, when values are shared between `Key`s via [Copy on Write](../2_in_progress/copy_on_write.md) copies.
The only solution here is to clearly document, that values returned from a `Key` must not be modified in any way.
Modifications must only happen through the public `libelektra-core` API, or it is known that the modification is guaranteed to be safe.

> **Note**: It is likely the best option, to _not_ document any modifications as safe.
> This way no modification is guaranteed to be safe, and any direct modification would be a mistake by the developer using `libelektra-core`.

### COW access

Because of [the COW implementation](../2_in_progress/copy_on_write.md), there exists a struct that holds a `Key`'s value in the form of a `void *` pointer and a `size`.

One possibility would be, to make this struct public as an opaque struct and create an API for that struct instead.
This may seem like a silly idea, since it would just move the problem to a different struct.
But depending on our reference counting implementation, this may make sense.

Because the caller has APIs for both the COW struct and the value itself, it should also be more obvious to them that a value may be shared between `Key`s.
It would also be more logical to create an API that allows the caller to check, whether a COW struct is shared (i.e., check the refcount).

Furthermore, this would alleviate the need for `keyCopy` to create COW copied values:

```c
// this
keyCopy (dest, source, KEY_CP_VALUE);

// would be equivalent to e.g.
keySetValue (dest, keyValue (source));
// because keyValue and keySetValue deal with the COW struct not the value contained in it
```

Again, this may seem like a minor change.
But it would make incorrect usage harder.
Doing `keySetValue (dest, keyValue (source))` is a very obvious idea, but if that doesn't deal with the COW structs, it may create an unnecessary copy of the full value data.

## Decision

## Rationale

## Implications

## Related Decisions

## Notes
