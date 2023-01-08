# Metakey semantics

## Problem

Metakeys (i.e., keys with namespace `KEY_NS_META`) have special requirements beyond what other namespaces want.
This is because metakeys are used for specifications.

These extra requirements are:

1. Copying metadata from `spec:/` keys to other namespaces must share as much memory as possible.
   Without sharing memory there would be massive unnecessary duplication of data.
2. It must be possible to identify whether a metakey was copied from `spec:/` key, or was set directly.
   Additionally, it must be possible to detect if the value of a copied metakey has changed.
3. Changing the value of a copied metakey must not affect the original `spec:/` key (and its metadata).

Unrelated to the specification use case (see also ["Validating Configuration with Specification"](../../usecases/kdb/UC_validate_config.md)), metakeys must also be prevented from having metadata of their own.
As `keyMeta` now "leaks" (compared with the previous API) the `KeySet` of metadata is unprotected and the requirements above are not fulfilled anymore.
For example, changes of metadata values could confuse the spec plugin and lead to invalid configuration passed to applications.

## Constraints

- The API should be hard to misuse.
- Elektra should protect against incorrect operations, that would lead to undefined behavior.
- As a specialized hook plugin `spec` could make use of specialized APIs.
  However, the need for such internal APIs should be limited as much as possible.

## Assumptions

- Not allowing metadata on metakeys, can also be ignored here, because it can be dealt with in `keyNew()` and `keyMeta()`.
  `keyMeta()` can just return `NULL` and `keyNew()` can either ignore metadata arguments or fail if metadata is given.

## Considered Alternatives

### Completely Read-only

One option to enforce all requirements is to just make any `Key` that is created with the `KEY_NS_META` namespace entirely read-only after `keyNew()`.

Because the `Key` is entirely read-only, it's value cannot change.
Therefore, we can find out, if it was copied from a given `spec:/` key by doing a pointer comparison:

```c
KeySet * specMeta = keyMeta (specKey);
KeySet * otherMeta = keyMeta (otherMeta);

for (elektraCursor it = 0; it < ksGetSize (otherMeta); it++)
{
   Key * metaKey = ksAtCursor (otherMeta, it);
   if (metaKey == ksLookup (specMeta, metaKey, 0))
   {
      // copied from spec
   }
}
```

To update metadata you would have to create a new `Key` to replace the existing one:

```c
ksAppendKey (keyMeta (key), keyNew ("meta:/type", KEY_VALUE, "string", KEY_END));

// would always fail, because metakeys are read-only
keySetString (ksLookupByName (keyMeta (key), "meta:/type", 0), "string");
```

Safely copying metadata would be very simple with this solution.

```c
ksAppend (keyMeta (dest), keyMeta (source))
```

This copies metadata while sharing the memory for the individual `Key`s.
Because the `Key`s are read-only they cannot be changed at all, so we don't have to worry about changes to `dest` affecting `source`.
If we want to change the metadata of `dest` we have to create a new `Key`, which will not be used by `source`.

### Read-only while in `KeySet`

A relaxation of the above solution would be to only make the `Key` read-only, while it is part of a `KeySet`.

The snippets from above would still work and changing the value of metakey directly as above would still fail.
But now you don't have to create the `Key` directly with the right name and value.

```c
Key * metaKey = keyNew ("meta:/", KEY_END);
keyAddBaseName (metaKey, somePart);
keySetString (metaKey, someValue);
```

The snippet above would not work, if the key is returned as read-only directly from `keyNew()`.

Copying metadata works the same as above.
Since the `Key`s are still read-only as long as `source` uses them, we again cannot affect `source` by changing `dest`.

### Utilize COW implementation

An issue with the solutions above is that you always have to create a new `Key` to change metadata.
This means we need to allocate all the memory for a new metakey.
This new metakey will replace the existing one in `keyMeta(key)`.
But that means, if the existing metakey was not shared with other keys, it will be deleted, when we could have just reused that memory.

One option to solve that issue, in "Read-only while in `KeySet`" is something like this:

```c
Key * metaKey = ksLookupByName (keyMeta (key), "meta:/type", KDB_O_POP);
if (isInKeySet (metaKey)) // <- hypothetical API
{
   // still used, can't modify -> use new key
   keyDel (metaKey);
   ksAppendKey (keyMeta (key), keyNew ("meta:/type", KEY_VALUE, "string", KEY_END));
}
else
{
   keySetString (metaKey);
   ksAppendKey (keyMeta (key), metaKey);
}
```

> **Note**: Where the code above would live doesn't matter.
> It may be part of some Elektra library, or it may be user code.
> The code is clearly not ideal and that's why the solution here actually is to avoid the need for a `isInKeySet` function entirely.

However, that still has some obvious issues:

- It needs a public API `isInKeySet` to check whether a metakey is used by some metadata `KeySet`.
- We still need to take the metakey out of the metadata `KeySet` and reinsert it.
  That means shuffling around the array in the `KeySet`, which may be worse than the duplicate memory for the new metakey.

Clearly this is **not** a viable solution to the problem.
But the good thing is we can solve these issues, because of the [COW implementation](../1_in_discussion/copy_on_write.md).

To solve the issues mentioned above, `spec` needs to do a few things differently:

#### Copying Metakeys in `spec`

Instead of copying metadata with

```c
ksAppend (keyMeta (dest), keyMeta (source))
```

we need to make a copy of all metakeys

```c
KeySet * sourceMeta = keyMeta (source);
KeySet * destMeta = keyMeta (dest);

for (elektraCursor i = 0; i < ksGetSize (sourceMeta); i++)
{
   ksAppendKey (destMeta, keyDup (ksAtCursor (sourceMeta, i), KEY_CP_ALL));
}
```

This seems like a bad change, but because of the COW implementation it's not as bad as it looks.
Only the `struct Key` will be duplicated, both the name and value data of the metakeys will still be shared between `source` and `dest`.
Still worse than reusing everything and just adding a few new pointers, but not too bad.

This change means that modifying metadata in `dest` cannot possibly affect `source`, because they do not share any `Key *`s.
At first, they do share all the name and value data, but through COW that stops as soon as either `Key` is modified.
Therefore, we don't need to make the value read-only and anybody (including user code) can just do:

```c
keySetString (ksLookupByName (keyMeta (key), "meta:/type", 0), "string");
```

#### Detecting & Removing Copied Metakeys in `spec`

The issue with not sharing `Key *`s of course is that a pointer comparison will no longer detect copied metakeys.
That means we need a new way of detecting what `spec` needs to remove.

First we'd add a new `keyGetCOWValue` function:

```c
KeyData * keyGetCOWValue (Key * key)
{
   return key->keyData;
}
```

> **Note**: This function wouldn't be part of `libelektra-core`.
> It would be in `libelektra-extra` or some other library on which `spec` would depend.

With this function, the check to detect copied metakeys becomes this straightforward snippet:

```c
KeySet * specMeta = keyMeta (specKey);
KeySet * otherMeta = keyMeta (otherMeta);

for (elektraCursor it = 0; it < ksGetSize (otherMeta); it++)
{
   Key * otherMetaKey = ksAtCursor (otherMeta, it);
   if (keyGetCOWValue (otherMetaKey) == keyGetCOWValue(ksLookup (specMeta, metaKey, 0)))
   {
      // copied from spec
   }
}
```

The check above works, because changing the value of a `Key` will allocate a new `key->keyData`, if it is shared with another key.
Therefore, the `keyData` pointers will only be the same if the value was not modified and was copied from the `spec:/` key.

> **Note**: Adding `keyGetCOWValue` (anywhere) might cause problem with some of the current guarantees related to COW.
> A quick solution for this would be to instead only provide `keyHasSameCOWValue`:
>
> ```c
> bool keyHasSameCOWValue (Key * key, Key * other)
> {
>   if (key == NULL && other == NULL) return true;
>   if (key == NULL || other == NULL) return false;
>   return key->keyData == other->keyData;
> }
> ```
>
> This would still allow making the comparison needed by `spec`, but doesn't actually provide any access to `key->keyData`, so nothing no guarantees can be broken.

## Decision

## Rationale

## Implications

## Related Decisions

- [Copy On Write](../1_in_discussion/copy_on_write.md)

## Notes
