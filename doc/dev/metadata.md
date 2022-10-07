# Metadata

Make sure to first read [elektra-metadata(7)](/doc/help/elektra-metadata.md)
to familiarize yourself with the _metadata_ concept.

In Elektra, _metakeys_ represent metadata. They can be stored in a key
database, often within the representation of the `Key`. Metakey is a
`Key` that is part of the data structure `Key`. It can be looked up
using a **metaname**. Metanames are unique within a `Key`. Metakeys can
also carry a value, called **metavalue**. It is possible to iterate
over all metakeys of a `Key`, but it is impossible for a metakey to hold
other metakeys recursively. The purpose of metakeys next to keys is to
distinguish between configuration and information about settings.

## Implementation

In this document, we discuss the implementation of metadata. Metakey
is implemented directly in a `Key`: Every metakey belongs to a key
**inseparable**. Unlike normal key names there is no absolute path for
it in the hierarchy, but a relative one only valid within the key.

The advantage of embedding metadata into a key is that functions
can operate on a key's metadata if a key is passed as a parameter.
Because of this, `keyNew()` directly supports adding metadata. A key
with metadata is self-contained. When the key is passed to a function,
the metadata is always passed with it. Because of the tight integration
into a `Key`, the metadata does not disturb the user.

A disadvantage of this approach is that storage plugins are more likely
to ignore metadata because metakeys are distinct from keys and have to
be handled separately. It is not possible to iterate over all keys and
their metadata in a single loop. Instead only a nested loop provides
full iteration over all keys and metakeys.

The metakey itself is also represented by a `Key`. So the data structure
`Key` is nested directly into a `Key`. The reason for this is to
make the concept easier for the user who already knows how to work
with a `Key`. But even new users need to learn only one interface.
During iteration the metakeys, represented through a `Key` object,
contain both the metaname and the metavalue. The metaname is shorter
than a key name because the name is unique only in the `Key` and not
for the whole global configuration.

The implementation adds no significant memory overhead per `Key` if
no metadata is used. For embedded systems it is useful to have keys
without metadata. Special plugins can help for systems that have very
limited memory capacity. Also for systems with enough memory we should
consider that adding the first metadata to a key has some additional
overhead. In the current implementation a new `KeySet` is allocated in
this situation.

### Interface

The interface to access metadata consists of the following functions:

Interface of metadata:

```c
const Key * keyGetMeta (const Key * key, const char * metaName);
ssize_t keySetMeta (Key * key, const char * metaName, const char *newMetaString);
KeySet * keyMeta (Key * key);
```

Inside a `Key`, metadata with a given metaname and a metavalue can be set
using `keySetMeta()` and retrieved using `keyGetMeta()`.

With `keyMeta()` you can get a `KeySet` with all metakeys of a given `Key`.
It's possible to iterate the returned metadata `KeySet` like any other `KeySet`.

Example for iterating metadata:

```c
void printMetaData (Key * key)
{
    Key * curMeta;
    KeySet metaKeys = keyMeta (key);
    ssize_t ksSize = ksGetSize (metaKeys);

    for (elektraCursor it = 0; it < ksSize; ++it)
    {
        curMeta = ksAtCursor (metaKeys, it);
        printf ("metaname: %s, metavalue: %s\n", keyName (curMeta), keyString (curMeta));
    }
}
```

Developers used to Elektra will immediately be familiar with the interface.
Tiny wrapper functions still support the old metadata interface.

### Sharing of Metakey

Usually substantial amounts of metadata are shared between keys.
For example, many keys have the type `int`.
To avoid the problem that every key with this
metadata occupies additional space, `keyCopyMeta()` was invented.
It copies metadata from one key to another.
Only one metakey resides in memory
as long as the metadata is not changed with `keySetMeta()`.
To copy metadata, the following functions can be used:

```c
int keyCopyMeta(Key *dest, const Key *source, const char *metaName);
int keyCopyAllMeta(Key *dest, const Key *source);
```

The name `copy` is used because the information is copied from one key to
another. It has the same meaning as in `ksCopy()`.
In both cases it is a flat copy.
`keyCopyAllMeta()` copies all metadata from one key to another.
It is more efficient than a loop with the same effect.

`keyDup()` copies all metadata as expected.
Sharing metadata makes no difference from the user's point of view.
Whenever a metavalue is changed a new metakey is generated.
It does not matter if the old metakey was shared or not.
This is the reason why a const pointer is always passed back.
The metakey must not be changed because it can be used within another
key.

## See also

- [all available metadata](/doc/METADATA.ini) in Elektra (defines SpecElektra)
- [glossary](/doc/help/elektra-glossary.md)
