elektra-metadata(7) -- metadata
=================================


**metadata** is data about data.  Up to now, there has been a limited
number of metadata entries suited for `filesys`.  For `filesys` this
was efficient, but it was of limited use for every other backend. This
situation has now changed fundamentally by introducing arbitrary metadata.

In Elektra, *metakeys* represent metadata. They can be stored in a key
database, often within the representation of the `Key`.  Metakey is a
`Key` that is part of the data structure `Key`.  It can be looked up
using a **metaname**. Metanames are unique within a `Key`. Metakeys can
also carry a value, called **metavalue**.  It is possible to iterate
over all metakeys of a `Key`, but it is impossible for a metakey to hold
other metakeys recursively.  The purpose of metakeys next to keys is to
distinguish between configuration and information about settings.


## Rationale

Metadata has different purposes:

-  Traditionally Elektra used metadata to carry file system semantics.
The backend `filesys` stores file metadata (File metadata in POSIX is
returned by `stat()`) in a *struct* with the same name.  It contains a
file type (directory, symbolic link,..)  as well as other metadata like
uid, gid, owner, mode, atime, mtime and ctime.  into the `Key` objects.
This solution, however, only makes sense when each file shelters only one
`Key` object.

-  The metaname `binary` shows if a `Key` object contains binary data.
Otherwise it has a null-terminated C string.

-  An application can set and get a flag in `Key` objects.

-  Comments and owner, together with the items above, were the only
metadata possible before arbitrary metadata was introduced.

-  Further metadata can hold information on how to check and validate keys
using types or regular expressions.  Additional constraints concerning
the validity of values can be convenient.  Maximum length, forbidden
characters and a specified range are examples of further constraints.

-  They can denote when the value has changed or can be informal comments
about the content or the character set being used.

-  They can express the information the user has about the key, for
example, comments in different languages.  Language specific information
can be supported by simply adding a unique language code to the metaname.

-  They can represent information to be used by storage
plugins. Information can be stored as syntactic, semantic or additional
information rather than text in the key database.  This could be ordering
or version information.

-  They can be interpreted by plugins, which is the most important
purpose of metadata.  Nearly all kinds of metadata mentioned above can
belong to this category.

-  Metadata is used to pass error or warning information from plugins to
the application. The application can decide to present it to the user. The
information is uniquely identified by numerical codes.  Metadata can
also embed descriptive text specifying a reason for the error.

-  Applications can remember something about keys in metadata.
Such metadata generalises the application-defined flag.

-  A more advanced idea is to use metadata to generate forms in a
programmatic way. While it is certainly possible to store the necessary
expressive metadata, it is plenty of work to define the semantics needed
to do that.


## Implementation

In this document, we discuss the implementation of metadata.
Metakey is implemented directly in a `Key`:
Every metakey belongs to a key **inseparable**.
Unlike normal key names there is no absolute path for it in the hierarchy,
but a relative one only valid within the key.

The advantage of embedding metadata into a key is that
functions can operate on a key's metadata if
a key is passed as a parameter.
Because of this,
`keyNew()` directly supports adding metadata.
A key with metadata is self-contained.
When the key is passed to a
function, the metadata is always passed with it.
Because of the tight integration into a `Key`,
the metadata does not disturb the user.

A disadvantage of this approach
is that storage plugins are more likely to ignore metadata
because metakeys are distinct from keys and have to be handled separately.
It is not possible to iterate over
all keys and their metadata in a single loop.
Instead only a nested loop provides full iteration over all keys and
metakeys.

The metakey itself is also represented by a `Key`.
So the data structure
`Key` is nested directly into a `Key`.
The reason for this
is to make the concept
easier for the user who already knows how to work with a `Key`.
But even new users need to learn only one interface.
During iteration the metakeys, represented through a `Key` object,
contain both the metaname and the metavalue.
The metaname is shorter than a key name
because the name is unique only in
the `Key` and not for the whole global configuration.

The implementation adds no significant memory overhead per `Key`
if no metadata is used.
For embedded systems it is useful to have keys without metadata.
Special plugins can help for
systems that have very limited memory capacity.
Also for systems with enough memory
we should consider that adding the first metadata to a key
has some additional overhead.
In the current implementation
a new `KeySet` is allocated in this situation.



### Interface

The interface to access metadata consists of the following functions:

Interface of metadata:

	const Key *keyGetMeta(const Key *key, const char* metaName);
	ssize_t    keySetMeta(Key *key, const char* metaName,
		const char *newMetaString);

Inside a `Key`, metadata with a given metaname and a metavalue can be set
using `keySetMeta()` and retrieved using `keyGetMeta()`.
Iteration over metadata is possible with:

Interface for iterating metadata:

	int keyRewindMeta(Key *key);
	const Key *keyNextMeta(Key *key);
	const Key *keyCurrentMeta(const Key *key);

Rewinding and forwarding to the next key works as for the `KeySet`.
Programmers used to Elektra will immediately be familiar with
the interface.
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

	int keyCopyMeta(Key *dest, const Key *source, const char *metaName);
	int keyCopyAllMeta(Key *dest, const Key *source);

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
