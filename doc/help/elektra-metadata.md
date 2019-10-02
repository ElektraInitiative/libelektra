# elektra-metadata(7) -- metadata

**Metadata** is data about data. In earlier versions of Elektra, there has been a limited
number of metadata entries suited for `filesys`. For `filesys` this
was efficient, but it was of limited use for every other backend. This
situation has now changed fundamentally by introducing arbitrary metadata.

## Rationale

Metadata has different purposes:

- Traditionally Elektra used metadata to carry file system semantics.
  The backend `filesys` stores file metadata (File metadata in POSIX is
  returned by `stat()`) in a _struct_ with the same name. It contains a
  file type (directory, symbolic link,..) as well as other metadata like
  uid, gid, owner, mode, atime, mtime and ctime. into the `Key` objects.
  This solution, however, only makes sense when each file shelters only one
  `Key` object.

- The metaname `binary` shows if a `Key` object contains binary data.
  Otherwise it has a null-terminated C string.

- An application can set and get a flag in `Key` objects.

- Comments and owner, together with the items above, were the only
  metadata possible before arbitrary metadata was introduced.

- Further metadata can hold information on how to check and validate keys
  using types or regular expressions. Additional constraints concerning
  the validity of values can be convenient. Maximum length, forbidden
  characters and a specified range are examples of further constraints.

- They can denote when the value has changed or can be informal comments
  about the content or the character set being used.

- They can express the information the user has about the key, for
  example, comments in different languages. Language specific information
  can be supported by simply adding a unique language code to the metaname.

- They can represent information to be used by storage
  plugins. Information can be stored as syntactic, semantic or additional
  information rather than text in the key database. This could be ordering
  or version information.

- They can be interpreted by plugins, which is the most important
  purpose of metadata. Nearly all kinds of metadata mentioned above can
  belong to this category.

- Metadata is used to pass error or warning information from plugins to
  the application. The application can decide to present it to the user. The
  information is uniquely identified by numerical codes. Metadata can
  also embed descriptive text specifying a reason for the error.

- Applications can remember something about keys in metadata.
  Such metadata generalizes the application-defined flag.

- A more advanced idea is to use metadata to generate forms in a
  programmatic way. While it is certainly possible to store the necessary
  expressive metadata, it is plenty of work to define the semantics needed
  to do that.

## Usage

Every key-value pair can have an arbitrary number of metakeys
with metavalues attached. Identical to keys, metakeys are
unique, but only within its key they are attached to.

To create a metakey, use [kdb-meta-set(1)](kdb-meta-set.md),
to get metadata [kdb-meta-get(1)](kdb-meta-get.md).

The preferred way to use metadata is to set all metadata
in the `spec` namespace and let the `spec` plugin copy
the metadata to all other namespaces.
