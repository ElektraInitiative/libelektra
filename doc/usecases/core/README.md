# Use cases for `libelektra-core`

This folder contains the use cases for `libelektra-core`.

`libelektra-core` primarily implements an ordered, hierarchical associative array data structure, called `KeySet`, which:

1.  uses arbitrary byte sequences, grouped into namespaces, as keys
2.  associates each key with
    1. values: an arbitrary byte sequence
    2. metadata: another ordered, hierarchical associative array, which only associates keys with values
3.  orders keys first by namespace then lexicographically
4.  supports the operations: insert, lookup, prefix lookup, access by index and delete

Additionally, `libelektra-core` provides a data structure, called `Key`, that represents a single key-value pair in the associative array, but can also be used standalone.

The individual use cases provide details on these data structures and the operations it supports.
