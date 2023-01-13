# Use cases for `libelektra-core`

This folder contains the use cases for `libelektra-core`.

`libelektra-core` primarily implements an ordered, hierarchical associative array data structure, called `KeySet`, which:

1.  uses arbitrary byte sequences, grouped into namespaces, as keys
2.  associates each key with
    1. values: an arbitrary byte sequence
    2. metadata: another ordered, hierarchical associative array, which only associates keys with values
3.  orders keys first by namespace then lexicographically with respect to hierarchy
4.  supports the operations: insert, remove, lookup, hierarchy lookup (a form of prefix lookup) and access by index (which enables iteration)

Additionally, `libelektra-core` provides a data structure, called `Key`, that represents a single key-value pair in the associative array, but can also be used standalone.

To support the hierarchical and ordered nature of a `KeySet` there are two fundamental comparison operations that can be performed on two `Key`s:

1. Order Comparison:
   Establishes the linear order of `Key`s and thereby defines the iteration order of a `KeySet`.
2. Hierarchy Comparison:
   Establishes the hierarchy of `Key`s, by defining based on their names whether one `Key` is a descendant of another.

The individual use cases provide details on these data structures and the operations it supports.

## Why index based?

The use cases are written in an "index based" fashion.
Most use cases for `KeySet` deal only with indices.
Only a select few use cases, are about turning these indices into `Key`s (or new `KeySet`s).

The reason behind this is the flexibility it gives the caller.
