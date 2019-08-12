# Data Structures

For an introduction, please
[read first about elektra classes](classes.md).
You might want to read
[about architecture first](architecture.md).

## Introduction

Data structures define the common layer in Elektra. They are used to
transfer configuration between Elektra and applications, but also
between plugins.

### ADT

Both the `KeySet` and the interface to metadata within a `Key` are
actually **ADT** (abstract data types).
The API is designed so that different implementations of the
data structures can be used internally.

A **hash** data structure
presents a good candidate as alternative data structure, especially for
the metadata interface.
It is believed to be much faster on
lookup, but considerably slower on sorted enumeration.

An **AVL tree** also serves as a competitor.
AVL trees are expected to be
faster for inserting keys at any place, but may be slower for appending
because of the needed reorganizations.
Their disadvantage is that they need to allocate
a large number of small pieces of memory.
Further investigations, namely implementation and benchmarks,
are required to decide.

A **trie** could also be used for lookup of key names.
It performs well for lookup, but needs more memory allocations.

Currently the `KeySet` is implemented as a sorted array.
It is fast on appending and iterating, and has nearly no size-overhead.
To improve the lookup-time, an additional **hash** will be used.

### ABI compatibility

Application binary interface, or ABI, is the interface to all data
structures of an application or library directly allocated or accessed
by the user.

Special care has been taken in Elektra to support all changes within the
data structures without any ABI changes. ABI changes would entail the
recompilation of applications and plugins using Elektra. The functions
`keyNew()`, `ksNew()` and `kdbOpen()` allocate the data structures for the
applications. The user only gets pointers to them. It is not possible
for the user to allocate or access these data structures directly when
only using the public header file `<elektra/kdb.h>`. The functions `keyDel()`,
`ksDel()` and `kdbClose()` free the resources after use. Using the C++
binding deallocation is done automatically.

## Meta Data

Read [here](metadata.md).

## KeySet

This subsection describes what has changed between 0.7 and 0.8 and deals with
some general implementation issues.

### Operations

`KeySet` resembles the classical mathematical
set. Operations like union, intersection or difference are well
defined. In mathematics typically every operation
yields a new set.
Instead, we try to reuse sets in the following ways:

- A completely new and independent `KeySet` as return value would
  resemble the mathematical ideal closely. This operation would be
  expensive. Every `Key` needs to be duplicated and
  inserted into a new `KeySet`.

  Such a **deep duplication** was only needed in `kdbSet()`.

- The resulting
  `KeySet` is created during the operation, but only a flat copy is
  made. This means that the keys in it are actually not duplicated, but only
  their reference counter is increased.
  This method is similar to the mathematical model.
  Compared with a deep copy it can achieve good performance.
  But all changes to the values of keys in the resulting `KeySet`
  affect the original `KeySet`, too.

  `ksDup(const KeySet *source)` produces a new `KeySet`. That way the
  `source` is not changed as shown by the `const` modifier.

- The result of the operation is applied to the
  `KeySet` passed as argument directly.
  This is actually quite common, but for this situation
  other names of the operations are more suitable.

  For example, a union
  which changes the `KeySet` is called `ksAppend()`.

- A new `KeySet` is created, but the `KeySet` passed as
  parameter is reduced by the keys needed for the new `KeySet`. This
  is useful in situations where many operations have to be applied in
  a sequence reducing the given `KeySet` until no
  more keys are left.
  None of the reference pointers change in this situation.

  `ksCut(KeySet *ks, const Key *cutpoint)` works that way.
  All keys below the `cutpoint` are moved from `ks` to the returned key
  set.

### Consistency

There are
several ways to define consistency relations on key sets.
For **strict consistency**
every parent key must exist before the user
can append a key to a key set.
For example, the key set with the keys

```
system
system/elektra
system/elektra/mountpoints
```

would allow the
key `system/elektra/mountpoints/tcl` to be added,
but not the key
`system/apps/abc` because `system/apps` is missing.
File systems enforce this kind of consistency.

These semantics are however not useful for configurations.
Especially for
user configurations often only some keys need to be overwritten.
It is not a good idea to copy all parent keys to the users configuration.
For this reason we use a less strict definition of consistency supporting
such holes.

We also evaluated a form of **weak consistency**.
It avoids adding some unnecessary keys.
A constraint is that a key can be added only if it has a parent
key.
But the constraint does not apply if
no other key exists above the key about to be inserted.
From that moment it will serve as parent key for other keys.
With the current implementation of `KeySet`, however,
it is not possible to decide this constraint in constant time.
Instead its worst-case complexity would
be $log(n) * x$ where $n$ is the number
of keys currently in the key set and $x$ is the _depth_ of the
key. The depth is the number of `/` in the key name.
The worst-case of the complexity applies when the inserting works
without a parent key.
For example, with the keys

```
user/sw/apps/abc/current/bindings
user/sw/apps/abc/current/bindings/key1
user/sw/apps/abc/current/bindings/key2
```

the weak consistency would allow inserting
`user/sw/apps/abc/current/bindings/key3`
because it is directly below an existing key.
It would also allow adding
`user/sw/apps/xyz/current`
because it does not have any parent key.
But it would not allow
`user/sw/apps/abc/current/bindings/dir/key1`
to add.
The worst-case complexity was
found to be too expensive, and hence `KeySet` has
**no consistency** check at all.

This means any key with a valid key name
can be inserted into `KeySet`.
The `KeySet` is changed so that
it is now impossible to append keys without a name.
`ksAppendKey(ks, Key *toAppend)` takes ownership of the key
`toAppend` and will delete the key in that case.
The caller does not have to free `toAppend`: either it is in the key set
or it is deleted.

Binary search determines
the position where to insert a key.
The C version of binary search `bsearch()` cannot tell where to insert
a key when it is not found.
So the algorithm has to be reimplemented.
Java's binary search `binarySearch()` uses a trick to both indicate
where a key is found and where to insert it with the same return code
by returning the negative value `((-insertion point) - 1)`
indicating where the new value should be inserted when the key
is not found.
Elektra now also uses this trick internally.

### Internal Cursor

`KeySet` supports an
**external iterator**
with the two functions
`ksRewind()` to go to the beginning and `ksNext()` to
advance the _internal cursor_ to the next key.
This side effect is used to indicate a position for
operations on a `KeySet` without any additional parameter.
This technique is comfortable to see which
key has caused an error after an unsuccessful key database operation.

Elektra only has some functions to change the cursor of a key set.
But these allow the user to compose powerful functions.
Plugins do that extensively as we will see later
in `ksLookupRE()`.
The user can additionally write more such functions for
his or her own purposes.
To change the internal cursor, it is
sufficient to iterate
over the `KeySet` and stop at the wanted key.
With this technique, we can, for example, realize
lookup by value, by specific metadata and by
parts of the name.
Without an additional index, it is not possible that
such operations perform more efficiently
than by a linear iteration key by key.
For that reason, Elektra’s core does not provide
such functions.
The function `ksLookupByName()`, however,
uses the more efficient binary search
because the array inside the `KeySet`
is ordered by name.

### External Cursor

External cursor is an alternative to the approach explained above.
Elektra provides a limited
_external cursor_
through the interface
`ksGetCursor()` and `ksSetCursor()`.
It is not possible to advance this cursor.
The difference to the internal cursor
is that the position is not stored within `KeySet`.

We considered providing an external cursor for performance reasons.
But we found out that
the speed of iteration is mainly limited because of safety checks.
The investigated methods become slower proportional to the ease of use
and safety.
When using null pointers and
range checks, the function is noticeably slower than without.
With the same amount of checks,
using an external cursor is not much faster than
`ksNext()`.
External cursor with checks is in a benchmark
about 10% faster.

But an external cursor directly accessing the array can be much
faster. Using an unchecked external cursor can be about 50%
faster than using the internal cursor with ksNext().
For this endeavour,
Elektra’s private header files need to be included.
Including private header files, however,
should not be done with levity
because ABI compatibility will be gone on any changes of the data
structures.
This fact means the application or plugin needs to be recompiled
when any of the internal structures of Elektra are
changed.
We strongly discourage including these header files.

Nevertheless, the overall
performance impact for iteration is minimal
and should not matter too much.
Even if only a single `keySetMeta()` is used inside
the iteration loop, the iteration costs are insignificant.
Only for trivial
actions such as just changing a variable, counter or marker for every key
the iteration costs become the lion's share.
In such situations
an _internal iterator_
yields better results.
For example,
`ksForEach()` applies a user defined function
for every key in a `KeySet` without having null pointer or
out of range problems.

## Trie vs. Split

Up to now,
we have discussed external data structures visible to the user of the
library.
The application and plugin programmer needs them
to access configuration.
Last, but not least,
we will show two internal data structures.
The user will not see them.
To understand the algorithm, however,
the user needs to understand them as well.

### Trie

A _Trie_ or prefix tree is an ordered tree
data structure.
In Elektra,
it provides the information
to decide
in which backend a key resides.
The algorithm, presented in [algorithm](algorithm.md),
also needs a list of all backends.
The initial approach was to iterate over the `Trie`
to get a list of all backends.
But the transformation of a `Trie` to a list of backends, contained
many bugs caused by corner cases in connection with the default backend
and cascading mount points.

### Split

So, instead of transforming the trie to a list of backends,
we introduced a new data structure called `Split`.
The name `Split` comes from the fact that
an initial key set is split into many key sets.
These key sets are stored in the `Split` object.
`Split` advanced to the central data structure for the algorithm:

```c
typedef struct _Split	Split;

struct _Split {
	size_t size;
	size_t alloc;
	KeySet **keysets;
	Backend **handles;
	Key **parents;
	int *syncbits;
};
```

The data structure `Split` contains the following fields:

- **size**: contains the number of key sets currently in `Split`.

- **alloc**: allows us to allocate more items than currently in use.

- **keysets** represents a list of key sets.
  The keys in one of the key sets are known to belong to a specific
  backend.

- **handles**: contains a list of handles to backends.

- **parents**: represents a list of keys.
  Each `parentKey` contains the
  root key of a backend. No key of the respective key set is above the
  `parentKey`.
  The key name of `parentKey` contains the mount point of a backend.
  The resolver writes the file name into the value of the `parentKey`.

- **syncbits**: are some bits that can be set for every backend.
  The algorithm uses the `syncbits` to decide if the key set needs to be
  synchronized.

Continue reading [with the error handling](error-handling.md).

## Order Preserving Minimal Perfect Hash Map (aka OPMPHM)

The OPMPHM is a non-dynamic randomized hash map of the Las Vegas type, that creates an index over the elements,
to gain O(1) access.

The elements must be arranged in an array and each element must have a unique name, to identify the elements.
The source can be found in [kdbopmphm.h](/src/include/kdbopmphm.h) and [opmphm.c](/src/libs/elektra/opmphm.c)
and also works outside of Elektra.

The OPMPHM does not store any buckets, your array of elements are the buckets and the OPMPHM represent an arbitrary
index over those. The desired index of an element, also known as the order, is set in `OpmphmGraph->edges[i].order`,
where `i` is the i-th element in your array. When the orders should represent the array indices, the default order
can be applied during the assignment step. When the orders are not the default order, `OpmphmGraph->edges[i].order`
should be set before the assignment step.

Because the OPMPHM is non-dynamic, there are no insert and delete operations. The OPMPHM gets build for a static set of
elements, once the OPMPHM is build, every:

- change of at least one indexed element name
- addition of a new element
- deletion of an indexed element

leads to an invalid OPMPHM and forces a rebuild. A build consists of two steps the mapping step and the assignment step.

During the mapping step the OPMPHM maps each element to an edge in an random acyclic r-uniform r-partite hypergraph.
In a r-uniform r-partite hypergraph each edge connects `r` vertices, each vertex in a different component.
The probability of being acyclic and the number of mapping step invocations depends on the the following variables:

- `r`: The `r` variable defines the number of components in the random r-uniform r-partite hypergraph.
  Use the `opmphmOptR (n)` function to get an optimal value for your number of elements (`n`).

- `c`: The `c` variable defines the number of vertices in each component of the random r-uniform r-partite hypergraph.
  The number of vertices in one component is defined as `(c * n / r) + 1`, where `n` is the number of elements
  and `r` is the variable from above.
  The `c` variable must have a minimal value to ensure a success probability, use the `opmphmMinC (r)` function,
  with your `r` from above.
  The ensure an optimal time until success increment the `c` variable with the value from the `opmphmOptC (n)`
  function, where `n` is the number of elements.

- `initSeed`: The initial seed set in `OpmphmInit->initSeed`.

`opmphmOptR (n)` and `opmphmOptC (n)` are heuristic functions constructed through benchmarks. Optimal is only one
mapping step invocation in 99.5% of the observed cases. The benchmarks took arbitrary uniform distributed initial seeds
and the heuristic functions are made to work with almost every seed.

### The Build

#### Initialization

Use `opmphmNew ()` and `opmphmGraphNew (...)` to instantiate the needed structures.
The function `opmphmGraphNew (...)` takes `r` and `c` as parameter. Use the `opmphmOptR (...)`
function to get your `r` value, use this `r` also to get your `c` value the following way:

`c = opmphmMinC (r) + opmphmOptC (n)`

To initialize the OPMPHM build the `OpmphmInit` must be set with information about your data.
Set your data array `OpmphmInit->data` and the element name extraction function `OpmphmInit->getName`,
which should extract the string from a single data array entry.
Provide a good seed in `OpmphmInit->initSeed`, needed in the next step.

#### Mapping

The function `opmphmMapping` uses your seed (the `OpmphmInit->seed` will be changed) and tries to
construct the random acyclic r-uniform r-partite hypergraph, this might not succeed, on cycles just call it again.

#### Assignment

The `opmphmAssignment ()` function assigns either your order (set at `OpmphmGraph->edges[i].order`) or a default order.
The `defaultOrder` parameter indicates the behavior.

After the build the OpmphmInit and OpmphmGraph should be freed.
The OPMPHM is now ready for constant lookups with the `opmphmLookup ()`.

### The Rebuild

Once build, follow the steps from the build, just omit the `opmphmNew ()` invocation.
