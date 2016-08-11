elektra-data-structures(7) -- data structures
=============================================

For an introduction, please
[read first about elektra classes](elektra-classes.md).
You might want to read
[about architecture first](elektra-architecture.md).


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
because of the needed reorganisations.
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

Special care has been taken in Elektra to support all
changes within the data
structures without any ABI changes.
ABI changes would entail the recompilation of applications
and plugins using Elektra.
The functions `keyNew()`, `ksNew()`
and `kdbOpen()` allocate the data structures for the applications.
The user only gets pointers to them.
It is not possible for the user to allocate or access these data
structures directly when only using the public header file `<kdb.h>`.
The functions `keyDel()`, `ksDel()` and `kdbClose()` free the
resources after use.
Using the C++ binding deallocation is done automatically.


## Meta Data

Read [here](elektra-meta-data.md).


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
  None of the reference pointers changes in this situation.

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

	system
	system/elektra
	system/elektra/mountpoints

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
of keys currently in the key set and $x$ is the *depth* of the
key. The depth is the number of `/` in the key name.
The worst-case of the complexity applies when the inserting works
without a parent key.
For example, with the keys

	user/sw/apps/abc/current/bindings
	user/sw/apps/abc/current/bindings/key1
	user/sw/apps/abc/current/bindings/key2

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
advance the *internal cursor* to the next key.
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
With this technique, we can, for example, realise
lookup by value, by specific metadata and by
parts of the name.
Without an additional index, it is not possible that
such operations perform more efficiently
than by a linear iteration key by key.
For that reason, Elektra's core does not provide
such functions.
The function `ksLookupByName()`, however,
uses the more efficient binary search
because the array inside the `KeySet`
is ordered by name.


### External Cursor

External cursor is an alternative to the approach explained above.
Elektra provides a limited
*external cursor*
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
the `ksNext()`.
External cursor with checks is in a benchmark
about 10% faster.

But an external cursor directly accessing the array can be much
faster. Using an unchecked external cursor can be about 50%
faster than using the internal cursor with ksNext().
For this endeavour,
Elektra's private header files need to be included.
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
an *internal iterator*
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

A *Trie* or prefix tree is an ordered tree
data structure.
In Elektra,
it provides the information
to decide
in which backend a key resides.
The algorithm, presented in [algorithm](elektra-algorithm.md),
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

	typedef struct _Split	Split;

	struct _Split {
		size_t size;
		size_t alloc;
		KeySet **keysets;
		Backend **handles;
		Key **parents;
		int *syncbits;
	};

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
synchronised.

Continue reading [with the error handling](elektra-error-handling.md).

## Order Preserving Minimal Perfect Hash Map

All structs are defined in [opmphm.h](/src/include/opmphm.h).

### Vstack

The `Vstack` is a virtual stack implementation with dynamic memory allocation.
The allocation doubles the space if the `Vstack` is full and reduces the space by half
if the memory usage drops below a quarter.

The data will be stored as void pointer and can be inserted with the push call.
Retrieve is possible through the pop call.
Besides the classical init and del functions, an is-empty check is also implemented.

To gain performance a clear function is also present to reset the `Vstack` for reuse.
Allocation will not be performed until the first pop.

The following [link](/src/libs/elektra/opmphm_vstack.c) leads to the code and
[this](@ref Vstack) one to the docu.

### Vheap

The `Vheap` is a virtual heap implementation with dynamic memory allocation.
A heap is an efficient data structure, if an ordered retrieval is needed.
The allocation works like the `Vstack` allocation.

The data will be stored as void pointer and can be inserted with the insert call.
At the point of insertion the data will be stored in a binary tree, where each parent and his children obey the order.
The order is defined by a compare function, set at init.

The compare function compares two elements and shall return 1 on a > b and 0 otherwise to construct a maximum heap.
And 1 on a < b and 0 otherwise to construct a minimum heap.

At the removal the fist element in the data array will be taken and the data gets reordered.
The ordering takes log (n) time so the complexity for the insert and remove is log (n).

Beside the classical init and del functions an is-empty check is also implemented.
The `Vheap` can also be cleared, reallocation works like the `Vstack` clear.

The following [link](/src/libs/elektra/opmphm_vheap.c) leads to the code and
[this](@ref Vheap) one to the docu.

####Example
If the order function constructs a maximum heap it holds for all elements if they have any children:

parent > child0 and parent > child1
