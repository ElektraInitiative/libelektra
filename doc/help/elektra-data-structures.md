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
actually \empha[ADT]{abstract data types},
also known as ADT.
The API is
designed so that different implementations of the
data structures can be used internally.
More information on this topic is given in \cite{raab:00,adt:00}.

A \intro[data structure!hash]{hash}
presents a good candidate as alternative data structure, especially for
the metadata interface.
It is believed to be much faster on
lookup, but considerably slower on sorted enumeration.

\intro[data structure!AVL tree]{AVL trees} also serve as a competitor.
AVL trees are expected to be
faster for inserting keys at any place, but may be slower for appending
because of the needed reorganisations.
Their
disadvantage is that they need to allocate
a large number of small pieces of memory.
Further investigations, namely implementation and benchmarks,
are required to decide.

Currently the `KeySet` is implemented as a sorted array\cite{raab:00}.
It is fast on appending and iterating, and
has nearly no size-overhead.

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



## Metadata

\label{implementation meta}

In this section, we discuss the implementation of metadata.
Metakey is implemented directly in a `Key` as shown in
Figure~\ref{fig:metakey}.
Every metakey belongs to a key \empha{inseparable}.
Unlike
normal key names there is no absolute path for it in the hierarchy,
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



### Interface}

The interface to access metadata consists of the following functions:

Interface of metadata:

	const Key *keyGetMeta(const Key *key, const char* metaName);
	ssize_t    keySetMeta(Key *key, const char* metaName,
		const char *newMetaString);

Inside a `Key`, metadata with a given metaname and a metavalue can be set
using \method{keySetMeta()} and retrieved using \method{keyGetMeta()}.
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

\begin{lstlisting}[caption={Interface for copying metadata}]
int keyCopyMeta(Key *dest, const Key *source, const char *metaName);
int keyCopyAllMeta(Key *dest, const Key *source);
\end{lstlisting}

The name `copy` is used because the information is copied from one key to
another. It has the same meaning as in `ksCopy()`.
In both cases it is a flat copy.
\method{keyCopyAllMeta()} copies all metadata from one key to another.
It is more efficient than a loop with the same effect.

`keyDup()` copies all metadata as expected.
Sharing metadata makes no difference from the user's point of view.
Whenever a metavalue is changed a new metakey is generated.
It does not matter if the old metakey was shared or not.
This is the reason why a const pointer is always passed back.
The metakey must not be changed because it can be used within another
key.



## KeySet}

The data structure of `KeySet` did not change between Elektra~$0.7$ and
Elektra~$0.8mile4$
as explained in \cite{raab:00}.
This subsection describes what has changed and deals with
some general implementation issues.

### Operations}

`KeySet` resembles the classical mathematical
set. Operations like union, intersection or difference are well
defined. In mathematics typically every operation
yields a new set.
Instead, we try to reuse sets in the following ways:

\begin{enumerate}

\item A completely new and independent `KeySet` as return value would
resemble the mathematical ideal closely. This operation would be
expensive. Every `Key` needs to be duplicated and
inserted into a new `KeySet`.

Such a \intro{deep duplication} was only needed in `kdbSet()` as we will see
in \secref{deep duplicate}.

\item The resulting
`KeySet` is created during the operation, but only a flat copy is
made. This means that the keys in it are actually not duplicated, but only
their reference counter is increased.
This method is similar to the mathematical model.
Compared with a deep copy it can achieve good performance.
But all changes to the values of keys in the resulting `KeySet`
affect the original `KeySet`, too.

`ksDup(const KeySet *source)` produces a new `KeySet` that way. The
`source` is not changed as shown by the `const` modifier.

\item The result of the operation is applied to the
`KeySet` passed as argument directly.
This is actually quite common, but for this situation
other names of the operations are more suitable.

For example, a union
which changes the `KeySet` is called `ksAppend()`.

\item A new `KeySet` is created, but the `KeySet` passed as
parameter is reduced by the keys needed for the new `KeySet`. This
is useful in situations where many operations have to be applied in
a sequence reducing the given `KeySet` until no
more keys are left.
None of the reference pointers changes in this situation.

`ksCut(KeySet *ks, const Key *cutpoint)` works that way.
All keys below the `cutpoint` are moved from `ks` to the returned key
set.

\end{enumerate}

### Consistency}

There are
several ways to define consistency relations on key sets.
For \intro[consistency!strict]{strict consistency}
every parent key must exist before the user
can append a key to a key set.
For example, the key set with the keys

	system
	system/elektra
	system/elektra/mountpoints

would allow the
key \keyname{system/elektra/mountpoints/tcl} to be added,
but not the key
\keyname{system/apps/abc} because \keyname{system/apps} is missing.
File systems provide this kind of consistency.

These semantics are however not useful for configurations.
Especially for
user configurations often only some keys need to be overwritten.
It is
not a good idea to copy all parent keys to the users configuration just
because some constraint forces us to do so.
For this reason we use a less strict definition of consistency supporting
such holes.

We also evaluated a form of \intro[consistency!weak]{weak consistency}.
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
of keys currently in the key set and $x$ is the \empha{depth} of the
key. The depth is the number of `/` in the key name.
The worst-case of the complexity applies when the inserting works
without a parent key.
For example, with the keys
\begin{lstlisting}
user/sw/apps/abc/current/bindings
user/sw/apps/abc/current/bindings/key1
user/sw/apps/abc/current/bindings/key2
\end{lstlisting}
\noindent
the weak consistency would allow inserting
\keyname{user/sw/apps/abc/current/bindings/key3}
because it is directly below an existing key.
It would also allow adding
\keyname{user/sw/apps/xyz/current}
because it does not have any parent key.
But it would not allow
\keyname{user/sw/apps/abc/current/bindings/dir/key1}
to add.
The worst-case complexity was
found to be too expensive, and hence `KeySet` has
\intro[consistency!no]{no consistency} check at all.

This means any key with a valid key name
can be inserted into `KeySet`.
The `KeySet` is changed so that
it is now impossible to append keys without a name.
`ksAppendKey(ks, Key *toAppend)` takes ownership of the key
`toAppend` and will delete the key in that case.
The caller does not have to free `toAppend`: either it is in the key set
or it is deleted.

\intro[binary search]{Binary search} determines
the position where to insert a key.
The C version of binary search `bsearch()` cannot tell where to insert
a key when it is not found.
So the algorithm has to be reimplemented.
Java's binary search `binarySearch()` uses a trick to both indicate
where a key is found and where to insert it with the same return code
by returning the negative value $((-insertion point) - 1)$
indicating where the new value should be inserted when the key
is not found.
Elektra now also uses this trick internally.


### Internal Cursor


`KeySet` supports an
\empha[iterator!external]{external iterator}\cite{adt:01}
with the two functions
`ksRewind()` to go to the beginning and `ksNext()` to
advance the \empha[cursor!internal]{internal cursor} to the next key.
This side effect is used to indicate a position for
operations on a `KeySet` without any additional parameter.
This technique is comfortable to see which
key has caused an error after an unsuccessful key database operation.

Elektra only has some functions to change the cursor of a key set.
But these allow the user to compose powerful functions.
Plugins do that extensively as we will see later
in \secref{ksLookupRE}.
%
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
The function \method{ksLookupByName()}, however,
uses the more efficient \empha{binary search}
because the array inside the `KeySet`
is ordered by name.


### External Cursor

External cursor is an alternative to the approach explained above.
Elektra provides a limited
\empha[cursor!external]{external cursor}
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
the `ksNext()`
interface\footnote{External cursor with checks is in a benchmark
about 10\% faster.}.

But an external cursor directly accessing the array can be much
faster\footnote{Using an unchecked external cursor can be about 50\%
faster than using the internal cursor with \lstinline{ksNext()}.}.
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
an \empha[iterator!internal]{internal iterator}
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

\intro[Trie@\lstinline{Trie}]{Trie} or prefix tree is an ordered tree
data structure.
In Elektra,
it provides the information
to decide
in which backend a key resides\cite{raab:00}.
The algorithm, presented in \secref{algorithm},
also needs a list of all backends.
The initial approach was to iterate over the `Trie`
to get a list of all backends.
But the transformation of a `Trie` to a list of backends, contained
many bugs caused by corner cases in connection with the default backend
and cascading mount points.

### Split

So, instead of transforming the trie to a list of backends,
we introduced a new data structure \intro[Split@\lstinline{Split}]{Split}.
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

\begin{description}

\item[size] contains the number of key sets currently in `Split`.

\item[alloc] allows allocating more items than currently in use.

\item[keysets] represents a list of key sets.
The keys in one of the key sets are known to belong to a specific
backend.

\item[handles] contains a list of handles to backends.

\item[parents] represents a list of keys.
Each `parentKey` contains the
root key of a backend. No key of the respective key set is above the
`parentKey`.
The key name of `parentKey` contains the mount point of a backend.
The resolver writes the file name into the value of the `parentKey`.

\index{syncbits}
\item[syncbits] are some bits that can be set for every backend.
The algorithm uses the `syncbits` to decide if the key set needs to be
synchronised.

\end{description}

Continue reading [with the error handling](elektra-error-handling.md).
