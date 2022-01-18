# Classes

This overview complements the introduction in
[the API documentation](https://doc.libelektra.org/api/latest/html/).

## Key

A `Key` consists of a name, a value and metadata.
It is the atomic unit in the key database. Its main purpose is that it
can be serialized to be written out to permanent storage. It can be
added to several aggregates using reference counting.
Putting `Key` objects into other data structures of supported
programming languages presents no problem.

## KeySet

The central data structure in Elektra is a `KeySet`. It aggregates `Key`
objects in order to describe configuration in an easy but complete way.
As the name "set" already implies every `Key` in a `KeySet` has a
unique name. A user can iterate over the `Key` objects of a `KeySet`.
`KeySet` sorts the keys by their names. This yields a deterministic
order advantage. So, independent of the appending sequence and, in
particular, the number of fetches and updates, `KeySet` guarantees the
same order of the `Key` objects. Some configuration storage systems
need this property, because they cannot remember a specific order.
On the other hand, any particular order can easily be introduced
(See [order](/doc/METADATA.ini)).

On the one side backends generate or store a `KeySet` object and, on the
other side, elektrified applications receive and send a `KeySet` object.
Both sides, as well as the core in between, have the possibility to
iterate, update, modify, extend and reduce the key set. Appending of
new or existing `Key` objects extends the key set. Otherwise it can be
reduced if keys are popped out. The `Key` object becomes independent of
the `KeySet` afterwards. The user can still change such a key or append
it into another key set. The affiliation to a key set is not exclusive.

Every key in a `KeySet` object has a unique name. Appending `Key` objects with
the same name will override the already existing `Key` object.

## KDB

While objects of `Key` and `KeySet` only reside in memory,
Elektra’s third class
`KDB` actually provides access to the global key database. `KDB`,
an abbreviation of key database, is responsible for
actually storing and receiving configuration. `KeySet` represents the
configuration when communicating with `KDB`.
The typical elektrified application collects its configuration by one or
many calls of `kdbGet()`.
As soon as the program finishes its work with the
`KeySet`,
`kdbSet()` is in charge of writing all changes back to the key
database.

This technique has some advantages. First, applications have full
control over modifying
`Key` and `KeySet` objects without touching the key database.
Second,
the decision how many `KeySet` objects the application
administrates is left to the application.
It can choose how to split up the `KeySet` objects.
The main reason for this technique is that for backend development the
same data structure is used, and
as we will see, the borderline between application
and backend development becomes blurred.

The application adapts the configuration between `kdbGet()`
and `kdbSet()` in memory.
The modifications are not only
faster, they also allow large atomic
configuration upgrades, robust merging of settings and handling of
complicated inter-relationships between keys without problematic
intermediate steps.
Elektrified applications, however, should be aware of conflicts.
It can happen that the key database is
changed while working with a `KeySet`.
Then, attempts to use `kdbSet()` lead to a conflict.
`KDB` detects such situations gracefully and lets the application decide
which configuration should be used.

For details and background
[read more about elektra data structures](data-structures.md).
For further information see
[the API documentation](https://doc.libelektra.org/api/latest/html/).
