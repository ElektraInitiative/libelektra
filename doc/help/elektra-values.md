# elektra-values(7) -- values of elektra keys

In Elektra string-keys are preferred.
Nevertheless, binary keys, i.e. strings with null-characters embedded
are possible.

## ABSENT KEYS

Sometimes a key does not exist at all.
When using ksLookup() you will get a null pointer.
In the C++ Binding the null pointer is wrapped in a key.
Thus this is also sometimes called **NULL KEYS**.

## NULL VALUES

Null values are binary values without content.
They are the only keys that have the size 0.

Null values are always binary values.

## EMPTY VALUES

Empty values point to a string that only contains
a null byte.

Empty values are possible for both string and binary
values.
