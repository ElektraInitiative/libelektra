# Internal KeySet Cache

## Problem

`kdbGet` might return more or less keys than requested.
This was found confusing several times.

### More Keys

Even if calling `kdbGet` with a parent key below a mountpoint, `kdbGet` will nevertheless return all keys of the mountpoint.
Pseudo code example, assuming there is a mountpoint at `/mountpoint` and a key `/mountpoint/other`:

```
kdbGet (kdb, ks, keyNew("/mountpoint/below"));
assert (ksLookup (ks, "/mountpoint/other") == NULL);
```

If was found unexpected that this assert will fail.

### Less Keys

When doing a second `kdbGet` with a new keyset no keys might be returned, because kdb internally is up-to-date.
Pseudo code example, assuming there is a key `/somewhere/key`:

```
kdbGet (kdb, ks1, keyNew("/somewhere"));
assert (ksLookup (ks1, "/somewhere/key") != NULL);
kdbGet (kdb, ks2, keyNew("/somewhere"));
assert (ksLookup (ks2, "/somewhere/key") != NULL);
```

If was found unexpected that the second assert will fail.

## Constraints

- memory consumption must be low for `kdbGet`, see [4. Goal: Performance](/doc/GOAL.md), in particular, deep duplication is too expensive

## Assumptions

## Considered Alternatives

### Keep Current Situation

Improve documentation to make people more aware of these two problems:

- add a tutorial about `kdbGet` semantics
- add full examples how to correctly work with `kdbGet`

### Cachefilter Plugin

Naively one would simply cache the whole keyset and use `ksBelow` to always get the keyset.

This idea was implemented and later on discarded: a3d95f07160d792fdd0f169d8543138c32a2f580

The main problems are:

- very high memory consumption (duplication of KeySets)
- problems specific to [hooks](hooks.md), see [#1072](https://issues.libelektra.org/1072)

### MMAP Cache with parent key

We make the mmap cache non-optional so that we always have a keyset of configuration data internally.
From this keyset, we use `ksBelow` to return the correct keyset.

**Cons:**

- invalidation of OPMPHM

### MMAP Cache without parent key

We make the mmap cache non-optional and only use a single cache, caching everything.
We remove the parent key of `kdbGet` and `kdbSet` and always return the keyset of the whole KDB.

### In-Memory COW Cache

We keep a duplicated keyset in-memory and tag the keys as copy-on-write (COW).
From this keyset, we use `ksBelow` to return the correct keyset.
If the user tries to change the keys, the value or metadata gets duplicated, so that the original keyset is not changed.
The name can stay shared.
Pseudo code example:

```
Key * key = keyNew ("dir:/something", KEY_VALUE, "my value", KEY_END);
keyCopy (key_dup, key, ELEKTRA_COW);
assert (keyString(key) == keyString(key_dup));
keySetString (key_dup, "other value"); // COW done here
assert (keyString(key) != keyString(key_dup));
assert (keySetName (key_dup, "dir:/valid") == -1); // must fail, as we have a COW key
assert (keyName(key) == keyName(key_dup)); // stays always valid
```

This is already implemented for the MMAP cache, so the implementation should be straightforward (do the same COW duplications as done for MMAP).

**Pros:**

- Elektra doesn't require MMAP

## Decision

Not yet decided.

## Rationale

Semantics can be provided without additional code or overhead in the core.

## Implications

## Related Decisions

- [Global Validation](global_validation.md)

## Notes

Issues where the problem described here was found confusing:

- [#760](https://issues.libelektra.org/760)
- [#1363](https://issues.libelektra.org/1363)
