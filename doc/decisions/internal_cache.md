# Internal KeySet Cache

## Problem

`kdbGet` might return more or fewer keys than requested.
This was found confusing several times.

### More Keys

Even if calling `kdbGet` with a parent key below a mountpoint, `kdbGet` will nevertheless return all keys of the mountpoint.
Pseudo code example, assuming there is a mountpoint at `/mountpoint` and a key `/mountpoint/other`:

```
kdbGet (kdb, ks, keyNew("/mountpoint/below"));
assert (ksLookup (ks, "/mountpoint/other") == NULL);
```

It was found unexpected that this assert will fail.

### Fewer Keys

When doing a second `kdbGet` with a new keyset no keys will be returned when no backends report changed data, because kdb internally thinks the data is already up-to-date.
A unit test by @atmaxinger:

```
static void test_doubleGet (void)
{
	printf("running %s\n", __func__);

	// Setup
	Key * parentKey = keyNew("/somewhere", KS_END);
	KeySet * ks = ksNew(0, KS_END);
	KDB * kdb = kdbOpen (ksNew(0, KS_END), parentKey);
	kdbGet (kdb, ks, parentKey);
	ksAppendKey (ks, keyNew ("user:/somewhere", KEY_VALUE, "abc", KEY_END));
	ksAppendKey (ks, keyNew ("user:/somewhere/key", KEY_VALUE, "xyz", KEY_END));
	kdbSet (kdb, ks, parentKey);
	kdbClose (kdb, parentKey);

	// Scenario
	kdb = kdbOpen (ksNew(0, KS_END), parentKey);

	KeySet * ks1 = ksNew (0, KS_END);
	KeySet * ks2 = ksNew (0, KS_END);

	kdbGet (kdb, ks1, keyNew("/somewhere", KEY_END));
	succeed_if (ksLookupByName (ks1, "/somewhere/key", 0) != NULL, "should find key (1)");
	kdbGet (kdb, ks2, keyNew("/somewhere", KEY_END));
	succeed_if (ksLookupByName (ks2, "/somewhere/key", 0) != NULL, "should find key (2)");
}
```

> It actually outputs should find key (2) so the assertion fails.

It was found unexpected that the second assert will fail.

## Constraints

- memory consumption must be low for `kdbGet`, see [4. Goal: Performance](/doc/GOALS.md), in particular, deep duplication is too expensive

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
The name is not relevant.
It is always read-only, because the key is in at least one keyset (the internal one).
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

This is already implemented for the MMAP cache, so the implementation should be straightforward:
Do the same COW duplications as done for MMAP but with a different flag.

**Pros:**

- Elektra doesn't require MMAP

### Data restrictions

@kodebach wrote:
> Make all the keys returned by kdbGet completely read-only.
> To change the data you need to append an entirely new key to replace the existing one.
> Then we just need to keep a shallow copy internally.

### API restrictions

@kodebach wrote:
> Change the API and remove KeySet * from kdbGet and kdbSet (also option 4 in #4574).
> If the keyset is owned by the KDB handle, it should not be as big surprise, if there is extra data in there.
> I certainly wouldn't try to asset anything on the contents of a KeySet * that I don't own directly, unless the condition is explicitly documented somewhere.

@markus2330 wrote:
> I disagree, it is actually the same kind of surprise for "More Keys".
> Only the "Fewer Keys" would get fixed.

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
