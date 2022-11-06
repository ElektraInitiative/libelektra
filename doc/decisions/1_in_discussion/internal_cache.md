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

```c
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
- problems specific to [hooks](../4_partially_implemented/hooks.md), see [#1072](https://issues.libelektra.org/1072)

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
If the user tries to change the value or metadata of these keys, the data gets duplicated.
I.e. the original keyset is not changed.
The name is not relevant.
It is always read-only, because the key is in at least one keyset (the internal one).
Pseudo code example:

```c
Key * key = keyNew ("dir:/something", KEY_VALUE, "my value", KEY_END);
keyCopy (key_dup, key, ELEKTRA_CP_COW);
assert (keyString(key) == keyString(key_dup));
keySetString (key_dup, "other value"); // COW done here
assert (keyString(key) != keyString(key_dup));
assert (keySetName (key_dup, "dir:/valid") == -1); // must fail, as we have a COW key
assert (keyName(key) == keyName(key_dup)); // stays always valid
```

This is already implemented for the MMAP cache, so the implementation should be straightforward:
Do the same COW duplications as done for MMAP but with a different flag.

@kodebach wrote:

> What I wanted to say is that mmap already does COW, so we can reuse the code and probably the flag.
> If there is some code that is only needed for mmap and not COW, we could make mmap set two flags one for mmap and one for the general COW code.

For the metadata, however, also COW KeySets might be needed (at least with the current API).
Example:

```c
keyCopy (cow, key, ELEKTRA_CP_COW);
KeySet * cowMeta = keyMeta (cow);
ksAppendKey (cowMeta, keyNew ("meta:/whatever", KEY_VALUE, "abc", KEY_END));
ksRemoveByName (cowMeta, "meta:/type");
```

**Pros:**

- Elektra doesn't require MMAP

**Cons:**

- Lifetime of a copied COW key MUST be less than the key it was copied from.
  We can not track how many keys point to the same data this way, so we can only free data if the key does not have the COW flag.
  If the original key gets deleted, using a COW key that points to the same data will lead to corrupt data.

### Data restrictions

@kodebach wrote:

> Make all the keys returned by kdbGet completely read-only.
> To change the data you need to append an entirely new key to replace the existing one.
> Then we just need to keep a shallow copy internally.

### API restrictions

@kodebach wrote:

> Change the API and remove KeySet from kdbGet and kdbSet also option 4 in [the operation sequences decision](operation_sequences.md).
> If the keyset is owned by the KDB handle, it should not be as big surprise, if there is extra data in there.
> I certainly wouldn't try to asset anything on the contents of a KeySet that I don't own directly, unless the condition is explicitly documented somewhere.

@markus2330 wrote:

> I disagree, it is actually the same kind of surprise for "More Keys".
> Only the "Fewer Keys" would get fixed.

### Full-blown copy-on-write implementation

Make Elektras `Key` and `KeySet` datastructures copy-on-write.
This requires some major refactoring of code within `libelektra-core`.
Code that does only interact with the datastructures via the public `libelektra-core` API should not notice any differences.
The `mmapstorage` plugin will need a major refactoring.

### Changes to `Key`

For the `Key`, we need to extract everything for the data and name into their own structs.
This is done for memory-management reasons, as we need to track how many keys point to the same data and/or name.

In the data structures below, an empty key does have 96 bytes.
An empty key of the current implementation has 64 bytes.

A copied, non-modified key with the data structure below does always have 32 bytes.
A copied, non-modified key of the current implementation has at least 64 bytes (for an empty key), but in reality much more as the name and the data are also copied.

```c
struct _KeyData {
    union {
        char * c;
        void * v;
    } data;

    size_t dataSize;

    uint16_t refs;
    uint16_t reserved;
};

struct _KeyName {
    char * key;
    size_t keySize;

    char * ukey;
    size_t keyUSize;

    uint16_t refs;
    uint16_t reserved;
};

struct _Key {
    struct _KeyData * keyData;    
    struct _KeyName * keyName;
    KeySet * meta;
    keyflag_t flags;

    uint16_t refs;
    uint16_t reserved;
};
```

### Changes to `KeySet`

For `KeySet`, we need to split out everything to do with the stored keys into a separate datastructure.
This includes the array itself, the sizes and the hashmap.

An empty keyset with the datastructure below has 80 bytes.
An empty keyset with the current implementation has 64 bytes.

A copied, non-modified keyset with the datastructure below has always 32 bytes.
A copied, non-modified keyset with the current implementation has at least 64 bytes (for an empty keyset).

```c
struct _KeySetData {
    struct _Key ** array;
    size_t size;  /**< Number of keys contained in the KeySet */
    size_t alloc; /**< Allocated size of array */

    Opmphm * opmphm;
    OpmphmPredictor * opmphmPredictor;

    uint16_t refs; /**< Reference counter */
    uint16_t reserved; /**< Reserved for future use */
};

struct _KeySet {
    struct _KeySetData * data;

    struct _Key * cursor; /**< Internal cursor */
    size_t current;		  /**< Current position of cursor */
	
    ksflag_t flags;

    uint16_t refs; /**< Reference counter */
    uint16_t reserved; /**< Reserved for future use */
};
```


## Decision

## Rationale

Semantics can be provided without additional code or overhead in the core.

## Implications

## Related Decisions

- [Global Validation](../0a_delayed/global_validation.md)

## Notes

Problem "Fewer Keys" was found to be a ["horrible problem"](https://pull.libelektra.org/4619)

Issues where the problem described here was found confusing:

- [#760](https://issues.libelektra.org/760)
- [#1363](https://issues.libelektra.org/1363)

@mpranj wrote about the performance for `MMAP Cache without parent key`:

> in my benchmarks the pointer correction was never a bottleneck.
