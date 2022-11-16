# Internal KeySet Cache

## Problem

`kdbGet` might return more or fewer keys than requested.
This was found confusing several times.

### More Keys

Even if calling `kdbGet` with a parent key below a mountpoint, `kdbGet` will nevertheless return all keys of the mountpoint.
Pseudo code example, assuming there is a mountpoint at `/mountpoint` and a key `/mountpoint/other`:

```c
kdbGet (kdb, ks, keyNew("/mountpoint/below"));
assert (ksLookup (ks, "/mountpoint/other") == NULL);
```

It was found unexpected that this assert will fail.

In a similar fashion, calling `kdbSet` without the seemingly superfluous keys causes Elektra to unintentionally delete them from disk.

```c
kdbGet (kdb, ks, keyNew("/mountpoint/below"));
KeySet * below = ksCut (ks, keyNew("/mountpoint/below"));

ksSet (kdb, below, keyNew("/mountpoint/below"));
// suddenly /mountpoint/other has been removed from the configuration file on disk, even if the user explicitly stated to only change stuff in /mountpoint/below 
```

### Fewer Keys

When doing a second `kdbGet` with a new keyset no keys will be returned when no backends report changed data, because kdb internally thinks the data is already up-to-date:

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

It was found unexpected that the second assert - should find key (2) - will fail.

## Constraints

- memory consumption must be low for `kdbGet`, see [4. Goal: Performance](/doc/GOALS.md), in particular, deep duplication is too expensive
- For non-optional parts of Elektra, also non-POSIX systems must be supported

## Assumptions

## Considered Alternatives

### Keep Current Situation

Improve documentation to make people more aware of these two problems:

- add a tutorial about `kdbGet` semantics
- add full examples how to correctly work with `kdbGet`

### Changing `parentKey` according to mountpoints

"More keys":

Upon returning from `kdbGet`/`kdbSet`, set the keyname of parentKey to the key that actually is parent of the data that was loaded.
I.e. to the mountpoint of the backend that contains parentKey.
`parentKey` is already an inout-type argument, since we use both it's value and metadata to return some information.

A few people found this behaviour just as unexpected as the current problem.
E.g. a sequence of `kdbGet` and `kdbSet` with the same parent key might lead to different outcomes depending on the mountpoints.

"Fewer keys":

This is a bug in the logic of the "nothing changed" optimization.
A partial solution would be to copy the keys from `backendData->keys`, so they are actually there, and we don't just assume they are there.
Still some extra steps are required to make it work in all cases, e.g.:

```c
TEST_F (Simple, NothingToDo2)
{
	using namespace kdb;
	KDB kdb;
	KeySet ks;

	auto parentKey = "system:" + testRoot;

	EXPECT_EQ (kdb.get (ks, parentKey), 0) << "nothing to do in get";

	ks.append (Key (parentKey + "a", KEY_VALUE, "x", KEY_END));
	kdb.set (ks, parentKey);

	EXPECT_EQ (kdb.get (ks, parentKey), 0) << "nothing to do in get";
	EXPECT_TRUE (ks.lookup (parentKey + "a")) << "key a not found";
	EXPECT_EQ (ks.lookup (parentKey + "a").get<std::string> (), "x") << "key a with wrong value";
	// TODO: adding the line below breaks the test
	//	 This is because the Key instance is shared between `ks` and the internal data of `kdb`.
	// ks.lookup(parentKey + "a").set("y");

	KeySet ks2;
	EXPECT_EQ (kdb.get (ks2, parentKey), 0) << "nothing to do in get";
	EXPECT_TRUE (ks2.lookup (parentKey + "a")) << "key a not found";
	EXPECT_EQ (ks2.lookup (parentKey + "a").get<std::string> (), "x") << "key a with wrong value";
}
```

A very simple way to make it work would be to make the keys returned by kdbGet read-only.
If we do not do this, we need a deep-copy or a copy-on-write copy.

### Cachefilter Plugin

Naively one would simply cache the whole keyset and use `ksBelow` to always get the keyset.

This idea was implemented and later on [discarded: a3d95f](https://github.com/ElektraInitiative/libelektra/commit/a3d95f07160d792fdd0f169d8543138c32a2f580)

The main problems are:

- very high memory consumption (duplication of KeySets)
- problems specific to [hooks](../4_partially_implemented/hooks.md), see [#1072](https://issues.libelektra.org/1072)

### MMAP Cache with parent key

We make the mmap cache non-optional so that we always have a keyset of configuration data internally.
The cache will be used to do change tracking.
From this keyset, we use `ksBelow` to return the correct keyset.

Disadvantage: mmap implementation for Windows would be needed.

The cache should be updated at the end of every kdbGet that was a cache miss.
So during kdbSet (assuming there is no external modification, i.e. conflict) the on-disk data of the cache should always be up-to-date.
The idea would be to just read the cached keyset from disk and diff against the current keyset in kdbSet.

This would mean enabling change tracking also enables the cache (or at least updating the cache, we don't have to use it in kdbGet).
If that's not wanted or if the cache data cannot be used directly for some other reason, the same approach could still be used.
We'd just have to write the keyset to disk during kdbGet and use it during kdbSet.
Since disk space is far less precious than RAM, we could even create separate files for every parent key.
If we do go down this route, kdbClose should clean up the files created by this KDB instance to avoid wasting disk space.

### MMAP Cache without parent key

We make the mmap cache non-optional and only use a single cache, caching everything.
We remove the parent key of `kdbGet` and `kdbSet` and always return the keyset of the whole KDB.

- Disadvantage: mmap implementation for Windows would be needed
- Then also symlinks (fallback, override etc.) and constraints for keys outside of the parentKey would work.
  It would make the `-a` option of `kdb get` unnecessary.

It is still unclear whether this would actually be a good default behavior.
Normally it is expected for mountpoints to be isolated
If symlinks work like this, the isolation is partially broken.
One could argue that the problem right now is that such "broken" symlinks are not prevented.
The proposed solution doesn't completely fix the problem either

For example:

- `system:/foo` is a mountpoint, there are no other mountpoints
- `spec:/foo/bar` refers to `system:/abc` via `meta:/override`

Now in a plugin that is part of the mountpoint `system:/foo` doing a `ksLookupByName (ks, "/foo/bar", 0)` would NOT use `system:/abc`, because that key is never part of the keyset passed to this plugin.

However, in an application that used `system:/foo` as the parent, the `meta:/override` would work with your proposal.
This seems like very confusing behavior, because both the application and the plugin seemingly use the same parent key.

### Data restrictions

Make all the keys returned by kdbGet completely read-only.
To change the data it is required to append an entirely new key to replace the existing one.
Then we just need to keep a shallow copy internally.

### API restrictions

Change the API and remove KeySet from kdbGet and kdbSet also option 4 in [the operation sequences decision](../0_drafts/operation_sequences.md).
If the keyset is owned by the KDB handle, it should not be such a big surprise, if there is extra data in there.

This only fixes the "Fewer Keys" issue.

### Copy On Write

We keep a duplicated keyset in-memory and tag the keys as copy-on-write (COW).
From this keyset, we use `ksBelow` to return the correct keyset.
If the user tries to change the value or metadata of these keys, the data gets duplicated.
I.e. the original keyset is not changed.
The name is not relevant.
It is always read-only, because the key is in at least one keyset (the internal one).

Possible copy-on-write implementations are described in [another decision](../1_in_discussion/copy_on_write.md).

## Decision

Implement the copy-on-write approach.

We keep a copy of all keys returned by the backends in memory.
We use `ksBelow` to only return a copy-on-write copy of keys the user requested on `kdbGet`.

If the user tries to change the value or metadata of these keys, the data gets duplicated (copy-on-write).
I.e. the data of the original keys is not changed.
The name is not relevant.
It is always read-only, because the key is in at least one keyset (the internal one).
Possible copy-on-write implementations are described in [another decision](../1_in_discussion/copy_on_write.md).

In `kdbSet` we use the user-provided `KeySet` for all backends strictly below `parentKey` as before.
For the backend that contains `parentKey`, we start with the internally cached data.
We then remove everything that is at or below `parentKey` (via `ksCut`) and replace it with the data from the user-provided `KeySet`.
Keys not at or below `parentKey` therefore remain untouched.

## Rationale

Semantics can be provided without additional code or overhead in the core.
As we need copy-on-write for efficient change tracking anyway, it makes sense to also use this approach for the internal cache.
The copy-on-write solution also does not require any changes or restrictions to the current API.

## Implications

- Before we can implement this decision, we need to implement the [copy-on-write decision](../1_in_discussion/copy_on_write.md).
- `kdbGet` will only return copy-on-write copies of keys below `parentKey` from the internal cache.
- `kdbSet` will use the keys within the internal cache to supplement all the keys above `parentKey` so that backends can write the correct data.

## Related Decisions

- [Global Validation](../0a_delayed/global_validation.md)
- [Copy On Write](../1_in_discussion/copy_on_write.md)

## Notes

Problem "Fewer Keys" was found to be a ["horrible problem"](https://pull.libelektra.org/4619)

Issues where the problem described here was found confusing:

- [#760](https://issues.libelektra.org/760)
- [#1363](https://issues.libelektra.org/1363)

@mpranj wrote about the performance for `MMAP Cache without parent key`:

> in my benchmarks the pointer correction was never a bottleneck.
