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

- Disadvantage: mmap implementation for Windows would be needed

@kodebach wrote:

> I'm not entirely sure this is possible (@mpranj may know more), but the way I understand it the cache should be updated at the end of every kdbGet that was a cache miss. So during kdbSet (assuming there is no external modification, i.e. conflict) the on-disk data of the cache should always be up-to-date. My idea would be to just read the cached keyset from disk and diff against the current keyset in kdbSet.
>
> This would mean enabling change tracking also enables the cache (or at least updating the cache, we don't have to use it in kdbGet). If that's not wanted or if the cache data cannot be used directly for some other reason, the same approach could still be used. We'd just have to write the keyset to disk during kdbGet and use it during kdbSet. Since disk space is far less precious than RAM, we could even create separate files for every parent key. If we do go down this route, kdbClose should cleanup the files created by this KDB instance to avoid wasting disk space.
>
> This approach wouldn't be very performant (since it uses disk IO), but especially if we can use the cache data, it should be pretty easy to do.

### MMAP Cache without parent key

We make the mmap cache non-optional and only use a single cache, caching everything.
We remove the parent key of `kdbGet` and `kdbSet` and always return the keyset of the whole KDB.

- Disadvantage: mmap implementation for Windows would be needed
- Then also symlinks (fallback, override etc.) and constraints for keys outside of the parentKey would work.
  It would make the `-a` option of `kdb get` unnecessary.

@kodebach wrote (in response to the 'symlinks' point):

> You still haven't addressed why this would actually be a good default behavior.
> You just claim it is an advantage without any explanation.
>
> Like I said, I would normally expect mountpoints to be isolated.
> If symlinks work like this, the isolation is partially broken.
> One could argue that the problem right now is that such "broken" symlinks are not prevented.
> But your solution doesn't completely fix the problem either.
>
> Take a variant of my previous example:
>
> - `system:/foo` is a mountpoint, there are no other mountpoints
> - `spec:/foo/bar` refers to `system:/abc` via `meta:/override`
>
> Now in a plugin that is part of the mountpoint `system:/foo` doing a `ksLookupByName (ks, "/foo/bar", 0)` would NOT use `system:/abc`, because that key is never part of the keyset passed to this plugin.
>
> However, in an application that used `system:/foo` as the parent, the `meta:/override` would work with your proposal.
> To me that seems like very confusing behavior, because both the application and the plugin seemingly use the same parent key.


### Data restrictions

@kodebach wrote:

> Make all the keys returned by kdbGet completely read-only.
> To change the data you need to append an entirely new key to replace the existing one.
> Then we just need to keep a shallow copy internally.

### API restrictions

@kodebach wrote:

> Change the API and remove KeySet from kdbGet and kdbSet also option 4 in [the operation sequences decision](../0_drafts/operation_sequences.md).
> If the keyset is owned by the KDB handle, it should not be as big surprise, if there is extra data in there.
> I certainly wouldn't try to asset anything on the contents of a KeySet that I don't own directly, unless the condition is explicitly documented somewhere.

@markus2330 wrote:

> I disagree, it is actually the same kind of surprise for "More Keys".
> Only the "Fewer Keys" would get fixed.

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
  We can not track how many keys point to the same data and name this way, so we can only free data and name if the key does not have the COW flag.
  If the original key gets deleted, using a COW key that points to the same data and name will lead to corrupt data.
  The same is true for updating values of the original key.

  This is only problematic if we want to use COW for keys outside of `KDB`.
  If it is only for use within `KDB`, especially for usage as internal cache and in change tracking, we could always guarantee that the original keys are going to last as long as the `KDB` instance.
  However, we need to document for the users of Elektra that keys returned from `kdbGet` are only valid until `kdbClose`.
  If they want to continue using them afterwards, they'd have to deep copy them.

  Triggering the delete problem:

  ```c
  Key * originalKey;
  Key * copiedKey;
  keyCopy (copiedKey, originalKey, ELEKTRA_CP_COW);

  assert (keyString (copiedKey) == keyString (originalKey));
  assert (keyName (copiedKey) == keyName (originalKey));

  keyDel (originalKey);

  keyString (copiedKey); // Error! Original value has been deleted. Pointer to data in copiedKey points to freed memory
  keyName (copiedKey);   // Error! Original name has been deleted.
  ```

  Triggering the update problem:

  ```c
  Key * originalKey;
  Key * copiedKey;
  keyCopy (copiedKey, originalKey, ELEKTRA_CP_COW);

  assert (keyString(copiedKey) == keyString(originalKey));
  keySetString (originalKey, "new value!");

  keyString(copiedKey); // Error! Original value has been deleted. Pointer to data in copiedKey points to potentially freed memory
  ```

  The same problems in principle exist for `mmapstorage` where `kdbSet` frees (`munmap`) the keyset.
  We can still let users access the flag `ELEKTRA_CP_COW`, we just need to clearly document what is forbidden.
  Maybe set the `KEY_FLAG_RO_VALUE` on the original key, so that the API itself detects the error.
  There is, however, no flag for `keyDel` that we could set.

#### Changes to `libelektra-core`

The `struct _Key` will be extended with two more pointers, if we want to eliminate the lifetime problem:

- `struct _Key * origData`: points to the key containing the referenced data
- `struct _Key * origName`: points to the key containing the referenced name

Those two pointers are needed for memory management.
Each referenced key will also have its reference counter increased.
This way, an original key can be `keyDel()`d without impacting the copied keys.

Three new key flags will be added:

- `KEY_FLAG_COW_VALUE`: the value points to a value of another key
- `KEY_FLAG_COW_NAME`: the name points to the name of another key
- `KEY_FLAG_COW_META`: metakeys are copy-on-write

A new copy flag will be added:

- `KEY_CP_COW`: instructs `keyCopy` to copy whatever it should copy as copy-on-write.
  This will NOT be part of `KEY_CP_ALL`.
  We don't want developers outside of Elektra to accidentally use this.

If `keyCopy()` is instructed to do a copy-on-write copy:

- `dest->data.v` and `dest->data.c` point to the exact same location as in the source.
  `dest->dataSize` is set to the same value as `source->dataSize`.
  `KEY_FLAG_COW_VALUE` is set on `dest->flags`.
  `KEY_FLAG_RO_VALUE` is set on `source->flags`.
  `dest->originalData` is set to `source`.
  `source->refs` is incremented.

- `dest->key` points to `source->key`.
  `dest->keySize` is set to the same value as `source->keySize`.
  `dest->ukey` points to `source->ukey`.
  `dest->keyUSize` is set to the same value as `source->keyUSize`.
  `KEY_FLAG_COW_NAME` is set on `dest->flags`.
  `KEY_FLAG_RO_NAME` is set on `source->flags`.
  `dest->originalName` is set to `source`.
  `source->refs` is incremented.

- `dest->meta` points to a **new** keyset.
  The keys in `dest->meta` are also copied with `KEY_CP_COW`, i.e. they are also copy-on-write keys.
  `KEY_FLAG_COW_META` is set on `dest->flags`.
  `KEY_FLAG_RO_META` is set on `source->flags`.

The source key will remain as a read-only key.
This constraint is needed, because the source key is the only key we can free the resources on.
If the data or the name would change in the source key, all COW-copied keys would suddenly have another value.
For the same reason, the source key will need to live longer than all COW-copied keys from it.

A `keyCopy()` without `KEY_CP_COW` from an COW key will create a deep copy of the key.
These keys are "normal" non-COW keys and can live on their own.

Every `key*()` method that modifies data on a COW-copied key will need to allocate new memory for this data and remove the `KEY_FLAG_COW_DATA` flag.
Every `key*()` method that modifies the name of a COW-copied key will need to allocate new memory for this name and remove the `KEY_FLAG_COW_NAME` flag.
Every `key*()` method that modifies metadata needs to make sure that the same happens for metakeys.

Keysets are not copy-on-write.
A `ksDeepDup()` of a keyset with COW keys will create a keyset with deep-copied non-COW keys.
Internally we may need a `ksCowDup()` function to create a keyset with copy-on-write keys from another keyset.
Whether this function will be part of the public API is a point for discussion.

### Full-blown copy-on-write implementation

Make Elektra's `Key` and `KeySet` data structures copy-on-write.
This requires some major refactoring of code within `libelektra-core`.
Code that does only interact with the data structures via the public `libelektra-core` API should not notice any differences.
The `mmapstorage` plugin needs to be fixed.

Unlike "In-Memory COW Cache" keyDup, keyCopy, ksCopy and ksDup will always use COW.
`ksCopy` and `ksDup` is needed for (de)duplication of metadata.
Furthermore, the API has better usability if Key and KeySet behave the same, especially for bindings where duplication might happen implicitly.

#### Changes to `Key`

For the `Key`, we need to extract everything for the data and name into their own structs.
This is done for memory-management reasons, as we need to track how many keys point to the same data and/or name.

```c
struct _KeyData {
    union {
        char * c;
        void * v;
    } data;

    size_t dataSize;

    uint16_t refs;
};

struct _KeyName {
    char * key;
    size_t keySize;

    char * ukey;
    size_t keyUSize;

    uint16_t refs;
};

struct _Key {
    struct _KeyData * keyData;
    struct _KeyName * keyName;
    KeySet * meta;
    keyflag_t flags;

    uint16_t refs;
};
```

@mpranj's thoughts regarding moving name and data to separate structures:

> 1. If they [key name and data] are a separate entity, `mmapstorage` will need a flag once again for each of those. 
>    This is used to mark whether the data is in an mmap region or not. (or we find some bit somewhere that we can steal for this purpose)
>
> 2. Adding more indirections is probably not going to help performance. (I understand that we save memory here)

#### Changes to `KeySet`

For `KeySet`, we need to split out everything to do with the stored keys into a separate datastructure.
This includes the array itself, the sizes and the hashmap.

Why don't we just add the number of references to the original `KeySet`?

- If we delete a copied KeySet, we don't know which KeySet is the original, so we couldn't decrement the counter.
  This could be dealt with storing a pointer to the original KeySet.
- If the original KeySet is deleted, we don't know which other KeySets point at the data, so updating their count would not work
- In similar fashion, if you update the original KeySet, the copied KeySets will also contain the new data (if the memory address does not change).
  This is unexpected behavior.

```c
struct _KeySetData {
    struct _Key ** array;
    size_t size;  /**< Number of keys contained in the KeySet */
    size_t alloc; /**< Allocated size of array */

    Opmphm * opmphm;
    OpmphmPredictor * opmphmPredictor;

    uint16_t refs; /**< Reference counter */
};

struct _KeySet {
    struct _KeySetData * data;

    ksflag_t flags;

    uint16_t refs; /**< Reference counter */
};
```

API notes:

- `ksNew()`:
  ```
  if size is 0, `data` will be NULL
  ```
- `ksDup()`: creates a new instance of `struct _KeySet`, points its `data` to the `source->data`, increases `source->data->refs` by 1.

- `ksCopy(dest, source)`:
  ```
  if `dest->data` is NULL, then point `dest->data` to `source->data` and increase `source->data->refs` by 1
  if `dest->data` is NOT NULL:
    if `dest->data->refs` == 0 (meaning no other keysets points to it):
      deallocate `dest->data`
      point `dest->data` to `source->data`, increase `source->data->refs` by 1.
    else if `dest->data->refs` > 0: (there are other keysets sharing the same data):
      decrease `dest->data->refs` by 1.
      allocate new `dest->data`
      copy the keys from old to `dest->data`
      copy the keys of `source->data->array` over to `dest->data->array`
  ```
- `ksAppend(dest, source)`:
  ```
  if `dest->data` is NULL, then point `dest->data` to `source->data` and increase `source->data->refs` by 1
  if `dest->data` is NOT NULL:
    if `dest->data->refs` == 0 (meaning no other keysets points to it):
      if `dest->data->size` == 0 (no keys are in this keyset):
        deallocate `dest->data` and point `dest->data` to `source->data`, increase `source->data->refs` by 1.
      else if `dest->data->size` > 0 (meaning there are already keys in this keyset):
        copy the keys of `source->data->array` over to `dest->data->array`
    else if `dest->data->refs` > 0: (there are other keysets sharing the same data):
      if `dest->data->size` == 0 (no keys are in this keyset):
        decrease `dest->data->refs` by 1.
        point `dest->data` to `source->data`, increase `source->data->refs` by 1.
      else if `dest->data->size` > 0 (meaning there are already keys in this keyset):
        decrease `dest->data->refs` by 1.
        allocate new `dest->data`
        copy the keys from old to `dest->data`
        copy the keys of `source->data->array` over to `dest->data->array`
  ```
- `ksAppendKey(dest, key)`:

  - Pretty much the same `ksAppend()` but only for a single key
  - if `dest->data` is NULL, allocate `dest->data` and put the key there

- `ksClear(dest)`:

  ```
  if `dest->data->refs` == 0 (meaning no other keysets points to it):
    deletes the keys in `dest->data`
  else if `dest->data->refs` > 0: (there are other keysets sharing the same data):
    decrease `dest->data->refs` by 1
    point `dest->data` to NULL
  ```

- `ksCut(dest, key)`:
  ```
  if `dest->data->refs` == 0 (meaning no other keysets points to it):
    perform the cutting algorithm directly on these keys
  else if `dest->data->refs` > 0: (there are other keysets sharing the same data):
    decrease `dest->data->refs` by 1
    create a new `dest->data`
    copy over the non-cut keys into `dest->data` from the old `dest->data`
  ```
- `ksPop(dest)`:

  ```
  if `dest->data->refs` == 0 (meaning no other keysets points to it):
    pop the last key directly on the keys
  else if `dest->data->refs` > 0: (there are other keysets sharing the same data):
    decrease `dest->data->refs` by 1
    create a new `dest->data`
    copy over all but the last keys from the old `dest->data`
  ```

- `ksLookup()`:
  - if the `DEL` or `POP` flag is specified, do the COW-stuff as described multiple times now in the operations above

#### Optimizations

This approach requires more allocation than previously.
We have not benchmarked whether this is big of an issue.
One optimization could be an expanding "pool" of `_KeySetData`, `_KeyData` and `_KeyName`.
We could then allocate multiple of them at the same time, and borrow and give back instance from and to the pool.

## Memory comparison of COW approaches

The following calculations are based on the AMD64 platform.
All results are in bytes unless stated otherwise.

Example key: `user:/hosts/ipv6/example.com = 2606:2800:220:1:248:1893:25c8:1946`

We want to measure the following properties:

- Empty KeySet: size of a simple malloc of the keyset struct
- Empty Key: size of a simple malloc of the key struct
- Empty Key (with name): size of simple malloc of all structs, so that the key has a name, but without including the size of the name
- Empty Key (with name + data): size of a simple malloc of all structs, so that the key has a name and data, but without including the size of the name or data
- Single Example Key: a single instance of the key defined above
- Example Key + 1 Duplicate: two instances of the key defined above, one of them is a duplication of the first
- Example Key + 2 Duplicates: three instances of the key defined above, two of them are duplications of the first

| Approach                                          | Empty KeySet | Empty Key | Empty Key (with name) | Empty Key (with name + data) | Single Example Key | Example Key + 1 Duplicate | Example Key + 2 Duplicates |
|:--------------------------------------------------|-------------:|----------:|----------------------:|-----------------------------:|-------------------:|--------------------------:|---------------------------:|
| Current Implementation                            |           64 |        64 |                    64 |                           64 |                153 |                       306 |                        459 |
| In-Memory COW cache (without additional pointers) |           64 |        64 |                    64 |                           64 |                153 |                       217 |                        281 |
| In-Memory COW cache (with additional pointers)    |           64 |        80 |                    80 |                           80 |                169 |                       249 |                        329 |
| Full-blown COW implementation                     |           16 |        32 |                    72 |                           96 |                185 |                       217 |                        249 |

### Calculations

Raw data size:

- keyname : 28 + 1 = 29
- unescaped keyname (measured): 25
- data: 34 + 1 = 35

Current Implementation:

- Empty KeySet [measured via `sizeof`]: 64
- Empty Key [measured via `sizeof`]: 64
- Empty Key (with name): 64
- Empty Key (with name + data): 64
- Single Example Key = Empty Key + keyname + unescaped keyname + data = 64 + 29 + 25 + 35 = 153
- Single Example Key + 1 Duplicate = Single Example Key * 2 = 153 * 2 = 306
- Single Example Key + 2 Duplicates = Single Example Key * 3 = 153 * 3 = 459

In-Memory COW cache (without additional pointers):

- Empty KeySet [measured via `sizeof`]: 64
- Empty Key [measured via `sizeof`]: 64
- Empty Key (with name): 64
- Empty Key (with name + data): 64
- Single Example Key = Empty Key + keyname + unescaped keyname + data = 64 + 29 + 25 + 35 = 153
- Single Example Key + 1 Duplicate = Single Example Key + Empty Key = 153 + 64 = 217
- Single Example Key + 2 Duplicates = Single Example Key + Empty Key * 2 = 153 + 64*2 = 281

In-Memory COW cache (with additional pointers):

- Empty KeySet [measured via `sizeof`]: 64
- Empty Key [measured via `sizeof`]: 80
- Empty Key (with name): 80
- Empty Key (with name + data): 80S
- Single Example Key = Empty Key + keyname + unescaped keyname + data = 80 + 29 + 25 + 35 = 169
- Single Example Key + 1 Duplicate = Single Example Key + Empty Key = 169 + 80 = 249
- Single Example Key + 2 Duplicates = Single Example Key + Empty Key * 2 = 169 + 80*2 = 329

Full-blown COW implementation:

- Empty KeySet [measured via `sizeof`]: 16
- Empty Key [measured via `sizeof`]: 32
- Empty Key (with name) [measured via `sizeof`]: Empty Key + sizeof(KeyName) = 32 + 40 = 72
- Empty Key (with name + data) [measured via `sizeof`]: Empty Key + sizeof(KeyName) + sizeof(KeyData) = 32 + 40 + 24 = 96
- Single Example Key = Empty Key (with name + data) + keyname + unescaped keyname + data = 96 + 29 + 25 + 35 = 185
- Single Example Key + 1 Duplicate = Single Example Key + Empty Key = 185 + 32 = 217
- Single Example Key + 2 Duplicates = Single Example Key + Empty Key * 2 = 185 + 32*2 = 249

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
