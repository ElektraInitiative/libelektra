# Copy On Write

## Problem

One of Elektra's core goals is low memory usage.
Currently, there are many places within Elektra where keys and keysets are duplicated and copied around.
Most of those copied keys are never modified, but are required to be detached from the lifetime of the original instance.
We want to introduce an in-memory copy-on-write mechanism to lower our memory usage.

In the near future, Elektra will also gain facilities for [change tracking](../1_in_discussion/change_tracking.md) and session recording, both of which will potentially again duplicate keys.
There are also aspirations to create a new, simple [internal cache](../3_decided/internal_cache.md) that would also benefit from a copy-on-write mechanism.

## Constraints

1. The lifetime of a `Key` and a `KeySet` must be unaffected by copy-on-write.

## Assumptions

## Considered Alternatives

### `mmapstorage`-like copy-on-write implementation

There is already some kind of copy-on-write semantics within `libelektra-core` to support the `mmapstorage` plugin.
We can build on this and add a more generic copy-on-write to it.

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
The `mmapstorage` plugin needs to be updated.

Unlike "mmapstorage-like COW implementation" keyDup, keyCopy, ksCopy and ksDup will always use COW.
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

#### Reference Counting

We need reference counting for the internal COW datastructures.
We do it the same way reference counting currently works for `Key` and `KeySet`.
One tweak though is that the refcount should never be 0, as this does not make sense for internal datastructures.

This means we always increment the refcount after creation and always decrement before deletion, so that the refcount is never zero.
An example implementation is shown below:

```c
static void keySetValue(Key * key, void * value, size_t size) {
  // [...] removal of current value from key
  struct _KeyData data = keyDataNew (value, size);
  keyDataIncRef (data);
  key->data = data;
}

static void keyDel(Key * key) {
  keyDataDecRef (key->data);
  keyDataDel (key->data);
  // [...] other cleanup
}
```

#### Variation 1 - RcBuffer

Instead of using different structs for `_KeyData`, and `_KeyName` use a more generic struct for reference counting.
This would avoid some duplication on the reference counting code for the key.
Keysets will still have their own data struct, as it contains more than just a pointer and a size.

```c
typedef struct {
	void * data;
	size_t size;
	uint16_t refs;
} RcBuffer;

struct _Key {
	RcBuffer * uname;
	RcBuffer * ename; // will be removed soon
	RcBuffer * value;

	KeySet * meta;
	keyflag_t flags;
	uint16_t refs;
};
```

#### Possible Edge Cases

In general, it should be possible to always do copy-on-write.
From a users perspective, copy-on-write copies of a key (and a keyset) should behave the same.
There is, however, one edge case: the user modifying the value of a key directly.
This is shown in the following example:

```c
Key * key;
struct foo myFoo = {
  .x = 0
};
keySetBinary (key, &myFoo, sizeof(myFoo));

Key * dup = keyDup (key);

((struct foo *)keyValue (key))->x = 1;

// with COW
assert (((struct foo *)keyValue (dup))->x == 1);
// without COW
assert (((struct foo *)keyValue (dup))->x == 0);
```

This edge case can be accounted for by providing a private function `keyDetach`, that forces that the key has its very own copy of the data.

```c
((struct foo *)keyValue (keyDetach(key)))->x = 1;

// with COW
assert (((struct foo *)keyValue (dup))->x == 0);
// without COW
assert (((struct foo *)keyValue (dup))->x == 0);
```

#### Compatibility with `mmapstorage` plugin

If we do change the internal data structures it makes much more sense to fix the cache and mmapstorage afterwards (or in tandem).
The most important constraint for mmap is that any structure (or bytes) that is an allocation unit (e.g. we malloc() the bytes needed for KeySet struct, so this is an unit) needs to have a flag to determine whether those bytes are actually malloc()ed or they are mmap()ed.
Thus all the newly added structures as proposed will need some kind of an mmap flag.

`mmapstorage` only calls `munmap` in some error cases, so basically `munmap` is almost never done and the keyset is never invalidated.

During `kdbSet` the storage plugins always write to a temp file, due to how the resolver works.
We also don't need to mmap the temp file here: when doing `kdbSet` we already have the `KeySet` at hand, mmap-ing it is not needed at all, because we have the data.
We just want to update the cache file.
The `mmap`/`munmap` in kdbSet are just so we can write the KeySet to a file in our format.
(`mmap()` is just simpler, but we could also `malloc()` a region and then `fwrite()` the stuff)

Therefore the only case where we return a `mmap()`ed KeySet should be in `kdbGet`.

When the `mmapstorage` was designed/implemented, not all structures had refcounters, so there was no way to know when a `munmap` is safe.
This was simply out of scope at that point in time.

If refcounting is now implemented for all structures, we might be able to properly `munmap` in future.

Two ideas to deal with this in conjunction with our reference counting implementation:

If we have `free` function-pointer along side the refcount, `mmapstorage` (and also other plugins with different allocators) could set it to their own implementation.
To mimic the current behavior of `mmapstorage` this would point to a no-op function.
However, we could also improve things and keep track of when all data has been freed and only then call `munmap`.

Another simpler way to avoid the flag, which doesn't really allow for further improvements, would be using the refcount.
`mmapstorage` could set the refcount to a value that is otherwise illegal.
This would allow us to detect the keys.
Depending on the refcount implementation good values would probably be 0 or UINT16_MAX.
The special value would have to ignored by all refcounting functions (inc, dec, del) and turn the functions into no-ops.

#### Possible Optimizations

- This approach requires more allocations than previously.
  We have not fully benchmarked whether this is a big issue.
  One optimization could be an expanding "pool" of `_KeySetData`, `_KeyData` and `_KeyName`.
  We could then allocate multiple of them at the same time, and borrow and give back instance from and to the pool.

- Embed the `KeySet * meta` directly in `struct _Key`.
  This may help with performance in cases we need metadata.
  It will, however, increase memory usage.
  This should only be considered after some benchmarking shows this is a real issue.

## Memory comparison of COW approaches

The following calculations are based on the AMD64 platform.
All results are in bytes unless stated otherwise.

Example key: `user:/hosts/ipv6/example.com = 2606:2800:220:1:248:1893:25c8:1946`

We want to measure the following properties for the key:

- Empty Key: size of a simple malloc of the key struct
- Empty Key (with name): size of simple malloc of all structs, so that the key has a name, but without including the size of the name
- Empty Key (with name + data): size of a simple malloc of all structs, so that the key has a name and data, but without including the size of the name or data
- Single Example Key: a single instance of the key defined above
- Example Key + 1 Duplicate: two instances of the key defined above, one of them is a duplication of the first
- Example Key + 2 Duplicates: three instances of the key defined above, two of them are duplications of the first

| Approach                                                          | Empty Key | Empty Key (with name) | Empty Key (with name + data) | Single Example Key | Example Key + 1 Duplicate | Example Key + 2 Duplicates |
| :---------------------------------------------------------------- | --------: | --------------------: | ---------------------------: | -----------------: | ------------------------: | -------------------------: |
| Current Implementation                                            |        64 |                    64 |                           64 |                153 |                       306 |                        459 |
| mmapstorage-like COW implementation (without additional pointers) |        64 |                    64 |                           64 |                153 |                       217 |                        281 |
| mmapstorage-like COW implementation (with additional pointers)    |        80 |                    80 |                           80 |                169 |                       249 |                        329 |
| Full-blown COW implementation                                     |        32 |                    72 |                           96 |                185 |                       217 |                        249 |
| Full-blown COW implementation - Variant 1 (RcBuffer)              |        40 |                    88 |                          112 |                201 |                       241 |                        281 |

We want to measure the following properties for the keyset:

- Empty KeySet: size of a simple malloc of the keyset struct
- Empty KeySet (with data): size of a simple malloc of all structs
- Example KeySet: size of a keyset with 15 keys + NULL byte
- Example KeySet + 1 Duplicate: two instance of Example KeySet, one of them is a duplication
- Example KeySet + 2 Duplicates: three instances of Example KeySet, two of them are duplications

| Approach                                                          | Empty KeySet | Empty KeySet (with data) | Example KeySet | Example KeySet + 1 Duplicate | Example KeySet + 2 Duplicates |
| :---------------------------------------------------------------- | -----------: | -----------------------: | -------------: | ---------------------------: | ----------------------------: |
| Current Implementation                                            |           64 |                       64 |            192 |                          384 |                           576 |
| mmapstorage-like COW implementation (without additional pointers) |           64 |                       64 |            192 |                          384 |                           576 |
| mmapstorage-like COW implementation (with additional pointers)    |           64 |                       64 |            192 |                          384 |                           576 |
| Full-blown COW implementation                                     |           16 |                       64 |            192 |                          208 |                           224 |

### Calculations

Raw data size:

- keyname : `28 + 1` = `29`
- unescaped keyname (measured): `25`
- data: `34 + 1` = `35`

Current Implementation:

- Empty Key [measured via `sizeof`]: `64`
- Empty Key (with name): `64`
- Empty Key (with name + data): `64`
- Single Example Key = `Empty Key + keyname + unescaped keyname + data` = `64 + 29 + 25 + 35` = `153`
- Single Example Key + 1 Duplicate = `Single Example Key * 2` = `153 * 2` = `306`
- Single Example Key + 2 Duplicates = `Single Example Key * 3` = `153 * 3` = `459`

- Empty KeySet [measured via `sizeof`]: `64`
- Empty KeySet (with data): `64`
- Example KeySet: `Empty KeySet (with data) + 16 * pointer to keys` = `64 + 16 * 8` = `192`
- Example KeySet + 1 Duplicate: `Example KeySet * 2` = `192 * 2` = `384`
- Example KeySet + 2 Duplicates: `Example KeySet * 3` = `192 * 3` = `576`

mmapstorage-like COW implementation (without additional pointers):

- Empty Key [measured via `sizeof`]: `64`
- Empty Key (with name): `64`
- Empty Key (with name + data): `64`
- Single Example Key = `Empty Key + keyname + unescaped keyname + data` = `64 + 29 + 25 + 35` = `153`
- Single Example Key + 1 Duplicate = `Single Example Key + Empty Key` = `153 + 64` = `217`
- Single Example Key + 2 Duplicates = `Single Example Key + Empty Key * 2` = `153 + 64 * 2` = `281`

- KeySets are not COW in this approach --> same as current implementation

mmapstorage-like COW implementation (with additional pointers):

- Empty KeySet [measured via `sizeof`]: `64`
- Empty Key [measured via `sizeof`]: `80`
- Empty Key (with name): `80`
- Empty Key (with name + data): `80`
- Single Example Key = `Empty Key + keyname + unescaped keyname + data` = `80 + 29 + 25 + 35` = `169`
- Single Example Key + 1 Duplicate = `Single Example Key + Empty Key` = `169 + 80` = `249`
- Single Example Key + 2 Duplicates = `Single Example Key + Empty Key * 2` = `169 + 80 * 2` = `329`

- KeySets are not COW in this approach --> same as current implementation

Full-blown COW implementation:

- Empty Key [measured via `sizeof`]: `32`
- Empty Key (with name) [measured via `sizeof`]: `Empty Key + sizeof(KeyName)` = `32 + 40` = `72`
- Empty Key (with name + data) [measured via `sizeof`]: `Empty Key + sizeof(KeyName) + sizeof(KeyData)` = `32 + 40 + 24` = `96`
- Single Example Key = `Empty Key (with name + data) + keyname + unescaped keyname + data` = `96 + 29 + 25 + 35` = `185`
- Single Example Key + 1 Duplicate = `Single Example Key + Empty Key` = `185 + 32` = `217`
- Single Example Key + 2 Duplicates = `Single Example Key + Empty Key * 2` = `185 + 32 * 2` = `249`

- Empty KeySet [measured via `sizeof`]: `16`
- Empty KeySet (with data): `Empty KeySet + sizeof(KeySetData)` = `16 + 48` = `64`
- Example KeySet: `Empty KeySet (with data) + 16 * pointer to keys` = `64 + 16 * 8` = `192`
- Example KeySet + 1 Duplicate: `Example KeySet + Empty KeySet` = `192 + 16` = `208`
- Example KeySet + 2 Duplicates: `Example KeySet + Empty KeySet * 2` = `192 + 16 * 2` = `224`

Full-blown COW implementation - Variant 1 (RcBuffer):

- Empty Key [measured via `sizeof`]: `40`
- Empty Key (with name) [measured via `sizeof`]: `Empty Key + sizeof(RcBuffer)*2` = `40 + 24*2` = `88`
- Empty Key (with name + data) [measured via `sizeof`]: `Empty Key + sizeof(RcBuffer)*3` = `40 + 24*3` = `112`
- Single Example Key = `Empty Key (with name + data) + keyname + unescaped keyname + data` = `112 + 29 + 25 + 35` = `201`
- Single Example Key + 1 Duplicate = `Single Example Key + Empty Key` = `201 + 40` = `241`
- Single Example Key + 2 Duplicates = `Single Example Key + Empty Key * 2` = `201 + 40 * 2` = `281`

- Empty KeySet [measured via `sizeof`]: `16`
- Empty KeySet (with data): `Empty KeySet + sizeof(KeySetData)` = `16 + 48` = `64`
- Example KeySet: `Empty KeySet (with data) + 16 * pointer to keys` = `64 + 16 * 8` = `192`
- Example KeySet + 1 Duplicate: `Example KeySet + Empty KeySet` = `192 + 16` = `208`
- Example KeySet + 2 Duplicates: `Example KeySet + Empty KeySet * 2` = `192 + 16 * 2` = `224`

### Allocations & Indirections comparison of COW approaches

For allocations want to measure the following properties:

- Empty key: how many objects to allocate for an empty key
- Empty Key (with name): how many objects to allocate for an empty key + name
- Empty Key (with name + data): how many objects to allocate for an empty key + name + data
- Duplication: how many objects to allocate for a duplication
- Key + 1 Duplication: how many objects to allocate for a full key + 1 duplication
- Key + 2 Duplications: how many objects to allocate for a full key + 2 duplications

| Approach                                                          | Empty Key | Empty Key (with name) | Empty Key (with name + data) | Duplication | Key + 1 Duplication | Key + 2 Duplications |
| :---------------------------------------------------------------- | --------: | --------------------: | ---------------------------: | ----------: | ------------------: | -------------------: |
| Current Implementation                                            |         1 |                     1 |                            1 |           1 |                   2 |                    3 |
| mmapstorage-like COW implementation (without additional pointers) |         1 |                     1 |                            1 |           1 |                   2 |                    3 |
| mmapstorage-like COW implementation (with additional pointers)    |         1 |                     1 |                            1 |           1 |                   2 |                    3 |
| Full-blown COW implementation                                     |         1 |                     2 |                            3 |           1 |                   4 |                    5 |

## Decision

Implement the full-blown COW approach.

## Rationale

- It is the most versatile option.
- No restrictions on the lifetime of `Key` and `KeySet` objects.
- Completely transparent to developers using Elektra's public API.

## Implications

- The `mmapstorage` plugins needs to be updated

## Related Decisions

- [change tracking](../1_in_discussion/change_tracking.md)
- [internal cache](../3_decided/internal_cache.md)

## Notes
