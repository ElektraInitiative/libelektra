# Reference Counting

## Problem

Elektra's data structures are shared between multiple owners.
A `Key` can be owned by many different `KeySet`s.
Both `Key` and `KeySet` can be used in bindings.
If the languages of those bindings have automatic memory management, this also implies a shared ownership.

To solve this issue of shared ownership, we need reference counting.

An adjacent problem is that the names of `Key`s must be read-only while the `Key` is part of a `KeySet`.
This can also be solved by a secondary counter just for `KeySet`s.

There has also been [discussion](copy_on_write.md) about adding further data structures, which would need reference counters as well.

## Constraints

- Deleting data structures that still have references must be prevented
- The APIs for `ksAppendKey` and `ksNew` must remain ergonomic.
  The reference counting must not prevent the inline creation of a `Key` while adding it to a `KeySet`.

## Assumptions

- The reference counter doesn't need to be thread-safe.
- A reference counter of size `uint16_t` is large enough for all use cases.
  Further restricting the counter by a small amount to allow for error value is also acceptable.

## Considered Alternatives

### Counted deletion lock

See decision

### Retain/Release reference counting

One of the most common and simplest approaches to reference counting is:

```c
struct rc_data {
  void * data;
  uint16_t count;
};

// used to create a new reference counted structure
struct rc_data * rc_create(void * data) {
  struct rc_data * rc = malloc(sizeof(struct rc_data));
  rc->data = data;
  rc->count = 1;
  return;
}

// add new reference
void rc_retain(struct rc_data * rc) {
  rc->count++;
}

// remove reference and delete data when last reference is removed
void rc_release(struct rc_data * rc) {
  rc->count--;
  if (rc->count == 0) {
    free(rc);
  }
}
```

In this basic form, the `void * data` must be created first and then wrapped in the reference counting structure.
To avoid unnecessary allocations there are a few different options.

The easiest way to avoid the allocation is to not have a separate structure for reference counting.
Instead, the reference counter is embedded into every structure that needs one.
This requires duplicating the retain/release functions for every structure.
For a small number of structures this acceptable, but for larger numbers it becomes error prone.

One way to solve the duplication of reference counting functions is by having macros for the reference counting functions.

### Retain/Release/Free reference counting

A variation of the retain/release approach, which accounts for different functions for deletion is this:

```c
// Source: https://nullprogram.com/blog/2015/02/17/

struct ref {
    void (*free)(const struct ref *);
    int count;
};

static inline void
ref_inc(const struct ref *ref)
{
    ((struct ref *)ref)->count++;
}

static inline void
ref_dec(const struct ref *ref)
{
    if (--((struct ref *)ref)->count == 0)
        ref->free(ref);
}
```

This structure is not meant to be allocated separately, instead it should be embedded like this:

```c
typedef struct Key {
    // [...] other stuff
    struct ref refcount;
} Key;
```

Now to create a `Key`, we need a function that takes a `const struct ref *` and frees the `Key` that we can put into `struct ref`.

```c
// Also used in Linux Kernel
// See: https://radek.io/2012/11/10/magical-container_of-macro/
#define container_of(ptr, type, member) ((type *)((char *)(ptr) - offsetof(type, member)))

static void keyFree(const struct ref *ref)
{
    Key * key = container_of(ref, struct Key, refcount);
    // [...] other cleanup
    free(key);
}
```

This function would only be private.
The public API would use functions like this:

```c
Key * keyCreate() {
  Key * key = malloc(sizeof(struct Key));
  // [...] other initialization
  key->refcount = (struct ref){keyFree, 1};
  return key;
}

void keyRetain(Key * key) {
  ref_inc(key->refcount);
}

void keyRelease(Key * key) {
  ref_dec(key->refcount);
}
```

These functions are needed to keep `struct Key` opaque.

A big advantage here would be that things like `mmapstorage` that do not use normal heap allocations, could set their own `free` pointer in `struct ref`.

### Reference counting in allocator

While we cannot have automatic reference counting (ARC), we could implement most parts of ARC other than the automatic counting inserted by a compiler.
In an ARC system, every heap memory allocation has a reference count, which is often stored as a hidden header.
Essentially, instead of calling `malloc(data_size)` the system does `malloc(header_size+data_size)` and uses the additional `header_size` bytes to store data like the reference count.
Crucially, the pointer returned to the caller still points after the header data, which is therefore hidden.

Since (almost) all allocations for Elektra's data structures go through `elektraMalloc`, we could implement something similar.
In `elektraMalloc` we allocate a few extra bytes (for alignment reasons this would be `sizeof(size_t)`) and use them for reference counting.
The reference counting would be done via generic `elektraRetain` and `elektraRelease` functions.
These functions increment and decrement the counter in the hidden header.
Additionally, `elektraRelease` would also call `free` when the count reaches zero.

Like in the "Retain/Release/Free" option above, the hidden header data could also be used to store a pointer to a `free` function.
Then things like `mmapstorage` could be covered as well.

```c
// Based on: https://codereview.stackexchange.com/questions/146561/reference-counting-in-c99

// Note: should NOT call free() on pointer
typedef void (*Destructor)(const void*);

typedef struct {
  Destructor destroy;
  unsigned int count;
} ObjectRecord;

static ObjectRecord *toRecord(void *object) { return (ObjectRecord*)(object) - 1; }
static void *fromRecord(ObjectRecord *record) { return record + 1; }

void * elektraMalloc(size_t size, Destructor destroy) {
  ObjectRecord *record = malloc (sizeof (ObjectRecord) + size);
  if (record == NULL) {
    return NULL;
  }
  record->count = 1;
  record->destroy = destroy;
  return recordToObject (record);
}

unsigned int elektraRefCount(const void * p) {
  return objectToRecord (p)->count;
}

void elektraRetain(const void *p)
{
    ++objectToRecord(p)->count;
}

void elektraRelease(const void *p)
{
    ObjectRecord *record = objectToRecord(p);

    record->count--;
    if (record->count > 0) {
        /* still referenced */
        return;
    }

    if (record->destory) {
        record->destory(p);
    }
    free(record);
}
```

Usage example:

```c
static void ksDestroy(const void * p) {
  KeySet * ks = (KeySet *)p;
  for (size_t i = 0; i < ks->size; i++) {
    elektraRelease(key);
  }
  // [...]
}

KeySet * ksCreate() {
  KeySet * ks = elektraMalloc(sizeof(struct KeySet), ksDestroy);
  // [...] initalization
  return ks;
}

void ksAppendKey(KeySet * ks, Key * key) {
  elektraRetain(key);
  // [...]
}

// for compatibility and for complete API
static inline void ksDel(KeySet * ks) {
  elektraRelease(ks);
}
```

## Decision

General decisions regardless of implementation:

- `Key` has two counters.
  One that blocks deletion, another to prevent name changes.
- Reference counts are of type `uint16_t`.

### Counted deletion lock

The reference counting consists of two functions `inc` and `dec`.
When a structure is created, its reference count is set to `0`.
Only in this state can the structure be deleted.
If the count is non-zero, deletion is no-op.
The `inc` and `dec` function increment and decrement this count.

Therefore, the reference counter can be considered a reentrant/counting lock, which blocks the deletion of a structure.
At zero the lock is unlocked, any calls to `inc` lock the lock one more time, and calls to `dec` unlock once.

The secondary counter for the name of a `Key` works the same way, but it blocks changes to the name.

Additional details:

- `inc` returns an error on reference count overflow
- `dec` does not return an error on underflow, instead the count stays at zero

## Rationale

- Adding a second reference counter to Key and reducing the size of both significantly (`size_t` to `uint16_t`)
  actually saves memory (32 vs 64bit on 64-bit machines) compared to the previous solution.
- The added complexity of maintaining two reference counters is worth the trade-off for the gained functionality.

## Implications

## Related Decisions

## Notes

Not implemented yet:

- Update bindings to use KeySet reference counter (especially C++)
- Second counter for automatic keyname (un)locking
