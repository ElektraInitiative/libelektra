# Readonly Keynames

## Problem

It might be a good idea to prevent changes to the keyname after creation.
Currently, the name of a key is automatically made read-only, when it is currently part of or at some point in the past was part of at least one `KeySet`.
There are discussions about making key names writable again, when the key is removed from all `KeySet`s.

There may also be situations where changing the name of a key after its creation is required.
In some situations, it may be wise to reuse a key instead of deleting it and creating a new one.

## Constraints

1. `keyAddName` et al. still have to work up to a certain point
2.
3.

## Assumptions

1.
2.
3.

## Considered Alternatives

### Separate API for keynames

Use the proposed `ElektraBuffer` struct to create a separate API for keynames independent of the `Key` API.

### Read-only keynames

The key name should be permanently read-only after creation.

TBD: How would we dynamically create keynames with this approach?

### Re-entrant lock for the key name

Since we assume a single threaded context, this can be implemented as a simple counter.

```c
struct _Key {
    // [...] other stuff
   uint16_t nameLock; // if zero, name is writable, otherwise name is readonly
};

void keyLockName (Key * k) {
    k->nameLock++;
}

void keyUnlockName (Key * k) {
    if (k->nameLock == 0) return;
    k->nameLock--;
}

void ksAppendKey (KeySet * ks, Key * k) {
    keyLockName (k);
    // [...]
}

void ksRemove (KeySet * ks, elektraCursor cursor) {
    keyUnlockName (ks->array[cursor]);
    // [...]
}
```

### Alternative C

## Decision

## Rationale

## Implications

-
-
-

## Related Decisions

- []()
- []()
- []()

## Notes

- [Issue 2202](https://issues.libelektra.org/2202) talks about how unexpected it is that keys will be readonly once in a keyset, but they don't get unlocked when removing them from a keyset.
