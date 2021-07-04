# Reference Counting

## Problem

- locking is not reset when ref counting again gets 0 (adding to keyset and pop again) #2202
- C++ API for KeySet and Key has unexpected differences: also use ref counting for KeySets (also suggested in #1332)

## Constraints

## Assumptions

## Considered Alternatives

- make ref counting thread safe (probably useful for JNI)
- start with 1 for reference counting and let keyDecRef do keyDel (remove ksDel&keyDel; or remove keyDecRef, rename keyIncRef to keyBorrow (or similar) and keyDecRef to keyDel) #1332

## Decision

Instead of renaming `keyIncRef` to `keyBorrow`, I would do this:

```c
/**
  * @return a new reference to k
  */
Key * keyBorrow (Key * k) {
   keyIncRef (k);
   return k;
}
```

`keyIncRef` and `keyDecRef` would be internal/private/static (whatever fits). The same `ks*` functions should exist for `KeySet`.

## Rationale

The end effect is basically the same. Users should

- use `keyNew` and `keyCopy`/`keyDup` to create entirely new `Key *` that don't have any links to other `Key *`.
- use `keyBorrow` to create a new `Key *` that is a reference an existing `Key *`.
- use `keyDel` to dispose of a `Key *` (whether the `Key *` comes from `keyNew`, `keyCopy`/`keyDup` or `keyBorrow`).

## Implications

## Related Decisions

## Notes
