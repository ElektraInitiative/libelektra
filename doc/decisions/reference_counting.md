# Reference Counting

## Problem

- locking is not reset when ref counting again gets 0 (adding to keyset and
  pop again) #2202
- C++ API for KeySet and Key has unexpected differences: also use ref counting
  for KeySets (also suggested in #1332)

## Constraints

## Assumptions

## Considered Alternatives

- make ref counting thread safe (probably useful for JNI)
- start with 1 for reference counting and let keyDecRef do keyDel

## Decision

- add second counter to Key
- One counter is for references, the other one is for locking the keyname. The keyname is thereby protected with a re-entrant lock.
- introduce reference counter for KeySets (for external keyset references, e.g. in bindings)
- limit number of references to `UINT16_MAX - 1` and use `UINT16_MAX` as an error value
- return error on reference count overflow
- no error on underflow (decrement when zero), instead stay at zero
- use fixed sized types (`uint16_t`) for reference counters
- increment/decrement references before/after passing instances to plugins

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
