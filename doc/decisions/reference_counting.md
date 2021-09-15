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

- add second reference counter to Key
- One counter is for locking references, the other one for general references. A locking reference automatically locks/unlocks the keyname.
- introduce reference counter for KeySets (for external keyset references)
- `keyBorrow` returns an error in case of reference counter overflow
- use fixed sized types for reference counters
- increment/decrement references before/after passing instances to plugins

## Rationale

- Adding a second reference counter to Key and reducing the size of both significantly (`size_t` to `uint16_t`)
  actually saves memory (32 vs 64bit on 64-bit machines) compared to the previous solution.
- The added complexity of maintaining two reference counters is worth the
  tradeoff for the gained functionality.

## Implications

## Related Decisions

## Notes
