# Use Case: `Key` Hierarchy

## Summary

- **Title:** Create `Key`
- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Hierarchy comparison between `Key`'s

## Scenarios

- **Precondition:**
  - [`Key` K1 has been created](UC_key_create.md)
  - [`Key` K2 has been created](UC_key_create.md)
- **Main success scenario:**
  - Caller [reads names](UC_key_name.md) N1 and N2 of `Key` K1 and `Key` K2
  - Caller requests hierarchy comparison between N1 and N2
  - Core returns
    - negative value indicating N1 and N2 do not form a hierarchy
    - zero indicating N1 is the same as N2
    - one indicating N2 is a direct child of N1 in the hierarchy
    - another positive value indicating N2 is a descendant of N1 in the hierarchy
- **Alternative scenario:** -
- **Error scenario:** -
- **Postcondition:** -
- **Non-functional Constraints:**
  - The hierarchy comparison with `result >= 0` MUST form a _partial order_ over `Key`s.
