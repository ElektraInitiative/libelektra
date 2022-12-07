# Use Case: `Key` Ordering

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Order comparison between `Key`s

## Scenarios

- **Precondition:**
  - [`Key` K1 has been created](UC_key_create.md)
  - [`Key` K2 has been created](UC_key_create.md)
- **Main success scenario:**
  - Caller [reads names](UC_key_name.md) N1 and N2 of `Key` K1 and `Key` K2
  - Caller requests order comparison between N1 and N2
  - Core returns
    - N1 is sorted before N2
    - N1 and N2 are sorted to the same place
    - N1 is sorted after N2
- **Alternative scenario:** -
- **Error scenario:** -
- **Postcondition:** -
- **Non-functional Constraints:**
  - The order comparison with MUST form a _total order_ over `Key`s.
  - Each of the possible results MAY be a single value, but it MAY also be a whole class of value (e.g. negative integer).
