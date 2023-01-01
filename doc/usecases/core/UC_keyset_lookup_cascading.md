# Use Case: Cascading Lookup in `KeySet`

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Caller looks up `Key` with cascading name in existing `KeySet`

## Scenarios

- **Precondition:**
  - [`KeySet` has been created](UC_keyset_create.md)
  - (Only Alternative) Specification for desired `Key` exists and contains links
- **Main success scenario:**
  - Caller asks Core to look up `Key` by cascading name in `KeySet`
  - Core checks for specification matching desired `Key`
  - Core does not find specification
  - Core tries equivalent name in all appropriate namespaces in correct order
  - If a matching `Key` is found, Core returns its index.
  - Otherwise, Core returns a negative value, which indicates "not found".
    Since a `Key` with cascading name cannot be inserted this return value conveys no other information.
- **Alternative scenario:**
  - Caller asks Core to look up `Key` by cascading name in `KeySet`
  - Core checks for specification matching desired `Key`
  - Core finds specification
  - Core tries links from specification and equivalent names in appropriate namespaces in correct order
  - If a matching `Key` is found, Core returns its index.
  - Otherwise, Core returns a negative value, which indicates "not found".
    Since a `Key` with cascading name cannot be inserted this return value conveys no other information.
- **Error scenario:** -
- **Postcondition:**
  - The returned index value MUST be valid and correct until new `Key`(s) are inserted into or removed from the `KeySet`.
- **Non-functional Constraints:** -
