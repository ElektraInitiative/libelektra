# kdbSet

- start = 2021-03-20 19:10
- end = 2021-03-20 19:30
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`int kdbSet(KDB *handle, KeySet *returned, Key *parentKey)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] 'Atomic operation to set Keys for a KDB'
- [ ] Simple example or snippet how to use the function  
       - [ ] add
- [ ] Longer description of function containing common use cases  
       - [ ] move description out of `@param`  
       - [ ] add info what happens if keys are read-only
- [ ] Description of functions reads nicely  
       - [ ] 'give a hint' -> 'specify'
- [ ] `@pre`  
       - [ ] move below description  
       - [ ] KeySet is not named `returned` anymore
- [ ] `@post`  
       - [ ] add
- [ ] `@invariant`  
       - [ ] add
- [ ] `@param` for every parameter  
       - [ ] move parentKey description to description text
- [x] `@return` / `@retval`
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [x] `@see`

### Naming

- [x] Abbreviations used in function names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)
- [x] Function names should neither be too long, nor too short
- [x] Function name should be clear and unambiguous
- [ ] Abbreviations used in parameter names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)  
       - [ ] define `ks`
- [ ] Parameter names should neither be too long, nor too short  
       - [ ] `ks` -> `keySet`
- [x] Parameter names should be clear and unambiguous

### Compatibility

(only in PRs)

- [Symbol versioning](/doc/dev/symbol-versioning.md)
  is correct for breaking changes
- ABI/API changes are forward-compatible (breaking backwards-compatibility
  to add additional symbols is fine)

### Parameter & Return Types

- Function parameters should use enum types instead of boolean types
  wherever sensible
- [x] Wherever possible, function parameters should be `const`
- [x] Wherever possible, return types should be `const`
- [x] Functions should have the least amount of parameters feasible

### Structural Clarity

- [x] Functions should do exactly one thing
- [x] Function name has the appropriate prefix
- [x] Order of signatures in kdb.h.in is the same as Doxygen
- [x] No functions with similar purpose exist

### Memory Management

- [x] Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [ ] Function code is fully covered by tests  
       - [ ] Lines 1737:1740  
       - [ ] Lines 1745:1748  
       - [ ] Lines 1753:1756  
       - [ ] Lines 1761:1764  
       - [ ] Lines 1796:1798  
       - [ ] Lines 1806:1809  
       - [ ] Line 1906
- [ ] All possible error states are covered by tests  
       - [ ] `handle == NULL`  
       - [ ] `ks == NULL`  
       - [ ] `parentKey == NULL`  
       - [ ] read-only parentKey
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

## Other Issues discovered (unrelated to function)
