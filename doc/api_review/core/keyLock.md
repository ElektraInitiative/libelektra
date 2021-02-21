# keyLock

- start = 2021-02-21 21:45
- end = 2021-02-21 22:05
- moderator = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`int keyLock (Key * key, elektraLockFlags what)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [x] First line explains briefly what the function does
- [ ] Simple example or snippet how to use the function - [ ] add
- [x] Longer description of function containing common use cases
- [x] Description of functions reads nicely
- [ ] `@pre`  
       - [ ] key is a valid key  
       - [ ] what contains valid elektraLockFlags
- [ ] `@post`  
       - [ ] parts of the keys, as stated in the what-parameter, are locked
- [ ] `@invariant`  
       - [ ] key stays a valid key
- [ ] `@param` for every parameter  
       - [ ] key  
       - [ ] what
- [ ] `@return` / `@retval`  
       - [ ] move above see also  
       - [ ] move `>0` to default case (`@return`)
- [ ] `@since`  
       - [ ] add
- [ ] `@ingroup`  
       - [ ] add
- [ ] `@see`  
       - [ ] add `keyIsLocked()`

### Naming

- Abbreviations used in function names must be defined in the
  [Glossary](/doc/help/elektra-glossary.md)
- [x] Function names should neither be too long, nor too short
- [x] Function name should be clear and unambiguous
- [x] Abbreviations used in parameter names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)
- [x] Parameter names should neither be too long, nor too short
- [ ] Parameter names should be clear and unambiguous  
       - [ ] rename what to flags?

### Compatibility

(only in PRs)

- [Symbol versioning](/doc/dev/symbol-versioning.md)
  is correct for breaking changes
- ABI/API changes are forward-compatible (breaking backwards-compatibility
  to add additional symbols is fine)

### Parameter & Return Types

- [x] Function parameters should use enum types instead of boolean types
      wherever sensible
- [x] Wherever possible, function parameters should be `const`
- [x] Wherever possible, return types should be `const`
- [x] Functions should have the least amount of parameters feasible

### Structural Clarity

- [x] Functions should do exactly one thing
- [x] Function name has the appropriate prefix
- [ ] Order of signatures in kdb.h.in is the same as Doxygen  
       - [ ] move below `keyGetRef()`-family of functions
- [x] No functions with similar purpose exist

### Memory Management

- [x] Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [ ] Documentation does not impose limits, that would hinder further extensions  
       - [ ] keys cannot be unlocked

### Tests

- [x] Function code is fully covered by tests
- [ ] All possible error states are covered by tests  
       - [ ] test null pointer
- [x] All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

Also check return values of function in tests
