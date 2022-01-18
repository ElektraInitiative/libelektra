# ksAtCursor

- start = 2021-03-14 10:35
- end = 2021-03-14 11:00
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`Key *ksAtCursor(KeySet *ks, elektraCursor cursor)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] 'Return key at the position given by cursor `pos`'
- [ ] Simple example or snippet how to use the function  
       - [ ] add
- [ ] Longer description of function containing common use cases  
       - [ ] Doesn't seem to be accurate, change to cursor
- [x] Description of functions reads nicely
- [ ] `@pre`  
       - [ ] add
- [ ] `@post`  
       - [ ] add
- [ ] `@invariant`  
       - [ ] add
- [ ] `@param` for every parameter  
       - [ ] `pos`: 'the cursor of the key that should be retrieved'
- [ ] `@return` / `@retval`  
       - [ ] split `@retval` into multiple lines
- [ ] `@since`  
       - [ ] add
- [ ] `@ingroup`  
       - [ ] add
- [ ] `@see`  
       - [ ] add `ksGetCursor()`  
       - [ ] add `ksSetCursor()`

### Naming

- [ ] Abbreviations used in function names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)  
       - [ ] define `ks`
- [x] Function names should neither be too long, nor too short
- [x] Function name should be clear and unambiguous
- [ ] Abbreviations used in parameter names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)  
       - [ ] define `ks`
- [ ] Parameter names should neither be too long, nor too short  
       - [ ] `ks` -> `keyset`  
       - [ ] `pos` -> `position`
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
- [ ] Wherever possible, function parameters should be `const`  
       - [ ] `ks` maybe could be `const`?
- [x] Wherever possible, return types should be `const`
- [x] Functions should have the least amount of parameters feasible

### Structural Clarity

- [x] Functions should do exactly one thing
- [x] Function name has the appropriate prefix
- [ ] Order of signatures in kdb.h.in is the same as Doxygen
- [x] No functions with similar purpose exist

### Memory Management

- [x] Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [x] Function code is fully covered by tests
- [ ] All possible error states are covered by tests  
       - [x] add test case for `pos` >= `ks->size`
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

## Other Issues discovered (unrelated to function)
