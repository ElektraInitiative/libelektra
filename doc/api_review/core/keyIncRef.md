# keyIncRef

- start = 2021-02-21 20:50
- end = 2021-02-21 21:05
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`ssize_t keyIncRef(Key *key)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] change viability to 'reference counter'
- [ ] Simple example or snippet how to use the function  
       - [ ] add example
- [x] Longer description of function containing common use cases
- [x] Description of functions reads nicely
- [ ] `@pre`  
       - [x] @pre key is a valid key
- [ ] `@post`  
       - [x] @post reference counter of the key is incremented by one
- [ ] `@invariant`  
       - [x] @invariant key stays a valid key
- [x] `@param` for every parameter
- [x] `@return` / `@retval`
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] add `keyGetRef()`  
       - [ ] add `keyDecRef()`

### Naming

- [ ] Abbreviations used in function names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)  
       - [ ] add inc to glossary  
       - [ ] add ref to glossary
- [x] Function names should neither be too long, nor too short
- [x] Function name should be clear and unambiguous
- Abbreviations used in parameter names must be defined in the
  [Glossary](/doc/help/elektra-glossary.md)
- [x] Parameter names should neither be too long, nor too short
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

- Function is easily extensible, e.g., with flags
- [ ] Documentation does not impose limits, that would hinder further extensions

### Tests

- [x] Function code is fully covered by tests
- [ ] All possible error states are covered by tests  
       - [x] add test for null pointer
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

## Other Issues discovered (unrelated to function)
