# keyDecRef

- start = 2021-02-21 21:10
- end = 2021-02-21 21:25
- moderator = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`ssize_t keyDecRef(Key *key)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] change viability to reference counter
- [ ] Simple example or snippet how to use the function  
       - [ ] add example
- [x] Longer description of function containing common use cases
- [ ] Description of functions reads nicely  
       - [ ] description seems a bit unclear when reading
- [ ] `@pre`  
       - [ ] @pre key is a valid key
- [ ] `@post`  
       - [ ] @post reference counter of the key is decremented by one
- [ ] `@invariant`  
       - [ ] @invariant key stays a valid key
- [x] `@param` for every parameter
- [x] `@return` / `@retval`
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - split to multiple lines

### Naming

- [ ] Abbreviations used in function names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)  
       - add dec to glossary  
       - add ref to glossary
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
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [ ] Function code is fully covered by tests  
       - test decrementing key with reference counter = 0
- [ ] All possible error states are covered by tests  
       - test decrementing key with reference counter = 0  
       - test null pointer
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary
