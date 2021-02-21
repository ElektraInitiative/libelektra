# keyGetRef

- start = 2021-02-21 21:25
- end = 2021-02-21 21:35
- moderator = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`ssize_t keyGetRef(const Key *key)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [x] First line explains briefly what the function does
- [ ] Simple example or snippet how to use the function  
      - [ ] add simple example for `keyGetRef()` base case without ks  
- [x] Longer description of function containing common use cases
- [x] Description of functions reads nicely
- [ ] `@pre`  
      - [ ] @pre key is a valid key  
- [ ] `@post`  
      - [ ] @post key stays unchanged  
- [ ] `@invariant`  
      - [ ] @invariant key stays a valid key  
- [x] `@param` for every parameter
- [x] `@return` / `@retval`
- [ ] `@since`  
      - [ ] add  
- [x] `@ingroup`
- [ ] `@see`  
      - maybe add `keyDel()`  

### Naming

- [ ] Abbreviations used in function names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)  
      - [ ] add Ref to the Glossary  
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

- Memory Management should be handled by the function wherever possible

### Extensibility

- Function is easily extensible, e.g., with flags
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [x] Function code is fully covered by tests
- [ ] All possible error states are covered by tests  
      - [ ] check for null pointer
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary
