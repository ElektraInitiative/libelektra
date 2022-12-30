# keyDup

- start = 2021-02-20 18:10
- end = 2021-02-20 18:35
- moderator = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`Key *keyDup(const Key *source)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] could be a bit more specific
- [ ] Simple example or snippet how to use the function  
       - [ ] remove the wrapping function as it only distracts
- [ ] Longer description of function containing common use cases  
       - [ ] be a bit more explicit
- [ ] Description of functions reads nicely  
       - [ ] "Like for a new key" could be formulated better
- [ ] `@pre`  
       - [ ] @pre source must be a valid key
- [ ] `@post`  
       - [ ] @post returns a full copy of the key including all info (metadata, ..)
- [ ] `@invariant`  
       - [ ] @invariant source stays a valid key
- [x] `@param` for every parameter
- [x] `@return` / `@retval`
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] `keyCopy()`

### Naming

- [ ] Abbreviations used in function names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)  
       - [ ] add dup to glossary
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

- [ ] Function is easily extensible, e.g., with flags  
       - [ ] could add flags similar to `keyCopy()`
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [x] Function code is fully covered by tests  
       - Error state is hard to check, cause it only appears on null pointers
      which gets checked in the function itself already
- [x] All possible error states are covered by tests  
       - see above
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

## Other Issues discovered (unrelated to function)
