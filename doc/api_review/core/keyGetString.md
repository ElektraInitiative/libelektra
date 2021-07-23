# keyGetString

- start = 2021-03-06 16:25
- end = 2021-03-06 16:40
- moderator = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`ssize_t keyGetString(const Key *key, char *returnedString, size_t maxSize)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] Copies the string value of a Key into a provided buffer
- [ ] Simple example or snippet how to use the function  
       - [ ] move up
- [ ] Longer description of function containing common use cases  
       - [ ] move binary functions to `@see`
- [ ] Description of functions reads nicely
- [ ] `@pre`  
       - [x] add
- [ ] `@post`  
       - [x] add
- [ ] `@invariant`  
       - [x] add
- [x] `@param` for every parameter
- [ ] `@return` / `@retval`  
       - [ ] merge maxSize errors?
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] split and add description

### Naming

- Abbreviations used in function names must be defined in the
  [Glossary](/doc/help/elektra-glossary.md)
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

- [x] Function is easily extensible, e.g., with flags
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [ ] Function code is fully covered by tests  
       - [x] Line 306  
       - [x] Line 317  
       (impossible to check - overflow)
- [ ] All possible error states are covered by tests  
       - [x] check return value for binary strings  
       (seems to already happen? tests/abi/testabi_key.c:1007 ff.)  
       - [x] check return value for maxSize too small  
       (seems to already happen? tests/abi/testabi_key.c:913)  
       - [x] check return value for maxSize > SSIZE_MAX  
       (impossible to check - overflow)
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

## Other Issues discovered (unrelated to function)

- Find a different term for functions that get a pointer and functions that
- return in to a buffer?
