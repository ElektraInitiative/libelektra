# keyGetBinary

- start = 2021-03-07 17:15
- end = 2021-03-07 17:30
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`ssize_t keyGetBinary(const Key *key, void *returnedBinary, size_t maxSize)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] as binary data
- [ ] Simple example or snippet how to use the function  
       - [ ] move up
- [ ] Longer description of function containing common use cases  
       - [ ] merge reference to string functions with `@see`  
       - [ ] how exactly is empty defined for binary?
- [x] Description of functions reads nicely
- [ ] `@pre`  
       - [x] add
- [ ] `@post`  
       - [x] add
- [ ] `@invariant`  
       - [x] add
- [x] `@param` for every parameter
- [ ] `@return` / `@retval`  
       - [ ] merge lines for maxSize errors
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] split first line

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
       - [x] L417  
       - [x] L422  
       - [x] L427
- [ ] All possible error states are covered by tests  
       - [x] add test for key with string value  
       - [x] L422 seems to be in the tests - check  
       - [x] L427 seems to be in the tests - check
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

"Clear" binary array on error, to mirror behavior from string?
What to fill it with, though? Zeroes?

## Other Issues discovered (unrelated to function)
