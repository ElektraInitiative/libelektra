# keyAddBaseName

- start = 2021-03-04 21:35
- end = 2021-03-04 22:00
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`ssize_t keyAddBaseName(Key *key,const char *baseName)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] Adds -> Appends
- [ ] Simple example or snippet how to use the function  
       - [ ] move below brief description
- [ ] Longer description of function containing common use cases  
       - [ ] add notice for read-only keys
- [ ] Description of functions reads nicely  
       - [ ] Add comma after "When baseName is 0"  
       - [ ] 0 -> null pointer
- [ ] `@pre`  
       - [ ] add
- [ ] `@post`  
       - [ ] add
- [ ] `@invariant`  
       - [ ] add
- [x] `@param` for every parameter
- [ ] `@return` / `@retval`  
       - [ ] add read-only case
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] move to bottom

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
- [ ] Order of signatures in kdb.h.in is the same as Doxygen  
       - [ ] swapped with `keySetBaseName`
- [x] No functions with similar purpose exist

### Memory Management

- [x] Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [x] Function code is fully covered by tests
- [x] All possible error states are covered by tests
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

## Other Issues discovered (unrelated to function)
