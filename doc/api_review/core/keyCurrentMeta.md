# keyCurrentMeta

- start = 2021-02-27 17:20
- end = 2021-02-27 17:30
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`const Key *keyCurrentMeta(const Key *key)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] returns the Key of the meta-information the internal iterator points at
- [ ] Simple example or snippet how to use the function  
       - [ ] add
- [x] Longer description of function containing common use cases
- [ ] Description of functions reads nicely  
       - [ ] The pointer -> The returned pointer
- [ ] `@pre`  
       - [x] add
- [ ] `@post`  
       - [x] add
- [ ] `@invariant`  
       - [x] add
- [ ] `@param` for every parameter  
       - [ ] Key to get the current meta-information from
- [ ] `@return` / `@retval`  
       - [ ] a pointer to the current meta-information's Key
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] split first line into two lines

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

- Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [ ] Documentation does not impose limits, that would hinder further extensions  
       - [ ] pointer is NULL after `ksRewind()`

### Tests

- [x] Function code is fully covered by tests
- [ ] All possible error states are covered by tests  
       - [x] add check for null pointer
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

## Other Issues discovered (unrelated to function)
