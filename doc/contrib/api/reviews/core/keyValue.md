# keyValue

- start = 2021-03-20 18:45
- end = 2021-03-20 18:55
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`const void *keyValue(const Key *key)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [x] First line explains briefly what the function does
- [x] Simple example or snippet how to use the function
- [ ] Longer description of function containing common use cases  
       - [ ] use correct testing functions in examples
- [ ] Description of functions reads nicely  
       - 'and you should use it if' -> 'and you should use it, if'
- [ ] `@pre`  
       - [x] add
- [ ] `@post`  
       - [x] add
- [ ] `@invariant`  
       - [x] add
- [x] `@param` for every parameter
- [x] `@return` / `@retval`
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] split first line and add explanations  
       - [ ] add `keyIsBinary()`  
       - [ ] add `keyIsString()`

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
- [ ] Documentation does not impose limits, that would hinder further extensions  
       - [ ] return type on empty string/ binary?

### Tests

- [x] Function code is fully covered by tests
- [x] All possible error states are covered by tests
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

## Other Issues discovered (unrelated to function)
