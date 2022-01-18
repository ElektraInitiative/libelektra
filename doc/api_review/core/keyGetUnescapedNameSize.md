# keyGetUnescapedNameSize

- start = 2021-03-02 22:00
- end = 2021-03-02 22:15
- moderator = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`ssize_t keyGetUnescapedNameSize(const Key *key)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [x] First line explains briefly what the function does
- [ ] Simple example or snippet how to use the function  
       - [ ] add
- [ ] Longer description of function containing common use cases
- [ ] Description of functions reads nicely  
       - [ ] return -> Returns the size of the unescaped name, including ...
- [ ] `@pre`  
       - [ ] add
- [ ] `@post`  
       - [ ] add
- [ ] `@invariant`  
       - [ ] add
- [x] `@param` for every parameter
- [ ] `@return` / `@retval`  
       - [ ] add `@return` for base case
- [ ] `@since`  
       - [ ] add
- [ ] `@ingroup`  
       - [ ] add
- [ ] `@see`  
       - [ ] add `keyGetNameSize()`  
       - [ ] add `keyGetUnescapedName()`

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
       - [ ] Order is mixed up, needs to be lower in Doxygen
- [x] No functions with similar purpose exist

### Memory Management

- [x] Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [ ] Function code is fully covered by tests  
       - [x] no explicit calls in tests suite
- [ ] All possible error states are covered by tests  
       - [x] no explicit calls in tests suite
- All possible enum values are covered by tests
- [ ] No inconsistencies between tests and documentation  
       - [x] no explicit calls in tests suite

## Summary

## Other Issues discovered (unrelated to function)
