# keyCmp

- start = 2021-03-07 17:40
- end = 2021-03-07 17:55
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`int keyCmp(const Key *k1, const Key *k2)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [x] First line explains briefly what the function does
- [ ] Simple example or snippet how to use the function  
       - [ ] move last example below brief explanation
- [ ] Longer description of function containing common use cases  
       - [ ] add description about behavior of namespace comparison
- [x] Description of functions reads nicely
- [ ] `@pre`  
       - [x] add
- [ ] `@post`  
       - [x] add
- [ ] `@invariant`  
       - [x] add
- [x] `@param` for every parameter
- [ ] `@return` / `@retval`  
       - [ ] remove `@return` in favor of `@retval`'s?  
       - [ ] add `@retval` for k1 > k2  
       - [ ] add `@retval` for k1 == k2  
       - [ ] add `@retval` for k1 < k2
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [x] `@see`

### Naming

- [ ] Abbreviations used in function names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)  
       - [ ] Define Cmp or write it out?
- [x] Function names should neither be too long, nor too short
- [x] Function name should be clear and unambiguous
- [x] Abbreviations used in parameter names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)
- [ ] Parameter names should neither be too long, nor too short  
       - [ ] k1 -> key1 / this - [ ] k2 -> key2 / that
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
       - [ ] add flags for comparing specific parts of the key
- [ ] Documentation does not impose limits, that would hinder further extensions

### Tests

- [ ] Function code is fully covered by tests  
       - [x] L517  
       - [x] L558
- [ ] All possible error states are covered by tests  
       - [x] test key with same name, but different owners
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

move implementation out of keyset.c ?

## Other Issues discovered (unrelated to function)
