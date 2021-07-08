# ksCut

- start = 2021-03-20 17:30
- end = 2021-03-20 17:50
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`KeySet *ksCut(KeySet *ks, const Key *cutpoint)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] 'Cuts out all Keys from a KeySet that are below the specified key (see `keyIsBelow()`)'
- [ ] Simple example or snippet how to use the function  
       - [ ] move up
- [x] Longer description of function containing common use cases
- [x] Description of functions reads nicely
- [ ] `@pre`  
       - [ ] add
- [ ] `@post`  
       - [ ] add
- [ ] `@invariant`  
       - [ ] add
- [ ] `@param` for every parameter  
       - [ ] move above return
- [x] `@return` / `@retval`
- [ ] `@since`  
       - [ ] add
- [ ] `@ingroup`  
       - [ ] add
- [ ] `@see`  
       - [ ] move to bottom  
       - [ ] add `keyIsBelow()`

### Naming

- [ ] Abbreviations used in function names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)  
       - [ ] define `ks`
- [x] Function names should neither be too long, nor too short
- [x] Function name should be clear and unambiguous
- [ ] Abbreviations used in parameter names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)  
       - [ ] define `ks`
- [ ] Parameter names should neither be too long, nor too short  
       - [ ] `ks` -> `keySet`
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
- [x] No functions with similar purpose exist

### Memory Management

- [x] Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [ ] Function code is fully covered by tests  
       - [ ] fully covered yes, but no return value is ever checked  
       - [ ] add test case for the example at least
- [ ] All possible error states are covered by tests  
       - [x] `ks == NULL`  
       - [x] `cutpoint == NULL`  
       - [x] `!ks->array`  
       - [x] `cutpoint->key == NULL`  
       - [x] `cutpoint->key == ''`
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

## Other Issues discovered (unrelated to function)
