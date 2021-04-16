# kdbOpen

- start = 2021-03-20 18:55
- end = 2021-03-20 19:10
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`KDB * kdbOpen(Key *errorKey)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [x] First line explains briefly what the function does
- [ ] Simple example or snippet how to use the function  
       - [ ] add
- [ ] Longer description of function containing common use cases  
       - [ ] how do errors end up in `errorKey`?  
       - [ ] what kind of errors end up in `errorKey`?
- [ ] Description of functions reads nicely  
       - [ ] monospace 'system:/elektra/mountpoints'  
       - [ ] move 'You must always call...' one paragraph up
- [ ] `@pre`  
       - [ ] remove first (duplicated)  
       - [ ] move below description
- [ ] `@post`  
       - [ ] add
- [ ] `@invariant`  
       - [ ] add
- [ ] `@param` for every parameter  
       - [ ] explain format of contract
- [ ] `@return` / `@retval`  
       - [ ] specify failure conditions
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] split into two lines  
       - [ ] remove `kdbGet`?

### Naming

- [x] Abbreviations used in function names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)
- [x] Function names should neither be too long, nor too short
- [x] Function name should be clear and unambiguous
- [x] Abbreviations used in parameter names must be defined in the
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
       - [ ] Lines 433:443  
       - [ ] Lines 451:470  
       - [ ] Line 478  
       - [ ] Lines 508:515  
       - [ ] Lines 531:539  
       - [ ] return value never checked in tests
- [ ] All possible error states are covered by tests  
       - [ ] `errorKey == NULl`
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

## Other Issues discovered (unrelated to function)

`ksRenameKeys` and `elektraOpenBootstrap` in API Documentation
