# kdbGet

- start = 2021-03-20 19:10
- end = 2021-03-20 19:25
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`int kdbGet(KDB *handle, KeySet *returned, Key *parentKey)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] 'Atomic operation to retrieve Keys from a KDB'
- [ ] Simple example or snippet how to use the function  
       - [ ] add
- [ ] Longer description of function containing common use cases  
       - [ ] explain in more detail which keys are included (`keyIsBelow()`)  
       - [ ] move above description out of `@param`
- [ ] Description of functions reads nicely  
       - [ ] remove indentation from first paragraph in Optimization
- [ ] `@pre`  
       - [ ] move below description  
       - [ ] KeySet is not named `returned` anymore
- [ ] `@post`  
       - [ ] add
- [ ] `@invariant`  
       - [ ] add
- [x] `@param` for every parameter
- [x] `@return` / `@retval`
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] `keyIsBelow()` for rules on parents / children

### Naming

- [x] Abbreviations used in function names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)
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
- [ ] Wherever possible, function parameters should be `const`  
       - [ ] handle  
       - [ ] parentKey
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
       - [ ] Lines 1185:1188  
       - [ ] Lines 1218:1220  
       - [ ] Lines 1225:1227  
       - [ ] Lines 1262:1288  
       - [ ] Lines 1298:1310  
       - [ ] Lines 1345:1347  
       - [ ] Line 1205  
       - [ ] Line 1234  
       - [ ] Line 1368  
       - [ ] Line 1403  
       - [ ] add `kdbGet` specific tests
- [ ] All possible error states are covered by tests  
       - [ ] parentKey namespace is `KEY_NS_META`  
       - [ ] parentKey namespace is `KEY_NS_NONE`  
       - [ ] `ks == NULL`
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

## Other Issues discovered (unrelated to function)
