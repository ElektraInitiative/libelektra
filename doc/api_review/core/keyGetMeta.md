# keyGetMeta

- start = 2021-02-26 14:20
- end = 2021-02-26 14:45
- moderator = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`const Key *keyGetMeta(const Key *key, const char* metaName)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] returns the Key of a meta...  
       - [ ] name -> metaName
- [ ] Simple example or snippet how to use the function  
       - [ ] move up  
       - [ ] assign to variable  
       - [ ] add comment about value in variable  
       - [ ] remove function, if
- [x] Longer description of function containing common use cases
- [ ] Description of functions reads nicely  
       - [ ] Remove 'You are not allowed....'
- [ ] `@pre`  
       - [x] add
- [ ] `@post`  
       - [x] add
- [ ] `@invariant`  
       - [x] add
- [ ] `@param` for every parameter  
       - [ ] split to two lines  
       - [ ] add dot after first sentence
- [ ] `@return` / `@retval`  
       - [ ] move `@return` up  
       - [ ] `@return`: value -> Key  
       - [ ] `@retval` 0 metaName invalid
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] add `keyMeta()`

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
- [ ] No functions with similar purpose exist  
       Similar thing could be achieved via  
       `ksLookupByName (keyMeta (key), metaName, 0)`

### Memory Management

- [x] Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [ ] Function code is fully covered by tests  
       - [x] see below
- [ ] All possible error states are covered by tests  
       - [x] key is 0  
       - [x] metaName is 0
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

## Other Issues discovered (unrelated to function)
