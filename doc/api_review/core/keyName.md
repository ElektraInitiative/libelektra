# keyName

- start = 2021-02-14 02:12
- end = 2021-02-14 02:44
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`const char *keyName(const Key *key)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [x] First line explains briefly what the function does
- [x] Simple example or snippet how to use the function
- [ ] Longer description of function containing common use cases
- [ ] Description of functions reads nicely
- [ ] `@pre`  
       - [ ] @pre key must be a valid key
- [ ] `@post`  
       - [ ] @post Pointer to key that can change over time
- [ ] `@invariant`  
       - [ ] key name stays valid
- [x] `@param` for every parameter
- [ ] `@return` / `@retval`  
       - [ ] add default return value to `@retval`
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [x] `@see`

### Naming

- [x] Abbreviations used in function names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)
- [x] Function names should neither be too long, nor too short
- [ ] Function name should be clear and unambiguous  
       - [ ] Function name might lead to confusions with `keyGetName()`, make  
       their intentions more clear in their names
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
       - [ ] `keyUnescapedName` might be considered too similar

### Memory Management

- [x] Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [x] Function code is fully covered by tests
- [ ] All possible error states are covered by tests  
       - [x] empty keyName is not covered in tests
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

## Other Issues discovered (unrelated to function)
