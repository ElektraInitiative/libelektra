# keyNew

- start = 2021-02-14 02:55
- end = 2021-02-14 03:22
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`Key *keyNew(const char *keyname, ...)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [x] First line explains briefly what the function does
- [x] Simple example or snippet how to use the function
- [ ] Longer description of function containing common use cases  
       - [ ] is 0 a valid name? returns NULL if 0 is param
- [x] Description of functions reads nicely
- [ ] `@pre`  
       - [x] @pre name must be a valid key name
- [ ] `@post`  
       - [x] @post returns a valid key object
- [ ] `@invariant`  
       - [x] add
- [ ] `@param` for every parameter  
       - [ ] add `@param` for `...`
- [x] `@return` / `@retval`
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [x] `@see`

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

- [x] Function parameters should use enum types instead of boolean types
      wherever sensible
- [x] Wherever possible, function parameters should be `const`
- [x] Wherever possible, return types should be `const`
- [x] Functions should have the least amount of parameters feasible

### Structural Clarity

- [x] Functions should do exactly one thing  
       - it doesn't technically - but its a convenience function
- [x] Function name has the appropriate prefix
- [x] Order of signatures in kdb.h.in is the same as Doxygen
- [x] No functions with similar purpose exist

### Memory Management

- [x] Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [ ] Documentation does not impose limits, that would hinder further extensions  
       - [x] Docs say `Key *k = keyNew(0);` has same effect  
       as `Key *k =keyNew("", KEY_END);`

### Tests

- [x] Function code is fully covered by tests
- [x] All possible error states are covered by tests
- [ ] All possible enum values are covered by tests  
       - [x] KEY_META  
       - [x] KEY_FLAGS
- [ ] No inconsistencies between tests and documentation  
       - [x] Documentation says i can work with `Key *k =keyNew("", KEY_END);`  
       Tests say  
       `k = keyNew ("", KEY_END); succeed_if (k == NULL, "should be invalid"); keyDel (k);`  
       - [x] same as above with `keyNew(0)`

## Summary

## Other Issues discovered (unrelated to function)
