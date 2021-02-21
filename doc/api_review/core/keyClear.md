# keyClear

- start = 2021-01-23 18:40
- end = 2021-01-23 19:00
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`int keyClear(Key *key)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does - remove first line, second line could be improved a bit
      (what is meant with internal data, exactly? is the name included?)
- [x] Simple example or snippet how to use the function
- [ ] Longer description of function containing common use cases - be more explicit which data is actually included
- [ ] Description of functions reads nicely  
       - [ ] very short sentences
- [ ] `@pre`  
       - [ ] @pre key is a valid key
- [ ] `@post`  
       - [ ] @post key contains not internal data
- [ ] `@invariant`  
       - [ ] @invariant key stays a valid key
- [x] `@param` for every parameter
- [ ] `@return` / `@retval`  
       - [ ] mark default case with `@return`
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] maybe `keyDel()`

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
       - [ ] swap with `keyDel()`
- [ ] No functions with similar purpose exist  
       - [ ] new keyCopy can also clear a key, maybe make `keyClear` an alias?

### Memory Management

- [x] Memory Management should be handled by the function wherever possible

### Extensibility

- [ ] Function is easily extensible, e.g., with flags  
       - [ ] could add flags for clearing parts of the key like `keyCopy()`
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [x] Function code is fully covered by tests
- [x] All possible error states are covered by tests
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary
