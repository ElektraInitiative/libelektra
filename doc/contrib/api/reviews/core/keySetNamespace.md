# keySetNamespace

- start = 2021-03-04 22:50
- end = 2021-03-04 23:05
- moderator = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`ssize_t keySetNamespace(Key * key, elektraNamespace ns)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [x] First line explains briefly what the function does
- [ ] Simple example or snippet how to use the function  
       - [ ] add
- [ ] Longer description of function containing common use cases  
       - [ ] add info for read-only keys
- [ ] Description of functions reads nicely  
       - [ ] References to Key not working
- [x] `@pre`
- [ ] `@post`  
       - [ ] add
- [ ] `@invariant`  
       - [ ] add
- [x] `@param` for every parameter
- [ ] `@return` / `@retval`  
       - [ ] move return values below returns
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] maybe add `keyGetNamespace()`

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

- [x] Function parameters should use enum types instead of boolean types
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
       - [x] several cases in switches for enums not covered  
       - [x] ns == `key->ukey[0]`
- [ ] All possible error states are covered by tests  
       - [x] key == `NULL`  
       - [x] ns == `KEY_NS_NONE`
- [ ] All possible enum values are covered by tests  
       - [x] several cases in switches for enums not covered
- [x] No inconsistencies between tests and documentation

## Summary

comment in Line 1619: growing -> shrinking

## Other Issues discovered (unrelated to function)
