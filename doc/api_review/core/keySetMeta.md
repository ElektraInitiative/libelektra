# keySetMeta

- start = 2021-02-26 13:30
- end = 2021-02-26 14:15
- moderator = Stefan Hanreich <stefanhani@gmail.com>
- reviewer = Klemens Boeswirth

## Signature

`ssize_t keySetMeta(Key *key, const char* metaName,const char *newMetaString)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] Set the value of a meta-information key
- [ ] Simple example or snippet how to use the function  
       - [ ] add simple example
- [x] Longer description of function containing common use cases
- [ ] Description of functions reads nicely  
       - [ ] improve description
- [ ] `@pre`  
       - [x] key should not have read-only metadata  
       - [x] metaName must be a valid key name
- [ ] `@post`  
       - [x] value set  
       - [x] Key has a meta-information Keyset (?)
- [ ] `@invariant`  
       - [x] key stays valid
- [ ] `@param` for every parameter  
       - [ ] metaName - add dot, split into two lines
- [ ] `@return` / `@retval`  
       - [ ] split `-1` into multiple lines
- [ ] `@since`  
       - [ ] add `1.0.0`
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
- [ ] Parameter names should neither be too long, nor too short  
       - [ ] newMetaString -> metaValue
- [ ] Parameter names should be clear and unambiguous  
       - [ ] newMetaString -> metaValue

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
       - [ ] `keyDelMeta()` ?  
       - Similar thing could be achieved with  
       `ksAppendKey ( keyMeta (key), keyNew (metaName, KEY_VALUE, newMetaString, KEY_END) )`

### Memory Management

- [x] Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [x] Function code is fully covered by tests  
       - [x] memory errors hard to cover
- [ ] All possible error states are covered by tests  
       - [x] memory errors hard to cover
- All possible enum values are covered by tests
- [ ] No inconsistencies between tests and documentation  
       - [x] test case for key 0  
       - [x] test case for name 0  
       - [ ] test case for invalid name

## Summary

- For 1.0.0 only metaNames with namespace `meta:/` should be accepted

## Other Issues discovered (unrelated to function)
