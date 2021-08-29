# keyCopyMeta

- start = 2021-02-27 17:30
- end = 2021-02-27 17:45
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`int keyCopyMeta(Key *dest, const Key *source, const char *metaName)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] of metadata -> of the metaData with name `metaName`
- [ ] Simple example or snippet how to use the function  
       - [ ] move below `@brief`
- [ ] Longer description of function containing common use cases  
       - [ ] describe the functionality more explicitly where meta-information
      gets deleted when `source`'s `metaName` is not set in `dest`  
       - [ ] add information that it does nothing on read-only keys
- [ ] Description of functions reads nicely  
       - [ ] it will take -> will take
- [ ] `@pre`  
       - [x] 'dest is a valid Key'  
       - [x] 'source is a valid Key'
- [x] `@post`
- [ ] `@invariant`  
       - [x] 'dest stays a valid Key'  
       - [x] 'source stays a valid Key'
- [ ] `@param` for every parameter  
       - [ ] too -> to  
       - [ ] name of the metadata -> name of the key in the metadata
- [ ] `@return` / `@retval`  
       - [ ] move `1` to `@return`  
       - [ ] add `-1` if meta-information is read-only
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] add `keyCopyAllMeta()`

### Naming

- Abbreviations used in function names must be defined in the
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
       - [ ] Lines 294:298
- [ ] All possible error states are covered by tests  
       - [ ] add test case for clearing meta-information key
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

## Other Issues discovered (unrelated to function)
