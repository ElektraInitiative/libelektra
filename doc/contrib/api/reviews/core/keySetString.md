# keySetString

- start = 2021-02-21 12:20
- end = 2021-02-21 18:10
- moderator = Stefan Hanreich <stefanhani@gmail.com>
- reviewer = Dominic Jäger

## Signature

`ssize_t keySetString(Key *key, const char *newString)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [x] First line explains briefly what the function does
- [ ] Simple example or snippet how to use the function  
       - [ ] add
- Longer description of function containing common use cases
  - All uses cases covered by brief description
- [ ] Description of functions reads nicely  
       - [ ] 'as new string value' seems strange  
       - [ ] 'private copy' is unclear
- [ ] `@pre`  
       - [x] add
- [ ] `@post`  
       - [x] add  
       - [x] string saved as UTF-8 in backend
- [ ] `@invariant`  
       - [x] add
- [ ] `@param` for every parameter  
       - [ ] 'text string' maybe redundant?
- [x] `@return` / `@retval`
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] split to multiple lines  
       - [ ] shortly explain differences between `keyString` `keyGetString`  
       - [ ] add `keySetBinary` ?  
       - [ ] remove `keyValue`, as it is not related to string

### Naming

- Abbreviations used in function names must be defined in the
  [Glossary](/doc/help/elektra-glossary.md)
- [x] Function names should neither be too long, nor too short
- [x] Function name should be clear and unambiguous
- Abbreviations used in parameter names must be defined in the
  [Glossary](/doc/help/elektra-glossary.md)
- [ ] Parameter names should neither be too long, nor too short  
       - [ ] `newStringValue` - remove Value?
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
- [ ] Documentation does not impose limits, that would hinder further extensions  
       - [ ] iconv-Plugin  
       - [ ] UTF-8  
       - [ ] Return Value on NULL

### Tests

- [x] Function code is fully covered by tests
- [x] All possible error states are covered by tests
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

Behavior when `newStringValue` is a NULL pointer seems strange

## Other Issues discovered (unrelated to function)

Exact difference keyString keyGetString ?
