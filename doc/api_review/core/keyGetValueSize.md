# keyGetValueSize

- start = 2021-02-26 16:40
- end = 2021-02-26 17:15
- moderator = Stefan Hanreich <stefanhani@gmail.com>
- reviewer = Robert Sowula

## Signature

`ssize_t keyGetValueSize(const Key *key)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [x] First line explains briefly what the function does
- [ ] Simple example or snippet how to use the function  
       - [ ] move below brief description
- [x] Longer description of function containing common use cases
- [ ] Description of functions reads nicely  
       - [ ] If the value is binary, there...  
       - [ ][..] for binary data, so 0 will be returned.  
       - [ ] String types have it, so to there length will be added 1 to have enough space to store it. -> Strings are null-terminated, and the null terminator will be considered for the length.
- [ ] `@pre`  
       - [x] add?
- [ ] `@post`  
       - [x] add?
- [ ] `@invariant`  
       - [x] add?
- [ ] `@param` for every parameter  
       - [ ] key: key -> Key
- [ ] `@return` / `@retval`  
       - [ ] 1: not binary -> string  
       - [ ] 0: binary -> not string (?)
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] `keyGetString()`  
       - [ ] `keyGetBinary()`

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
       - [ ] swapped `with keyGetValueSize()`
- [x] No functions with similar purpose exist

### Memory Management

- Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [ ] Function code is fully covered by tests  
       - [x] Line 249
- [x] All possible error states are covered by tests
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

## Other Issues discovered (unrelated to function)
