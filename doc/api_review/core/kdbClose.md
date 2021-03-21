# kdbClose

- start = 2021-03-09 01:55
- end = 2021-03-09 02:10
- moderator = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`int kdbClose(KDB *handle, Key *errorKey)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [x] First line explains briefly what the function does
- [ ] Simple example or snippet how to use the function  
       - [ ] add
- [ ] Longer description of function containing common use cases  
       - [ ] add possible error messages `errorKey` could contain
- [x] Description of functions reads nicely
- [ ] `@pre`  
       - [ ] move below description
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
       - [ ] add `kdbOpen()`

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
       - [ ] line 587  
       - [ ] return value never gets checked
- [ ] All possible error states are covered by tests  
       - [ ] add test for `handle == NULl`
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

- [ ] What happens if `errorKey` is `NULL`?

## Other Issues discovered (unrelated to function)

- [ ] `ksRenameKeys()` in KDB-Module?
