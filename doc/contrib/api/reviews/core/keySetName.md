# keySetName

- start = 2021-02-14 03:30
- end = 2021-02-14 03:50
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`ssize_t keySetName(Key *key, const char *newname)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [x] First line explains briefly what the function does
- [ ] Simple example or snippet how to use the function  
       - [ ] add
- [ ] Longer description of function containing common use cases
- [ ] Description of functions reads nicely
- [ ] `@pre`  
       - [ ] @pre newName must be a valid name  
       - [ ] @pre must not be a read-only key  
       - [ ] @pre must not have been inserted before
- [ ] `@post`  
       - [ ] @post Key has (possibly modified) newName as name
- [ ] `@invariant`  
       - [ ] add
- [x] `@param` for every parameter
- [ ] `@return` / `@retval`  
       - [ ] add `@retval` -1 if key is read-only
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] add `keySetNameSpace()`

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
- [ ] Wherever possible, return types should be `const`  
       - [ ] might be possible to make it `const`
- [x] Functions should have the least amount of parameters feasible

### Structural Clarity

- [x] Functions should do exactly one thing
- [x] Function name has the appropriate prefix
- [ ] Order of signatures in kdb.h.in is the same as Doxygen  
       - [ ] swapped with functions for unescaped
- [x] No functions with similar purpose exist

### Memory Management

- [x] Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [ ] Documentation does not impose limits, that would hinder further extensions  
       - [ ] behavior on invalid names

### Tests

- [ ] Function code is fully covered by tests  
       - [x] test_bit "CANNOT" fail, so might not be necessary to cover this  
       for now should be made more resistant to future changes
- [ ] All possible error states are covered by tests  
       - [x] test read-only keys
- All possible enum values are covered by tests
- [ ] No inconsistencies between tests and documentation  
       - [x] https://github.com/ElektraInitiative/libelektra/blob/master/tests/abi/testabi_key.c#L357  
       checks for -1 if null pointer is provided  
       documentation says 0 will be returned  
       - [x] Documentation says name will be `""` after an invalid name  
       Tests show that name stays unchanged  
       https://github.com/ElektraInitiative/libelektra/blob/master/tests/abi/testabi_key.c#L601  
       - [x] Documentations should include stripping trailing `/`

## Summary

## Other Issues discovered (unrelated to function)
