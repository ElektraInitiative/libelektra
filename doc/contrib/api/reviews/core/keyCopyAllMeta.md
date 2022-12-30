# keyCopyAllMeta

- start = 2021-02-27 17:45
- end = 2021-02-27 18:00
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`int keyCopyAllMeta(Key *dest, const Key *source)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [x] First line explains briefly what the function does
- [ ] Simple example or snippet how to use the function  
       - [ ] move simple example below `@brief`
- [ ] Longer description of function containing common use cases  
       - [ ] add info that if `dest` is read-only nothing will happen
- [x] Description of functions reads nicely
- [ ] `@pre`  
       - [x] add 'dest is a valid Key'  
       - [x] add 'source is a valid Key'
- [x] `@post`
- [ ] `@invariant`  
       - [x] add 'dest stays a valid Key'  
       - [x] add 'source stays a valid Key'
- [ ] `@param` for every parameter  
       - [ ] move in front of return values
- [ ] `@return` / `@retval`  
       - [ ] if **meta-information** was successfully copied
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] add `keyCopyMeta()`

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

- [x] Function code is fully covered by tests
- [ ] All possible error states are covered by tests  
       - [x] add test for dest NULL  
       - [x] add test for source NULL
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

## Other Issues discovered (unrelated to function)
