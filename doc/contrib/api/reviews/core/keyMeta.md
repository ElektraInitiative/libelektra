# keyMeta

- start = 2021-01-23 18:05
- end = 2021-01-23 18:20
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`KeySet * keyMeta (Key * key)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] keyset -> KeySet
- [ ] Simple example or snippet how to use the function  
       - [ ] add
- [ ] Longer description of function containing common use cases  
       - [ ] what happens if metadata is read-only?
- [x] Description of functions reads nicely
- [ ] `@pre`  
       - [x] add
- [ ] `@post`  
       - [x] add
- [ ] `@invariant`  
       - [x] add
- [x] `@param` for every parameter
- [ ] `@return` / `@retval`  
       - [ ] move `@return` above `@retval`  
       - [ ] add `@retval` 0 if key has no metadata
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] split into two lines

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
- [ ] Wherever possible, function parameters should be `const`  
       - [ ] could key be const here? it doesn't get modified per se
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
       - [x] add check if key is NULL  
       - [x] add check if key has no metadata  
       - [x] add test if metadata is read-only
- All possible enum values are covered by tests
- [ ] No inconsistencies between tests and documentation

## Summary

Should this function work if metadata of Key is set to read-only?

## Other Issues discovered (unrelated to function)
