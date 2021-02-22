# keyGetNameSize

- start = 2021-02-22 13:10
- end = 2021-02-22 13:40
- moderator = Stefan Hanreich <stefanhani@gmail.com>
- reviewer = Philipp Gackstatter

## Signature

`ssize_t keyGetNameSize(const Key *key)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] only describes return value  
       - [ ] null-termination  
       - [ ] mit return mergen
- [ ] Simple example or snippet how to use the function  
       - [ ] add
- [ ] Longer description of function containing common use cases  
       - [ ] better describe common use case  
       - [ ] example for error case  
       - [ ] example for empty string
- [x] Description of functions reads nicely
- [ ] `@pre`  
       - [ ] add 'Key should be valid'
- [ ] `@post`  
       - [ ] add 'key doesn't get modified'
- [ ] `@invariant`  
       - [ ] add 'key doesn't get modified'
- [ ] `@param` for every parameter  
       - [ ] 'key object to get the size of the name from'  
       - [ ] make consistent
- [ ] `@return` / `@retval`  
       - [ ] without owner?
- [ ] `@see`  
       - [ ] add parentheses to keyGetUnescapedNameSize

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
- [x] Order of signatures in kdb.h.in is the same as Doxygen
- [x] No functions with similar purpose exist

### Memory Management

- Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [x] Function code is fully covered by tests
- [ ] All possible error states are covered by tests  
       - [ ] add test case for NULL pointer
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

try splitting ifs to two lines, for better coverage reports
