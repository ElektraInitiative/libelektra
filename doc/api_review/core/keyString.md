# keyString

- start = 2021-02-26 16:05
- end = 2021-02-26 16:40
- moderator = Stefan Hanreich <stefanhani@gmail.com>
- reviewer = Robert Sowula

## Signature

`const char *keyString(const Key *key)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] Get the pointer to the string representing the Key's value.
- [ ] Simple example or snippet how to use the function  
       - [ ] add example
- [ ] Longer description of function containing common use cases  
       - [ ] add explanation about pointers  
       - [ ] add explanation about modifications from the user
- [ ] Description of functions reads nicely  
       - [ ] `(null)` -> `"(null)"`  
       - [ ] `(binary)` -> `"(binary)"`
- [ ] `@pre`  
       - [x] add?
- [ ] `@post`  
       - [x] add?
- [ ] `@invariant`  
       - [x] add?
- [ ] `@param` for every parameter  
       - [ ] move before return  
       - [ ] key: key -> Key
- [ ] `@return` / `@retval`  
       - [ ] pointer to the string representing the Key's value  
       - [ ] `(null)` -> `"(null)"`  
       - [ ] `(binary)` -> `"(binary)"`
- [ ] `@since`  
       - [ ] add `1.0.0`
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] `keyGetString()`  
       - [ ] `keyGetBinary()`  
       - [ ] `keyValue()`

### Naming

- Abbreviations used in function names must be defined in the
  [Glossary](/doc/help/elektra-glossary.md)
- [x] Function names should neither be too long, nor too short
- [ ] Function name should be clear and unambiguous  
       - [ ] `keyString()` vs `keyGetString()`
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

- [ ] Functions should do exactly one thing  
       - [ ] Return Values (null) and (binary)
- [x] Function name has the appropriate prefix
- [ ] Order of signatures in kdb.h.in is the same as Doxygen  
       - [ ] swapped `with keyGetValueSize()`
- [x] No functions with similar purpose exist

### Memory Management

- [x] Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [ ] Documentation does not impose limits, that would hinder further extensions  
       - "(null)" & "(binary)"

### Tests

- [ ] Function code is fully covered by tests  
       - [x] Line 198  
       - [x] Line 203
- All possible error states are covered by tests
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

Think about changing the return values for 1.0.0. It seems like a hard step,
but also inevitable to do at some point. Now would probably be better than in
the future.

## Other Issues discovered (unrelated to function)
