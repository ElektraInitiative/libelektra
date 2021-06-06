# keyAddName

- start = 2021-02-26 12:25
- end = 2021-02-26 18:10
- moderator = Stefan Hanreich <stefanhani@gmail.com>
- reviewer = Michael Tucek <michael@tucek.eu>

## Signature

`ssize_t keyAddName(Key *key, const char *addName)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] Add an already escaped key name part to the key name
- [ ] Simple example or snippet how to use the function  
       - [ ] add very simple example, with one simple key name part without dots
- [ ] Longer description of function containing common use cases  
       - [ ] add common use cases (append to a key name,..)
- [ ] Description of functions reads nicely  
       - [ ] move examples to bottom  
       - [ ] passed name below brief description  
       - [ ] replace list with reference to canonicalization docs  
       - [ ] 'key name' rules no space before dot
- [ ] `@pre`  
       - [ ] move above `@param`  
       - [ ] writeable key  
       - [ ] newName must be a valid name (reference to key name rules)  
       - [ ] move "It is not allowed to" precondition
- [ ] `@post`  
       - [ ] newName has been added to key
- [ ] `@invariant`  
       - [ ] add
- [ ] `@param` for every parameter  
       - [ ] move below `@pre`  
       - [ ] key - pointer to Key object  
       - [ ] newName - name part to append to `key`'s name
- [ ] `@return` / `@retval`  
       - [ ] move below `@param`  
       - [ ] move `@retval` into list  
       - [ ] make error cases differentiable (key errors, name errors)?
- [ ] `@since`  
       - [ ] move lower
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] `keyAddBaseName()`  
       - [ ] `keySetName()`  
       - [ ] `keySetBaseName()`

### Naming

- Abbreviations used in function names must be defined in the
  [Glossary](/doc/help/elektra-glossary.md)
- [x] Function names should neither be too long, nor too short
- [ ] Function name should be clear and unambiguous  
       - [ ] Add -> Append
- Abbreviations used in parameter names must be defined in the
  [Glossary](/doc/help/elektra-glossary.md)
- [x] Parameter names should neither be too long, nor too short
- [ ] Parameter names should be clear and unambiguous  
       - [ ] newName -> nameParts

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
       - [ ] order is different
- [ ] No functions with similar purpose exist  
       - [ ] `keyAddName()` merge with `keySetName()`

### Memory Management

- Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [ ] Function code is fully covered by tests  
       - [x] check if we can remove uncovered code  
       - [x] Line 542
- [ ] All possible error states are covered by tests  
       - [x] add test case for read-only key  
       - [x] test invalid name
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

## Other Issues discovered (unrelated to function)

- What is a valid Key exactly? Or does it just mean a valid pointer?
- Different Return Vlaues for different errors
- Set Doxygen Options to sort after header file
