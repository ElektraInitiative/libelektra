# keyUnescapedName

- start = 2021-02-26 11:15
- end = 2021-02-26 12:15
- moderator = Stefan Hanreich <stefanhani@gmail.com>
- reviewer = Michael Tucek <michael@tucek.eu>

## Signature

`const void *keyUnescapedName(const Key *key)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] Merge first and second sentence  
       'Returns a key's name where the key name parts are separated by null bytes and does not use backslash for escaping'
- [ ] Simple example or snippet how to use the function  
       - [ ] add example
- [x] Longer description of function containing common use cases
- [x] Description of functions reads nicely
- [ ] `@pre`  
       - [ ] add
- [ ] `@post`  
       - [ ] add  
       - [ ] key stays unchanged  
       - [ ] returns pointer to unescaped Name
- [ ] `@invariant`  
       - [ ] add  
       - [ ] key stays unchanged
- [ ] `@param` for every parameter  
       - [ ] 'optional pointer to Key object'
- [ ] `@return` / `@retval`  
       - [ ] swap order  
       - [ ] 'if Key has no name'  
       - [ ] 'the Key's name'
- [ ] `@since`  
       - [ ] add
- [ ] `@ingroup`  
       - [ ] add
- [ ] `@see`  
       - [ ] keyGetUnescapedName() for getting a copy

### Naming

- Abbreviations used in function names must be defined in the
  [Glossary](/doc/help/elektra-glossary.md)
- [x] Function names should neither be too long, nor too short
- [ ] Function name should be clear and unambiguous  
       - [ ] `keyGetUnescapedName()` make difference to this function more clear?
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
       - [ ] mismatch, move up in docs to match place in header file
- [x] No functions with similar purpose exist

### Memory Management

- Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [ ] Function code is fully covered by tests  
       - [x] no tests for this function
- [ ] All possible error states are covered by tests  
       - [x] no tests for this function
- [ ] All possible enum values are covered by tests  
       - [x] no tests for this function
- [ ] No inconsistencies between tests and documentation  
       - [x] no tests for this function

## Summary

- Why is return type void\* exactly?
- include optional in key `@param` - define schema for key param
- Add explicit tests for this function

## Other Issues discovered (unrelated to function)

- Convention for keyX / keyGetX (pointer vs copy) intended?
- Remove elektra-internal functions
- Functions are sorted alphabetically - can we change this easily?
