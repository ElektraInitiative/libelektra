# keyGetName

- start = 2021-02-22 13:40
- end = 2021-02-22 14:20
- moderator = Stefan Hanreich <stefanhani@gmail.com>
- reviewer = Philipp Gackstatter

## Signature

`ssize_t keyGetName(const Key *key, char *returnedName, size_t maxSize)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does  
       - [ ] owner still recent?
- [ ] Simple example or snippet how to use the function  
       - [ ] move up
- [ ] Longer description of function containing common use cases  
       - [ ] maxSize special case has very long description (>50% of body)  
       - [ ] maxSize special case move description to pre / post / return values  
       - [ ] mention explicitly, that user has to manage their buffer  
       - [ ] move special case 'not enough space' to return values / pre / post
- [ ] Description of functions reads nicely  
       - [ ] remove / define abbreviated (make it more explicit)  
       - [ ] "Writes keyName of `key` into the provided buffer `returnedName` if it is shorter than `maxSize`"
- [ ] `@pre`  
       - [ ] `key` is a valid Key  
       - [ ] `returnedName` is a pre-allocated buffer  
       - [ ] maxSize is the size of `returnedName`
- [ ] `@post`  
       - [ ] `returnedName` contains the name of the Key  
       - [ ] `key` is not modified
- [ ] `@invariant`  
       - [ ] `key` is not modified
- [ ] `@param` for every parameter  
       - [ ] `returnedName`: pre-allocated buffer to write the key name into
- [ ] `@return` / `@retval`  
       - [ ] `1`: when only a null-terminator was written  
       - [ ] `-1`: then -> than  
       - [ ] `-1`: standardize / change 'keyname' / 'key name' or 'Key name'  
       - [ ] `-1` check if null pointer case is even still applicable  
       - [ ] `-1` split into two lines: maxSize / NULL pointer
- [ ] `@since`  
       - [ ] add
- [x] `@ingroup`
- [ ] `@see`  
       - [ ] keyName  
       - [ ] keyGetUnescapedName  
       - [ ] keyGetBaseName  
       - [ ] keyGetNamespace

### Naming

- Abbreviations used in function names must be defined in the
  [Glossary](/doc/help/elektra-glossary.md)
- [x] Function names should neither be too long, nor too short
- [ ] Function name should be clear and unambiguous  
       - [ ] `keyName` vs `keyGetName()`
- [ ] Abbreviations used in parameter names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)  
       - [ ] max - maybe just rename to `bufferSize`
- [ ] Parameter names should neither be too long, nor too short  
       - [ ] `returnedName` -> `name` / `nameBuffer`
- [ ] Parameter names should be clear and unambiguous  
       - [ ] `maxSize` -> `bufferSize`

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
       - wrong order
- [x] No functions with similar purpose exist

### Memory Management

- [ ] Memory Management should be handled by the function wherever possible  
       - [ ] pass variable and return size instead  
       - [ ] remove `maxSize` param

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [ ] Documentation does not impose limits, that would hinder further extensions  
       - [ ] nothing is written on maxSize < nameSize (would be fixed with
      memory management suggestion from above)

### Tests

- [ ] Function code is fully covered by tests  
       - [x] Lines 365 / 366
- [ ] All possible error states are covered by tests  
       - [x] test empty string
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

Different error codes for different errors

## Other Issues discovered (unrelated to function)
