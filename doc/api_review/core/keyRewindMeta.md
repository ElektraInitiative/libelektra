# keyRewindMeta

- start = 2021-02-27 17:00
- end = 2021-02-27 17:10
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`int keyRewindMeta(Key *key)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [ ] First line explains briefly what the function does - [ ] first entry in meta-information
- [ ] Simple example or snippet how to use the function - [ ] move up a bit
- [x] Longer description of function containing common use cases
- [ ] Description of functions reads nicely - [ ] Use it -> Use this function - [ ] -then - [ ] So you want -> Afterwards, when you want to iterate the meta-information, you have - [ ] to **use** `keyNextMeta()`
- [ ] `@pre` - [ ] add 'key must be a valid key'
- [ ] `@post` - [ ] add 'key stays valid' - [ ] add 'internal iterator set to first entry in metadata'
- [ ] `@invariant` - [ ] add
- [ ] `@param` for every parameter - [ ] key: Key whose internal iterator should be rewinded
- [ ] `@return` / `@retval` - [ ] move success to normal `@return`
- [ ] `@since` - [ ] add
- [x] `@ingroup`
- [ ] `@see` - [ ] split first line into two

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

- [x] Function parameters should use enum types instead of boolean types
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
- [ ] All possible error states are covered by tests - [ ] add test for null pointer
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

## Other Issues discovered (unrelated to function)
