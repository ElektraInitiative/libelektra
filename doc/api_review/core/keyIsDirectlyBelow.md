# keyIsDirectlyBelow

- start = 2021-03-07 19:10
- end = 2021-03-07 19:20
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`int keyIsDirectlyBelow(const Key *key, const Key *check)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [x] First line explains briefly what the function does
- [ ] Simple example or snippet how to use the function
      - [ ] split examples
      - [ ] turn examples into proper code
- [ ] Longer description of function containing common use cases
      - [ ] move description from examples to documentation body
- [x] Description of functions reads nicely
- [ ] `@pre`
      - [ ] add
- [ ] `@post`
      - [ ] add
- [ ] `@invariant`
      - [ ] add
- [x] `@param` for every parameter
- [ ] `@return` / `@retval`
      - [ ] 'check is below key' -> 'check is directly below key'
- [ ] `@since`
      - [ ] add
- [x] `@ingroup`
- [ ] `@see`
      - [ ] split and add descriptions

### Naming

- Abbreviations used in function names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)
- [x] Function names should neither be too long, nor too short
- [x] Function name should be clear and unambiguous
- Abbreviations used in parameter names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)
- [x] Parameter names should neither be too long, nor too short
- [ ] Parameter names should be clear and unambiguous
      - [ ] the name of `check` might be improved

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
- [ ] Order of signatures in kdb.h.in is the same as Doxygen
      - [ ] swapped with `keyIsBelowOrSame()`
- [x] No functions with similar purpose exist

### Memory Management

- [x] Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [ ] Function code is fully covered by tests
      - [ ] Line 293
      - [ ] Line 326
- [ ] All possible error states are covered by tests
      - [ ] Line 293 seems to be checked
      - [ ] Line 326 seems to be checked
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

## Other Issues discovered (unrelated to function)
- [ ] `keyIsBelowOrSame` is not contained in Doxygen
- [ ] maybe merge `keyBelowFamily` into one function with flags
