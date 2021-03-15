# ksLookup

- start = 2021-03-14 13:00
- end = 2021-03-14 13:20
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`Key *ksLookup(KeySet *ks, Key *k, elektraLookupFlags options)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [x] First line explains briefly what the function does
- [ ] Simple example or snippet how to use the function
      - [ ] add
- [ ] Longer description of function containing common use cases
      - [ ] only example uses `ksLookupByName`
- [x] Description of functions reads nicely
- [ ] `@pre`
      - [ ] add
- [ ] `@post`
      - [ ] add
- [ ] `@invariant`
      - [ ] add
- [x] `@param` for every parameter
- [ ] `@return` / `@retval`
      - [ ] remove '0 otherwise' from `@return`
- [ ] `@since`
      - [ ] add
- [ ] `@ingroup`
      - [ ] add
- [x] `@see`

### Naming

- [ ] Abbreviations used in function names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)
      - [ ] define `ks`
- [ ] Function names should neither be too long, nor too short
      - [ ] change name to `ksLookupByKey` to fit `ksLookupByName()`?
- [x] Function name should be clear and unambiguous
- [ ] Abbreviations used in parameter names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)
      - [ ] define `ks`
- [ ] Parameter names should neither be too long, nor too short
      - [ ] `ks` -> `keySet`
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
- [ ] Order of signatures in kdb.h.in is the same as Doxygen
- [x] No functions with similar purpose exist

### Memory Management

- [x] Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [x] Function code is fully covered by tests
- [x] All possible error states are covered by tests
- [ ] All possible enum values are covered by tests
      - [ ] `KDB_O_DEL`
- [x] No inconsistencies between tests and documentation

## Summary
- [ ] Maybe convert some documentation from here to snippets
and reference it in `ksLookupByName()`

## Other Issues discovered (unrelated to function)
