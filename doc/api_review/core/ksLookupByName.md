# ksLookupByName

- start = 2021-03-14 12:35
- end = 2021-03-14 13:00
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`Key *ksLookupByName(KeySet *ks, const char *name, elektraLookupFlags options)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [x] First line explains briefly what the function does
- [ ] Simple example or snippet how to use the function - [ ] add
- [ ] Longer description of function containing common use cases - [ ] add part about options including reference to `elektraLookupFlags`
- [x] Description of functions reads nicely
- [ ] `@pre` - [ ] add
- [ ] `@post` - [ ] add
- [ ] `@invariant` - [ ] add
- [ ] `@param` for every parameter - [ ] shorten `options` by replacing explanation with reference to real
      real explanation
- [ ] `@return` / `@retval` - [ ] remove '0 otherwise' from `@return`
- [ ] `@since` - [ ] add
- [ ] `@ingroup` - [ ] add
- [ ] `@see` - [ ] remove first 'See Also' section

### Naming

- [ ] Abbreviations used in function names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md) - [ ] define `ks`
- [x] Function names should neither be too long, nor too short
- [x] Function name should be clear and unambiguous
- [ ] Abbreviations used in parameter names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md) - [ ] define `ks`
- [ ] Parameter names should neither be too long, nor too short - [ ] `ks` -> `keySet`
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
- [ ] All possible error states are covered by tests - [ ] `name == NULL`
- [ ] All possible enum values are covered by tests - [ ] `KDB_O_DEL`
- [ ] No inconsistencies between tests and documentation - [ ] several flags used in test, that cant be found in the documentation
      for `ksLookup()` or `elektraLookupFlags`

## Summary

- [ ] Maybe convert some documentation from `ksLookup()` to snippets
      and reference it in both

## Other Issues discovered (unrelated to function)
