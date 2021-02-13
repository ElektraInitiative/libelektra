# ksDup

- start = 2021-01-23 18:10
- end = 2021-01-23 18:10
- moderator = Stefan Hanreich <stefanhani@gmail.com>


## Signature
KeySet *ksDup(const KeySet * source)

## Checklist

#### Doxygen

- [ ] first line explains briefly what the function does
- [ ] `@see`
- [ ] `@since`
- [ ] `@ingroup`
- [ ] `@retval`
- [ ] good example or snippet how to use the function
- [ ] simple examples go first
- [ ] Precondition
- [ ] Postcondition
- [ ] Invariant
- [ ] `@param` for every parameter

### Documentation

- [ ] Change is mentioned in the Compatibility section of the release notes
- [ ] Proper Documentation of thread-safety of function
- [ ] All possible error states are documented
- [ ] Valid flags are documented

### Naming

- [ ] Abbreviations used in function names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)
- [ ] Function names should neither be too long, nor too short
- [ ] Function name should be clear and unambiguous
- [ ] Abbreviations used in parameter names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)
- [ ] Parameter names should neither be too long, nor too short
- [ ] Parameter names should be clear and unambiguous

### Compatibility

- [ ] [Symbol versioning](/doc/dev/symbol-versioning.md)
      is correct for breaking changes
- [ ] ABI/API changes are forward-compatible (breaking backwards-compatibility
      to add additional symbols is fine)

### Parameter & Return Types

- [ ] Functions should return the most specific type possible
- [ ] Functions should require the most general type possible
- [ ] Function parameters should use enum types instead of boolean types
      wherever sensible
- [ ] Wherever possible, function parameters should be `const`
- [ ] Wherever possible, return types should be `const`
- [ ] Functions should have the least amount of parameters feasible

### Structural Clarity

- [ ] Functions should do exactly one thing
- [ ] Function name has the appropriate prefix
- [ ] Signature in kdb.h.in has same order as Doxygen docu
- [ ] No functions with similar purpose exist

### Memory Management

- [ ] Memory Management should be handled by the function wherever possible

### Extensibility

- [ ] Function is easily extensible, e.g., with flags
- [ ] Documentation does not impose limits, that would hinder further extensions

### Tests

- [ ] Added functions are fully covered by tests
- [ ] All possible error states are covered by tests
- [ ] All possible enum values are covered by tests
- [ ] No inconsistencies between tests and documentation
- [ ] Functions should have no side effects (idempotency)


## Summary
