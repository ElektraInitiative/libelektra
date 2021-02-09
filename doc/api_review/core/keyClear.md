- start = 2021-01-23 18:10
- end = 2021-01-23 18:10
- reviewer = Stefan Hanreich <stefanhani@gmail.com>


## Checklist

### Documentation

- [ ] Change is mentioned in the Compatibility section of the release notes
- [ ] No inconsistencies between documentation and tests
- [ ] Proper Documentation of thread-safety of function
- [ ] All possible error states are documented
- [ ] Configuration flags are documented

#### Doxygen

- [ ] Precondition / Postcondition / Invariant
- [ ] `@retval`
- [ ] `@see`
- [ ] `@since`
- [ ] `@ingroup`
- [ ] there is at least one example or snippet how to use the function

### Naming

- [ ] Abbreviations used in parameter names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)
- [ ] Abbreviations used in function names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)
- [ ] Parameter names should neither be too long, nor too short
- [ ] Function names should neither be too long, nor too short
- [ ] Function name should be clear and unambiguous
- [ ] Parameter names should be clear and unambiguous

### Compatibility

- [ ] [Symbol versioning](/doc/dev/symbol-versioning.md)
      is correct for breaking changes
- [ ] ABI/API changes are forward-compatible (breaking backwards-compatibility
      to add additional symbols is fine)

### Parameter & Return Types

- [ ] Functions should return the most specific type possible
- [ ] Functions should require the most general type possible
- [ ] Functions should use constant types instead of boolean types
      wherever sensible
- [ ] Wherever possible, function parameters should be `const`
- [ ] Wherever possible, return types should be `const`
- [ ] Functions should have the least amount of parameters feasible

### Structural Clarity

- [ ] Functions should do exactly one thing
- [ ] Functions should have no side effects
- [ ] Configuration options are named constants
- [ ] Function belongs to appropriate group

### Memory Management

- [ ] Memory Management should be handled by the function wherever possible
- [ ] Every memory allocation should be documented

### Extensibility

- [ ] Function is easily extensible with function flags (constants)
- [ ] Documentation does not impose limits, that would hinder further extensions

### Tests

- [ ] Added functions are fully covered by tests
- [ ] All possible error states are covered by tests
- [ ] All possible configuration options are covered by tests
- [ ] Function body has full test coverage
- [ ] No inconsistencies between tests and documentation


## Summary
