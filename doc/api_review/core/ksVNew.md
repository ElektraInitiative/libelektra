# ksVNew

- start = 2021-03-14 21:40
- end = 2021-03-14 22:15
- reviewer = Stefan Hanreich <stefanhani@gmail.com>

## Signature

`KeySet *ksVNew(size_t alloc, va_list ap)`

## Checklist

#### Doxygen

(bullet points are in order of appearance)

- [x] First line explains briefly what the function does
- [ ] Simple example or snippet how to use the function  
       - [ ] move up
- [x] Longer description of function containing common use cases
- [ ] Description of functions reads nicely  
       - [ ] 'you should read the next statements' seems a bit strange  
       - [ ] remove 'va the list of arguments' heading
- [ ] `@pre`  
       - [ ] move up  
       - [ ] add more
- [ ] `@post`  
       - [ ] add more
- [ ] `@invariant`  
       - [ ] add
- [ ] `@param` for every parameter  
       - [ ] delete the first one  
       - [ ] move below conditions / invariants  
       - [ ] improve description for alloc: clarify hint
- [x] `@return` / `@retval`
- [ ] `@since`  
       - [ ] add
- [ ] `@ingroup`  
       - [ ] add
- [ ] `@see`  
       - [ ] move to bottom  
       - [ ] add `keyNew()`?  
       - [ ] add `ksNew()`

### Naming

- [ ] Abbreviations used in function names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)  
       - [ ] define `ks`
- [x] Function names should neither be too long, nor too short
- [ ] Function name should be clear and unambiguous  
       - [ ] What does the `V` mean?
- [ ] Abbreviations used in parameter names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)  
       - [ ] define `ks`
- [x] Parameter names should neither be too long, nor too short
- [ ] Parameter names should be clear and unambiguous  
       - [ ] `alloc` -> `size` / `initialSize`  
       - [ ] `va` -> `params` / `parameters` / `va_list`

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
- [ ] No functions with similar purpose exist  
       - [ ] difference to `keyNew()`?

### Memory Management

- [x] Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [x] Documentation does not impose limits, that would hinder further extensions

### Tests

- [ ] Function code is fully covered by tests  
       - [x] Line 254  
       - [x] Line 270
- [ ] All possible error states are covered by tests  
       - [x] above error cases seem hard to test
- All possible enum values are covered by tests
- [x] No inconsistencies between tests and documentation

## Summary

Is this function still needed? Is it necessary to provide raw access, as keyNew
is just a wrapper for this function that performs actions, which the user has to
manually perform when using `keyVNew()`.

## Other Issues discovered (unrelated to function)
