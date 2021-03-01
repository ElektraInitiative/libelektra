# keyCopy

- start = 2021-02-13 12:00
- end = 2021-02-13 13:10
- moderator = Stefan Hanreich <stefanhani@gmail.com>
- reviewer = Markus Raab <markus@libelektra.org>

## Signature

`int keyCopy(Key *dest, const Key *source);`

## Checklist

### Doxygen

- [ ] first line explains briefly what the function does  
       - [x] Clear -> clear  
       - [x] do not instantly reference `keyDup`
- [ ] `@see`  
       - [x] add `@see` for `keyDup`
- [ ] `@since` (optional: for after 1.0.0)  
       - [x] add `@since` 0.9.5
- [x] `@ingroup`
- [ ] `@retval`  
       - [x] use retval instead of return (see snippet below)
- [ ] good example or snippet how to use the function  
       - [ ] move example to `keyDup`
- [ ] simple examples go first  
       - [x] move simple example to the top
- [ ] Precondition  
       - [x] add precondition (valid key values)  
       - [x] @pre dest must be a valid Key (created with keyNew)  
       - [x] @pre source must be a valid Key or NULL
- [ ] Postcondition  
       - [x] add postcondition  
       - [x] @post Value is written to key dest
- [ ] Invariant  
       - [x] add invariant  
       - [x] Key name stays valid (document at struct)  
       - [x] Key name stays valid until delete
- [ ] `@param` for every parameter  
       - [x] better describe flags (|)

@copydoc for invariants?

```
@retval dest
@retval NULL in case of error
```

### Naming

- [x] Abbreviations used in function names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)
      src/libs/elektra/symbols.map
- [x] Function names should neither be too long, nor too short
- [x] Function name should be clear and unambiguous
- [ ] Abbreviations used in parameter names must be defined in the
      [Glossary](/doc/help/elektra-glossary.md)  
       - [x] add dest to glossary - maybe rename dest to destination?
- [x] Parameter names should neither be too long, nor too short
- [x] Parameter names should be clear and unambiguous

### Compatibility

- [x] ABI/API changes are forward-compatible (breaking backwards-compatibility
      to add additional symbols is fine)

### Parameter & Return Types

- Functions should return the most specific type possible
- Functions should require the most general type possible
- [x] Function parameters should use enum types instead of boolean types
      wherever sensible
- [x] Wherever possible, function parameters should be `const`
- [x] Wherever possible, return types should be `const`
- [x] Functions should have the least amount of parameters feasible

### Structural Clarity

- [ ] Functions should do exactly one thing  
       - [x] remove keySetName keySetRaw ksClear functionality from keyCopy?
- [x] Function name has the appropriate prefix
- [x] Signature in kdb.h.in has same order as Doxygen docu
- [ ] No functions with similar purpose exist  
       - [x] keyCopyAllMeta

### Memory Management

- [x] Memory Management should be handled by the function wherever possible

### Extensibility

- [x] Function is easily extensible, e.g., with flags
- [ ] Documentation does not impose limits, that would hinder further extensions  
       - [x] KEY_CP_ALL equivalent to KEY_CP_NAME | KEY_CP_VALUE | KEY_CP_META

### Tests

- [ ] Added functions are fully covered by tests
      https://doc.libelektra.org/coverage/master/debian-buster-full/src/libs/elektra/key.c.gcov.html
- [ ] All possible error states are covered by tests and documented
- [ ] All possible enum values are covered by tests  
       - [x] use flags other than KEY_CP_ALL
- [ ] No inconsistencies between tests and documentation
- [ ] Functions should have no side effects (idempotency)

## Summary

## Other Issues discovered (unrelated to function)
