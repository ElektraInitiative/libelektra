# High-level API

## Problem

Projects usually do not want to use low-level APIs.
`KDB` and `KeySet` is useful for plugins and to
implement APIs but not to be directly used in applications.

## Constraints

1. should be extremely easy to get started with
2. should be very hard to use it wrong
3. all high-level APIs should work together very nicely
   - same principles
   - same API style
   - same error handling
   - can be arbitrarily intermixed

## Assumptions

- Thread-safety: a handle is the accepted better solution than having to
  care about whether it is reentrant, thread-safe, ...
- assumes that spec is available (either by compiled-in `KeySet` or exit after elektraOpen)
- many projects do not care about some limitations (no binary, no metadata)
  but prefer a straightforward way to get/set config
- When people hit limitations they fall back to direct use of ^KeySet^, ^Key^

## Considered Alternatives

- storing errors in the handle:
  - Maintenance problem if error handling is added later
- only provide `KDB`, applications need to implement their own APIs:
  - reduces consistency of how the API is used
- only provide generated API
- assume type as `string` if not given
- force `default` to be part of parameters

## Decision

We provide 3 high-level C APIs:

1. libelektra-highlevel (generic key-value getter/setter)
2. libelektra-hierarchy (generic hierarchical getter/setter in a tree)
3. code generator (specified key-value getter/setter with function names,
   KeySets, or strings from specifications)

Furthermore, we will:

- have as goal that no errors in specified keys with default can occur
- if you use `elektraGetType` before getting a value, no error can occur when getting it later
- enforce that every key has a type
- use `elektraError` as extra parameter (for prototyping and examples you can pass 0)

## Rationale

1. Very easy to get started with, to get a key needs 3 lines of codes (without error handling):

   ```c
   Elektra *handle = elektraOpen ("/sw/elektra/kdb/#0/current", 0);
   printf ("number /mykey is " ELEKTRA_LONG_F "\n", elektraGetLong (handle, "/mykey"));
   elektraClose (handle);
   ```

2. It is also easier to get started with writing new bindings.
3. User can combine the different APIs.

## Implications

## Related Decisions

## Notes

https://issues.libelektra.org/1359
