# Lookup Every Key

## Problem

On structures like maps or [arrays](../5_partially_implemented/array.md) there are different possibilities which keys are looked up in the KeySet and which are simply iterated.

Without any guidelines, applications would provide arbitrary inconsistent behavior.

## Constraints

## Assumptions

- Applications that have good reasons to ignore the guidelines (e.g. they only read from one namespace), are allowed to do so.

## Considered Alternatives

- only lookup the roots and then iterate over the next keys
- let the applications do what they want without any guideline

## Decision

Every key that an application wants to use, must be looked up with `ksLookup` using a cascading lookup key.

## Rationale

- very simple rule, easy to understand, easy to follow
- provides consistent behavior (`spec` is always honored)
- `ksLookup` is quite cheap as it has only a few simple loops, only one allocation and less than 10% of CPU time in profiling, even in very simple applications with many lookups.

## Implications

Needs some helper functions or support in bindings as it is a bit tricky to implement e.g. for arrays.

## Related Decisions

- [Arrays](../5_partially_implemented/array.md)

## Notes
