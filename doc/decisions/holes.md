# Holes in KeySets

## Problem

Config files ideally do not copy any structure if they only want to
set a single key.

## Constraints

- strongly hierarchically structured data must still be supported

## Assumptions

## Considered Alternatives

- data structure is always complete
- to not allow "directory values" or "directory meta data"

## Decision

- support holes in KeySet
- See [hierarchy example](/src/bindings/cpp/examples/cpp_example_hierarchy.cpp)
  for how to implement a hierarchy that gets rid of the problem that holes
  might create when iterating over KeySet.

## Rationale

## Implications

## Related Decisions

## Notes
