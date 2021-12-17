# Binary

## Problem

binary

- is the only metadata that allows more values (instead of making the validation stricter)
- is the only API that modifies metadata

## Constraints

- simplify API

## Assumptions

binary values are a rarely used feature

## Considered Alternatives

- flag for indicating if a value is binary

## Decision

- make keys binary per default
- all current types are not binary, so type = string is the way to indicate a string doesn't
  - contain null bytes
  - is not null

## Rationale

## Implications

## Related Decisions

## Notes
