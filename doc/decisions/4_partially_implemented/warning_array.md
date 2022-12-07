# Arrays

## Problem

Currently multiple warnings are saved in an elektra non-conforming array notation which is limited to 100 entries.
The notation of `#00` is against the design [decision made](../4_partially_implemented/array.md).

## Constraints

## Assumptions

## Considered Alternatives

## Decision

The format should be aligned with the correct array notation, starting with `#0`.
The maximum number of warnings will stay at 100 entries (`#0` - `#_99`).

## Rationale

## Implications

To keep the ordering we add an underscore `_` once we go to beyond 10 warnings (`#_10`).

## Related Decisions

- [Array](../4_partially_implemented/array.md)

## Notes
