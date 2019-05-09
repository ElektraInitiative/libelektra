# Arrays

## Problem

Currently multiple warnings are saved in an elektra non-conforming array
notation which is limited to 100 entries. The notation of `#00` is against
the design [decision made](array.md).

Furthermore if the number of warnings is exceeded, the earliest entries are overwritten
which might yield the most useful warning messages.

## Constraints

- The number of entries should stay limited but may be more than 100 entries.

## Assumptions

## Considered Alternatives

## Decision

The format should be aligned with the correct array notation,
starting with `#0`. The maximum number of warnings will be expanded to
1001 entries (`#0` - `#1000`).

If more warnings occur, warning `#1000` is overwritten each time.

## Rationale

## Implications

## Related Decisions

- [Array](array.md)

## Notes
