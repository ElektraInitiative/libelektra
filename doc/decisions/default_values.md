# Default Values

## Problem

- KeySet might get modified on access (hash rebuilds)
- Expectation that already all keys are there after `kdbGet()`
- No default value calculation

## Constraints

- Should work with dynamic search for keys

## Assumptions

## Considered Alternatives

## Decision

- spec-plugin does a lookup for values (Maybe also resolving missing fallback/override links?)

## Rationale

## Implications

## Related Decisions

## Notes

- #533
- #972
