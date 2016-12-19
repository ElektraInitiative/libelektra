# Default Values

## Issue

- KeySet might get modified on access (hash rebuilds)
- Expectation that already all keys are there after `kdbGet()`
- No default value calculation

## Constraints

- Should work with dynamic search for keys

## Assumptions

## Considered Alternatives

## Decision

- spec-plugin does a lookup for values (Maybe also resolving missing fallback/override links?)

## Argument

## Implications

## Related decisions

## Notes

- #533
- #972
