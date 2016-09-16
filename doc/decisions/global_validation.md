# Global Validation

## Issue

Links and validation constraints might point to keys not loaded
with the respective `kdbGet`.

## Constraints

- no workaround such as `kdb get -a`

## Assumptions

## Considered Alternatives

- global plugin can register additional backends to load
- split `kdbGet` into multiple steps:
  1. first do `kdbGet` on the `spec/`-namespace
  2. then calculate which backends are needed
  3. then fetch all backends as needed
- split loops in `kdbGet` not only according placements
  but also according namespace (get spec first)


## Decision

## Argument

## Implications

## Related decisions

## Notes
