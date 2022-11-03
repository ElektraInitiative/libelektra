# Global Validation

## Problem

Links and validation constraints might point to keys not loaded
with the respective `kdbGet`.

## Constraints

- no workaround such as `kdb get -a`

## Assumptions

## Considered Alternatives

- global plugin can register additional backends to load
- split `kdbGet` into multiple steps:
  1. do `kdbGet` on `system:/elektra` to update mount points
  2. first do `kdbGet` on the `spec:/`-namespace
  3. then calculate which backends are needed
  4. then fetch all backends as needed
- split loops in `kdbGet` not only according placements
  but also according namespace (get spec first)

## Decision

Not supported, admins/maintainers need to stay with their spec within what applications request by `kdbGet`.

## Rationale

- it is too expensive to always load all keys (@mpranj: is this true?)
- problems in implementing to always get everything
- it makes the parameter to `kdbGet` basically useless

## Implications

## Related Decisions

- [Internal Cache](../0_drafts/internal_cache.md)

## Notes

see #1291
