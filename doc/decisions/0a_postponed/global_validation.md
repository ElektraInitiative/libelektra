# Global Validation

## Problem

Links (`fallback` and `override`) and validation constraints might point to keys not loaded with the respective `kdbGet`.

## Constraints

- no workaround such as `kdb get -a`

## Assumptions

- it is too expensive to always load all keys
  (as some backends are not cacheable)

## Considered Alternatives

- global plugin can register additional backends to load
- split `kdbGet` into multiple steps:
  1. do `kdbGet` on `system:/elektra` to update mount points
  2. first do `kdbGet` on the `spec:/`-namespace
  3. then calculate which backends are needed
  4. then fetch all backends as needed
- split loops in `kdbGet` not only according placements but also according namespace (get spec first)

## Decision

Implementation delayed after 1.0.

For now, the specification can only refer to what applications request by `kdbGet`.

## Rationale

- problems in implementing to always get everything
- it would make the parameter `parentKey` to `kdbGet` useless

## Implications

## Related Decisions

- [Internal Cache](../4_decided/internal_cache.md)

## Notes

- Was part of [improved KDB logic #1291](https://issues.libelektra.org/1291)
