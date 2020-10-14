# Arbitrary Metadata

## Problem

To make storage-plugins suitable for `spec` they need to be able to store
all the meta-data as specified in [METADATA.ini](/doc/METADATA.ini).
Most file formats do not have support for that.

## Constraints

## Assumptions

## Considered Alternatives

- store metadata in the comments like the `ini` plugin:
  this exposes internal meta-data into the comments and
  can drastically affect the readability of a storage file.
  Comments should never be touched by a parser.
-

## Decision

Use different storage plugins, or plugins with different configurations,
for the `spec` namespace:

- `ni`
- TOML with `meta` configuration

## Rationale

## Implications

## Related Decisions

## Notes
