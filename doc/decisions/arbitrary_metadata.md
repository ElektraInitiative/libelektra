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

## Decision

Use different storage plugins, or plugins with different configurations,
for the `spec` namespace:

- `ni`
- TOML with `meta` configuration

## Rationale

- We do not need a storage plugin suitable for everything.
- The problems that internal metadata ends up in configuration files disappears.

## Implications

We need to have different default plugins in `spec` than in the other namespaces.

## Related Decisions

## Notes
