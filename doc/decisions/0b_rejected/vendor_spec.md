# Vendor Spec

## Problem

Vendors (distributors, administrators) might want to modify the specification.
`gsettings` has a similar feature.

## Constraints

There are many constraints in providing such a feature because it is possible to get an inconsistent or unusable specification.

## Assumptions

Developers who elektrify their applications do care about good integration and being administer friendly.

## Considered Alternatives

- implementing a new namespace that gets merged
- merge specification files during installation

## Decision

As found out during implementation of [specload](/src/plugins/specload), only a very limited subset can be modified safely, e.g.:

- add/edit/remove `description`, `opt/help` and `comment`

## Rationale

- Elektra wants to reduce fragmentation, and vendor specific changes obviously is a severe kind of fragmentation
- providing vendor overrides/fallbacks might be an excuse to not provide or general override/fallback features

## Implications

Provide means for a single specification to be very good integrated in every system.

## Related Decisions

## Notes
