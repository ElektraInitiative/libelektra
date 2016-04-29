# Vendor Spec

## Issue

Vendors (distributors, administrators) might want to modify the specification.
gsettings has a similar feature.

## Constraints

There are many constraints in providing such a feature because it is possible
to get an inconsistent or unusable specification.

## Assumptions

Developers who elektrify their applications do care about good integration
and being administer friendly.

## Considered Alternatives

- implementing a new namespace that gets merged
- merge specification files during installation

## Decision

Provide means that a single specification can satisfy every distribution and administrator.

## Argument

- Elektra wants to reduce fragmentation, and vendor specific changes obviously is a severe
  kind of fragmentation
- providing vendor overrides/fallbacks might be an excuse to not provide better typing or
  general overrides/fallbacks features which would avoid the need for a vendor overrides/fallbacks
  at all

## Implications

Provide means for a single specification to be very good integrated in every system.

## Related decisions

## Notes
