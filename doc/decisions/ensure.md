# Ensure

## Problem

Applications want to ensure that some functionality (=global plugin)
is present in Elektra.

## Constraints

- keep notifications and highlevel API as-is

## Assumptions

## Considered Alternatives

- integrate in kdbOpen

## Decision

Keep kdbEnsure but:

- reduce for only global plugins, as the partial other functionality is confusing
  and not needed
- find solution that list plugin is not needed
- only as implementation detail below libraries (e.g. like done for notification)

## Rationale

- allows interfaces that let the user do `kdbOpen`

## Implications

- gopts needs a nice wrapper library (low-prior)

## Related Decisions

- [Global Plugins](global_plugins.md)

## Notes
