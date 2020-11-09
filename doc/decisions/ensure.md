# Ensure

## Problem

Applications want to ensure that some functionality (=global plugin)
is present in Elektra.

## Constraints

- keep notifications and highlevel API as-is

## Assumptions

## Considered Alternatives

- integrate in kdbOpen:
  - can immediately build up correct plugin positioning
  * cannot be called later easily (needs `kdbClose` and `kdbOpen` again)
  * cannot be integrated with some APIs, e.g. `kdbDup` or current notification API
- specific APIs per plugin
  - difficult for application developers
  - every plugin would need to design new APIs

## Decision

Keep `kdbEnsure` but:

- reduce for only global plugins, as the partial other functionality is confusing
  and not needed
- find solution that list plugin is not needed
- only as implementation detail below libraries (e.g. like done for notification)

## Rationale

- allows APIs (that call `kdbEnsure`) while the user still calls `kdbOpen`, like done in the notification API:
  `kdb = kdbOpen(); APIcallDoingKdbEnsure (kdb)`
  (`APIcallDoingKdbEnsure` would not be possible if the functionality would be integrated in `kdbOpen`)
- allows applications to continue even if `kdbEnsure` failed
- is already implemented
- is already used by notification and high-level library
- gopts would continue to work as is but also a nice wrapper library could be added (low-prior)

## Implications

## Related Decisions

- [Global Plugins](global_plugins.md)

## Notes
