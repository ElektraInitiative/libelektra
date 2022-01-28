# Bootstrap

## Problem

Currently, the default backend (default.ecf) will also be used for bootstrapping. There are two problems with this approach:

1. Thus the default backend first will be read with parentKey `system:/elektra` and later with parentKey `system:/`, it needs to store absolute paths and thus won't work with most of the plugins (except dump).
2. When `system:` is large without mount points, everything is reread twice during bootstrapping.

## Constraints

- Bootstrap should be fast and not unnecessarily read large files

## Assumptions

## Considered Alternatives

- Implement a hack so that `system:/elektra` is actually read as `system:/`. (Will not solve problem 2.)
  - It's a hack.
  - Its confusing and does not play well with persistent data with relative key names.
- Split up without compatibility mode: would need to migrate all mount points by exporting (with old version!) and then importing (with new version!)
  - I consider this too error-prone, people might easily forget to export with the old version and then discard their mount points unintentional.

## Decision

Split up the concepts of default (`default.ecf`) and bootstrap (`elektra.ecf`) backend.
During bootstrap only `elektra.ecf` is read.
The default backend reading `default.ecf` is only relevant as long as no root backend is mounted.

Algorithm:

1. try to get system:/elektra using the file elektra.ecf (KDB_DB_INIT)
2. mount the init backend to system:/elektra

## Rationale

## Implications

## Related Decisions

## Notes

Added scripts/upgrade-bootstrap to migrate from previous setups
to upgrade to new system, either:

- touch /etc/kdb/elektra.ecf (loses old mount points)
- or do kdb export system:/elektra/mountpoints, kdb rm -r system:/elektra/mountpoints, kdb import system:/elektra/mountpoints
