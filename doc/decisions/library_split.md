# Library Split

## Problem

libelektra.so contains KDB and Key(Set) but plugins only need Key(Set) but not KDB.

## Constraints

- full and static libraries remain unchanged

## Assumptions

- linker overhead of less than a dozen libraries is not measurable

## Considered Alternatives

- keep current situation

## Decision

- [split library](/src/libs)

## Rationale

- allows various users (plugins, applications) to link to exactly what they need
- allows symbol versioning on different levels (for different evolving libraries)
- allows alternative implementation of parts of Elektra, e.g. a new libkdb which still uses libcore
- easier to navigate in source tree
- facilitates code reuse between plugins
- easier entrance to create commonly used libs (less fear of "changes in the core" for some helper functionality)

## Implications

- bindings need to decide which parts to link against
- plugins and applications need changes
- we need to get rid of the legacy library

## Related Decisions

- none

## Notes

- was discussed in a meeting, all were in favor (or did not say anything)

Sizes of libelektra.so

- 0.8.14 unmodified 128K
- 0.8.14 w/o meta.c 120K
- 0.8.14 w/o array 126K

So the removal of the non-core functionality is actually relevant.
And kdb/core is now nearly half-split:

- 52K lib/libelektra-core.so.0.8.14
- 7,5K lib/libelektra-ease.so.0.8.14
- 1,3M lib/libelektra-full.so.0.8.14
- 76K lib/libelektra-kdb.so.0.8.14
- 9,3K lib/libelektra-meta.so.0.8.14
- 5,1K lib/libelektra-plugin.so.0.8.14
- 7,6K lib/libelektra-proposal.so.0.8.14
