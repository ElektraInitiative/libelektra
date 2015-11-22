# Library Split

## Issue

Various source files of different:

- users
- version evolution

are all linked together to libelektra.org.

## Constraints

- have a legacy libelektra that is identical to current behaviour
- full and static libraries remain unchanged

## Assumptions

- linker overhead of a dozen libraries is not measurable

## Considered Alternatives

- keep current situation

## Decision

- split libraries

## Argument

- allows various users (plugins, applications) to link to exactly what they need
- allows symbol versioning on different levels (for different evolving libraries)
- allows alternative implementation of parts of Elektra, e.g. a new libkdb which still uses libcore
- easier to navigate in source tree
- facilitates code reuse between plugins
- easier entrance to create commonly used libs (no fear of "changes in the core" for some helper functionality)

## Implications

- maybe to bindings?
- plugins and applications need changes in order to profit from less dependencies

## Related decisions

- none

## Notes

- was discussed in a meeting, all were in favour (or did not say anything)

Sizes of libelektra.so

- 0.8.14 unmodified 128K
- 0.8.14 w/o meta.c 120K
- 0.8.14 w/o array  126K

So the removal of the non-core functionality is actually relevant.
And kdb/core will nearly be half-split.
