# Manpages

## Problem

Our manpages are written as markdown in doc/help and then converted to roff and stored in doc/man.
Storing generated files is annoying, as it requires:

- developers to always update generated files if the sources are changed
- developers not committing irrelevant changes to generated files (e.g. as may occur with different CMAKE_INSTALL_PREFIX etc.)
- require extra effort for continuous integration, e.g. [#4542](https://issues.libelektra.org/4542)

## Constraints

1. we want beautiful rendered man pages
2. we cannot require rare tools for the build process

## Assumptions

- ronn-ng doesn't have packages on any distribution (violates constraint 2.)

## Considered Alternatives

1. Doxygen: need to be rechecked in the latest version, see [#4551](https://issues.libelektra.org/4551)
2. Pandoc 2.9 doesn't have the Markdown description-list feature like ronn-ng has (violates constraint 1.)

## Decision

## Rationale

## Implications

## Related Decisions

- []()
- []()
- []()

## Notes

Currently no solution found, please recheck assumptions and/or check out new tools.
