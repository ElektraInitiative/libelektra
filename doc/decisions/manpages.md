# Manpages

## Problem

Our manpages are written as Markdown in doc/help and then converted to roff and stored in doc/man.
Storing generated files is annoying, as it requires:

- developers to always update generated files if the sources are changed
- developers not committing irrelevant changes to generated files (e.g. as may occur with different CMAKE_INSTALL_PREFIX etc.)
- require extra effort for continuous integration, e.g. [#4542](https://issues.libelektra.org/4542)
- ronn-ng, which doesn't have packages on most distribution (violates constraint 2.)

## Constraints

1. we want beautiful rendered man pages, e.g., OPTIONS section looks like normal man pages
2. we cannot require rare tools for the build process

## Assumptions

## Considered Alternatives

1. Write a tool that converts our specification, similar to [pythongen](src/tools/pythongen/template/template.man)
2. Write a tool that parses our `--help` output
3. [help2man](https://www.gnu.org/software/help2man/)
4. Doxygen: will be rechecked in the latest version, see [#4551](https://issues.libelektra.org/4551)
5. Pandoc 2.9: doesn't have the Markdown description-list feature¹ like ronn-ng has (violates constraint 1.)

## Decision

## Rationale

## Implications

## Related Decisions

- []()
- []()
- []()

## Notes

Currently no solution found, we recheck assumptions and/or check out new tools.

¹ronn-ng description-list feature converts:

```
- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile <profile>`:
  Use a different kdb profile.
```

to:

```
OPTIONS
       -H, --help
              Show the man page.

       -V, --version
              Print version info.

       -p, --profile <profile>
              Use a different kdb profile.
```
