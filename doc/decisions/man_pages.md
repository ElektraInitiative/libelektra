# Man Pages

## Problem

Our man pages are written as Markdown in doc/help and then converted to roff and stored in doc/man.
This was a workaround, because `ronn-ng` isn't available on most distributions and we want to avoid packages without man pages are being built.
Storing generated files is annoying, as it requires:

- developers to always update generated files if the sources are changed
- developers not committing irrelevant changes to generated files (e.g. as may occur with different CMAKE_INSTALL_PREFIX etc.)
- require extra effort for continuous integration, e.g. [#4542](https://issues.libelektra.org/4542)

## Constraints

1. we want beautiful rendered man pages, e.g., OPTIONS section looks like normal man pages, see in Notes¹ below
2. we cannot require rare tools for the build process: the man pages must be present in every package

## Assumptions

## Considered Alternatives

0. `ronn-ng`, which doesn't have packages on most distribution (violates constraint 2.) and thus created this problem
1. Write a tool that converts our specification, similar to [pythongen](/src/tools/pythongen/template/template.man)
2. Write a tool that parses gopts `--help` output
3. [help2man](https://www.gnu.org/software/help2man/)
4. Doxygen:
   - Constraint 1 probably broken
5. Pandoc:
   - has a few standard dependencies
   - would need rewrite of the current documentation in doc/help:
     - To fulfill Constraint 1 [definition lists](https://pandoc.org/MANUAL.html#definition-lists) would be needed
     - would need YAML metadata/front matter for every file
       (It would be possible, but not advisable, to:
       - also pass information as command-line arguments via `--variable` but then we would move meta-information about man pages to the build system
       - that we use the current (non-standard) front matter and convert it to Pandoc's frontmatter but this makes the build system more complicated)

## Decision

Not yet done except spelling of man pages, see [#4567](https://issues.libelektra.org/4567).

## Rationale

## Implications

## Related Decisions

- []()
- []()
- []()

## Notes

¹ ronn-ng converts:

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
