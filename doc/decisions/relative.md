# Relative

## Problem

There is a different behavior of various plugins whether their name is
absolute or relative, including:

1. mounting the same file somewhere else does not work
2. importing somewhere else (other than from where it was exported) does not work
   (See [here](https://github.com/ElektraInitiative/libelektra/issues/51))

## Constraints

- at least the dump plugin must be able to handle its old files

## Assumptions

- it still will be easy to support a workflow that exports/imports
  everything
- mounting across namespaces (user/system) does not make sense

## Considered Alternatives

- allow relative/absolute plugins and mark them what they are, tools
  (e.g. import/export) use this knowledge and react accordingly.
  This would still not solve issue 1.)

## Decision

Key names shall be relative to parent Key name

## Rationale

Provides a better import/export/remount and also a more uniform
experience between different plugins.

## Implications

Plugins must be adapted to be relative as tracked
[here](https://github.com/ElektraInitiative/libelektra/issues/51).

## Related Decisions

None

## Notes
