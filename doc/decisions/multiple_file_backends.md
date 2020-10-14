# Multiple File Backends

## Problem

- For XDG in the `system` namespace may contain several files (XDG_CONFIG_DIRS)
- Unreachable sources (e.g. `curl`) need some fallback
- As fallback source if some data cannot be stored in some format

## Constraints

## Assumptions

## Considered Alternatives

## Decision

Multiple File Backends are not supported as they:

- make it impossible for admins to modify content of all files
- do not work atomically (a journal would be needed)
- do not work together with mmap

Instead all but the first of the files in `XDG_CONFIG_DIRS` are ignored
and unreachable sources yield an error.

## Rationale

## Implications

## Related Decisions

## Notes
