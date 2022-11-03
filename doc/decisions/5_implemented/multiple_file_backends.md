# Multiple File Backends

Usually, a backend refers to exactly one file per namespace.
This file can be returned using `kdb file`.

## Problem

In some situations a single mountpoint refers to more than one file per namespace:

- For XDG in the `system` namespace may contain several files (XDG_CONFIG_DIRS).
- A fallback file if some data cannot be stored in some format (Idea from @kodebach:
  writing the same content to several files, merging when reading)

## Constraints

## Assumptions

## Considered Alternatives

## Decision

Multiple File Backends are not supported for now in the case of writing files.

Multiple sources in one namespace only work, if the fallback KeySet is
part of the mountpoint config. That way any change to the fallback
KeySet would essentially make the whole thing a different mountpoint
and thereby invalidate all guarantees.

## Rationale

Writeable multiple file backends would:

- make it impossible for admins to modify content of all files using Elektra
- do not work atomically (a journal would be needed)
- do not work together with mmap (as it only checks one file for cache misses)

## Implications

## Related Decisions

## Notes
