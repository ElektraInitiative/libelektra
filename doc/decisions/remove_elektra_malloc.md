# Remove elektraMalloc() et al.

## Problem

A big problem with having elektraMalloc is, that it makes it impossible to call
libc functions that allocate data with malloc (e.g. strndup) or others that
expect a pointer that can be passed to realloc. You might even do that by
accident and never notice the problem, until someone replaces elektraMalloc
and then you could get some pretty bad memory bugs.

Functions considered for removal:

- `elektraMalloc`
- `elektraCalloc`
- `elektraFree`
- `elektraStrDup`
- `elektraStrLen`

## Constraints

Proprietary apps are sometimes delivered together with a libc (either static or
a startup script makes sure their own libs are found). If they would use
Elektra, and the person wants the app to use the global Elektra it is a valid
use case for the dedicated functions.

## Assumptions

The only safe way to keep elektraMalloc et al. is to define that they will
always call their libc counterpart (malloc et al.) and their purpose is simply
to add assertions. Then we can also make them private, because you can just
call free to free any pointer returned by Elektra. Ideally, I'd remove the
functions, but it seems unlikely this will be accepted.

## Considered Alternatives

- Completely replace `elektraMalloc` / `elektraCalloc` with simple calls to malloc

## Decision

- keep current state with the custom functions
- make it optional for plugin developers to use c builtins or the custom Elektra
  functions
- Fix places where non-Elektra functions get used
- Remove `elektraMalloc` / `elektraCalloc` / `eletraRealloc` from public API

## Rationale

While the current state might cause some problems with Compiler optimizations
and discoverability, those issues probably are only very minor in practice.
Removing all functions poses a considerable amount of work, which would have to
be undone in case we need those functions one day anyway. The current downsides
don't justify such a big procedure.

## Implications

## Related Decisions

## Notes
