# Remove elektraMalloc() et al.

## Problem

A problem with having `elektraMalloc` et al.¹ is, that:

- it makes it impossible to call libc functions that allocate data with malloc (e.g. strndup) or
- others that expect a pointer that can be passed to realloc.

You might even do that by accident and never notice the problem, until someone replaces elektraMalloc and then you could get some pretty bad memory bugs.

¹ Affected functions:

- `elektraMalloc`
- `elektraCalloc`
- `elektraFree`
- `elektraStrDup`
- `elektraStrLen`

## Constraints

Proprietary apps are sometimes delivered together with a libc (either static or a startup script makes sure their own libs are found).
If they would use Elektra, and the person wants the app to use the global Elektra it is a valid use case for the dedicated functions.

## Assumptions

The only safe way to keep elektraMalloc et al. is to define that they will always call their libc counterpart (malloc et al.) and their purpose is simply to add assertions.
Then we can also make them private, because you can just call free to free any pointer returned by Elektra.

## Considered Alternatives

- Completely replace `elektraMalloc` / `elektraCalloc` with simple calls to malloc
- Fix all places where non-Elektra functions get used

## Decision

- keep current state with the custom functions (allocators)
- make it optional for plugin developers to use language specific allocators or our custom allocators
- remove everything except `elektraFree` from public API
- remove all functions that don't actually involve any memory allocations (e.g. `elektraStrLen`, `elektraStrCmp`)
- add `elektraStrNDup` and other `stdlib` equivalents that do memory allocation.

## Rationale

While the current state might cause some problems with compiler optimizations and discoverability, those issues probably are only very minor in practice.
Removing all functions poses a considerable amount of work, which would have to be undone in case we need those functions one day anyway.
The current downsides don't justify such a big procedure.

## Implications

## Related Decisions

## Notes

@kodebach wrote: Ideally, I'd remove the functions, but it seems unlikely this will be accepted.
