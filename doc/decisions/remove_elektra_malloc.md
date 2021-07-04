# Remove elektraMalloc() et al.

## Problem

A big problem with keeping elektraMalloc is that it is makes it impossible to call some libc functions that allocate data with malloc (e.g. strndup) or others that expect a pointer that can be passed to realloc. You might even do that by accident and never notice the problem, until someone replaces elektraMalloc and then you could get some pretty bad memory bugs.

Functions considered for removal:

- `elektraMalloc`
- `elektraCalloc`
- `elektraFree`
- `elektraStrDup`
- `elektraStrLen`

## Constraints

Proprietary apps are sometimes delivered together with a libc (either static or a startup script makes sure their own libs are found). If they would use Elektra, and the person wants the app to use the global Elektra it is a valid use case for the dedicated functions.

## Assumptions

According to Klemens' opinion, the only safe way to keep elektraMalloc et al. is to define that they will always call their libc counterpart (malloc et al.) and their purpose is simply to add assertions. Then we can also make them private, because you can just call free to free any pointer returned by Elektra. Ideally, I'd remove the functions, but it seems unlikely this will be accepted.

## Considered Alternatives

- Completely replace elektraMalloc / elektraCalloc with simple calls to malloc
- Leave as it is

## Decision

## Rationale

My main goal with removing elektraMalloc et al. is to improve developer friendliness, especially for newcomers. It is still annoying that I using e.g. strndup may be problematic, because it doesn't use elektraMalloc for allocations. But when I was new to Elektra (and I suspect many other felt the same), it was downright frustrating that you have to remember to use Elektra's special sauce instead of the standard stuff.

The functions `elektraRealloc` and `elektraMemDup` (renamed from elektraStrNDup) can stay, because they are an improvement upon the libc.

## Implications

## Related Decisions

## Notes
