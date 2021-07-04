# Iterators

## Problem

The internal iterator inside KeySets seems to cause more problems than it solves.

## Constraints

To ensure backwards compatibility, we could embed one such iterator into KeySets, but recommend that people use an external instance in new code.

## Assumptions

@raphi011 made benchmarks showing that external and internal iterators have about the same speed.
So it should be a safe choice to not provide the internal iterators for 1.0 and only the external instead.

## Considered Alternatives

## Decision

- make `ksNext` `ksRewind` private (not remove it because it would be too much effort to change all the loops)
- better document and advocate the external iterators (docu mostly done, maybe we also should have the examples how to write loops in the release notes)
- start using external iterators in new code
- remove `ksPopAtCursor`
- remove docu about internal cursor from all functions. Changing the internal cursor will be an implementation detail. (Of course we need to somehow keep it consistent to the current state as otherwise we will break our own code.)

## Rationale

- The documentation for the cursor system is not the best.
- The only function that returns a `cursor_t` is `ksGetCursor`. Its documentation is completely broken:
- `ksRewind` and `keyNextMeta` cannot be used on the same variable (`ks`).
- The documentation (not just for the above function, but all over the `ks*` functions) also contains lots of warnings (use only cursor from same keyset, may be invalid, may become invalid, etc.) that make it seem like there is something special about these cursors, when in fact they are simply indicies. There is no information (at least that I could find), how cursors can be modified. Most of the time it even seems like modifying cursors would be a bad idea.

## Implications

## Related Decisions

## Notes
