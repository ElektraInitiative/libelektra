# Iterators

## Problem

The internal iterator inside KeySets seems to cause more problems than it solves.

## Constraints

We could embed one such iterator into KeySets, but recommend that people use an
external instance (int) in new code.

## Assumptions

@raphi011 made benchmarks showing that external and internal iterators have
about the same speed. So it should be a safe choice to not provide the internal
iterators for 1.0 and only the external instead.

## Considered Alternatives

## Decision

- remove all functions related to the internal iterator:
  - ksRewind
  - ksNext
  - ksCurrent
  - ksGetCursor
  - ksSetCursor
  - ksHead
  - ksTail
  - keyRewindMeta
  - keyNextMeta
  - keyCurrentMeta
- change `ksAtCursor` to `ksAt`
- add implementation / documentation / tests for the external iterator
- start using external iterators in new code
- remove documentation about internal cursor from all functions.

## Rationale

- The only function that returns a `cursor_t` is `ksGetCursor`.
  Its documentation is completely broken:
- `ksRewind` and `keyNextMeta` cannot be used on the same variable (`ks`).
- The documentation (not just for the above function, but all over the `ks*`
  functions) also contains lots of warnings (use only cursor from same keyset,
  may be invalid, may become invalid, etc.) that make it seem like there is
  something special about these cursors, when in fact they are simply indicies.
  There is no information (at least that I could find), how cursors can be
  modified. Most of the time it even seems like modifying cursors would be a bad
  idea.

## Implications

## Related Decisions

## Notes
