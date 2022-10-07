# Memory Layout

## Problem

`mmapstorage` leaks internals of the `struct _Key`.

## Constraints

## Assumptions

## Considered Alternatives

- don't make `Key` opaque
- don't provide `mmapstorage`

## Decision

Have versioning and memory layout checks¹ of cache files written by `mmapstorage`.
The cache files get discarded if these checks fail.

## Rationale

See Implications.

## Implications

- will also discard cache files from different architectures
- representation of `struct _Key` can be changed as wanted

## Related Decisions

## Notes

¹ A `KeySet` with known content gets written and mmapstorage checks if this
`KeySet` was restored correctly.
