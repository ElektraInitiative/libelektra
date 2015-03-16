# Internal KeySet Cache

## Issue

When doing kdbGet() possible more keys are returned which might be
confusing. When doing a second kdbGet() with a new keyset
no keys might be returned, because it is up-to-date.

When doing kdbSet() a deep duplication is needed.

Idea: keep a duplicated keyset internally. Return (non-deep?)
duplications exactly of the parts as requested.

## Constraints

## Assumptions

## Considered Alternatives

- no cache (current situation)
- flat cache with COW
- deep duplicated cache

## Decision

## Argument

## Implications

## Related decisions

## Notes
