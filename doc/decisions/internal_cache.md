# Internal KeySet Cache

## Problem

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
- Use global plugin to implement internal cache.
- Use global mmap plugin and make kdbGet() to always retrieve everything.

## Decision

Not yet decided.

## Rationale

Semantics can be provided without additional code or overhead in the core.

## Implications

## Related Decisions

- [Global Validation](global_validation.md)

## Notes

internal caches lead to duplication of memory consumption
(most of it is avoided by reference counting, though)

in some cases caches cannot be avoided?
-> filesys, databases?

### Cache Discussion

**Pros:**

- not more keys than needed
- kdbGet avoids IO even if done somewhere else
- KDB handles could be more locally

**Cons:**

- not possible to access cache with current architecture, KDB high level API
- implementation overhead
- where should the caches be
