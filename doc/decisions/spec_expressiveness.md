# Spec's Expressiveness

## Problem

Currently, you can easily come into wrong assumptions
that something would work in the specification.

We need to find minimal requirements to implement a sane spec plugin.

## Constraints

## Assumptions

## Considered Alternatives

## Decision

- no defaults for `sw/_/key` specifications
  (default will not work for `ksLookup(/sw/sthg/key)`)
- plugins are not allowed to create keys (may change in future; depends on plugin positions)

The spec plugin should yield errors when it detects such situations.

## Rationale

## Implications

## Related Decisions

## Notes
