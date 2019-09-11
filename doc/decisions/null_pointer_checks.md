# Null Pointer Checks

## Problem

Currently all functions do proper argument checking which might degrade
performance.

## Constraints

## Assumptions

## Considered Alternatives

- Removing all null pointer checks and do assert on debug code
- Removing some null pointer checks
- adding functional high-level methods that avoid most null pointer checks

## Decision

Delayed to 1.0.0

## Rationale

- should have consistent, defined behavior

## Implications

- ABI, API

## Related Decisions

## Notes

- Benchmarks needed
