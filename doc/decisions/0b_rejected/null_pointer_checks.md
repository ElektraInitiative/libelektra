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

Rejected (keep checks) due to time constraints

## Rationale

- should have consistent, defined behavior

## Implications

- ABI, API

## Related Decisions

## Notes

- Benchmarks needed
