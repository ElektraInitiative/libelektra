# Record Storage

## Problem

We need a way to store session recordings.
Sometimes it is required to run the `kdb` application with `sudo`, especially when changing system-wide configuration.
Session recordings should be on a per-user basis. Storing them in the `user:/` namespace is not compatible with `sudo`.

## Constraints

We need to support cases where kdb is run as `sudo`

## Assumptions

## Considered Alternatives

## Decision

**Solution 1:** Store session in `system:/` namespace and mount each session as a file in the `/tmp/` directory.

Pros:
 - Storage layer already implemented, reuse of existing functionality
 - Can query recordings with existing tooling

Cons: 
 - Still needs workaround to start session (aka mount a session in `system:/`)
 - Semantic mismatch: Elektra is built for configuration data; session recordings are not configuration data
 - Need to model session recordings to fit into Elektra data model (KeySet)


**Solution 2:** Do not store session recordings in Elektra. Use an appropriate external serialization format instead.

Pros:
 - We can use a data format that is optimized for our use-case
 - No need to find workaround for mounting session in system-wide namespace
 - When using an "append-only" format there is no need to create sequence numbers

Cons:
 - Need to implement reliable saving ourself
 - Most likely requires additional external dependencies
 - Not possible to query recordings using Elektra tooling

## Rationale

## Implications

## Related Decisions

## Notes

https://github.com/ElektraInitiative/libelektra/pull/4371
