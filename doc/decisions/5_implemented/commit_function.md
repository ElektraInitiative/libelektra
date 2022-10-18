# Commit Function

<!-- FIXME [new_backend]: Decide whether the extra commit/error/init functions make sense.
      Possible options are:
      1. keep the separate functions
      2. merge them into set, and let backend plugins dispatch based on elektraPluginGetPhase
      3. in addition to merging into set, add an extra `ElektraGetPhase phase`/`ElektraSetPhase phase` argument to get/set so it is clear that the phase must be taken into account
 -->

## Problem

When `kdbSet()` is called, plugins implementing the commit role need to
track their state to distinguish between carrying out that role and
carrying out potential other roles (commit and setresolver for the
resolver plugin, for example). This limits the possibilities of plugin
reuse and the ways plugins can be combined.

## Constraints

- It should be possible for all existing plugins to run normally

## Assumptions

## Considered Alternatives

## Decision

Committing will no longer be done by `kdbSet()`. Instead, the functionality
will be implemented by its own function, `kdbCommit()`.

## Rationale

The implementation of this function is a prerequisite for a future
improvement to backends, which will enable greater amounts of plugins
in a single backend and plugin statelessness.

Currently, plugins combining the `commit` role with a different role (such as
resolver plugins which combine the `setresolver` and `commit` roles) need to
distinguish between these roles and track their state.

## Implications

## Related Decisions

## Notes

https://issues.libelektra.org/2798
