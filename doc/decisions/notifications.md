# Notifications

## Problem

Ensure (contracts) shifted expectations:
Applications want control over if they receive notifications.

## Constraints

- applications need to send notifications in any case, otherwise others cannot receive it

## Assumptions

- multiple transport mechanism at once do not need to be supported

## Considered Alternatives

## Decision

Have a symlink to which transport notification plugin is used.
This notification plugin is used by default to sent notifications.

If applications also want to receive notifications is up to the contract.

## Rationale

## Implications

## Related Decisions

- [Global Plugins](global_plugins.md)
- [Ensure](ensure.md)

## Notes
