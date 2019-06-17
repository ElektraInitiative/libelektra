# Error message & handling concept

## Problem

Currently, an error or warning message in elektra causes the following line to be shown:

```
Ingroup: <group>
```

Its main purpose is to show the user if the error occurred in either `kdb`, `module` or `plugin`.

The problem is that this message is of no value for the user and increases the verbosity of the message.

## Constraints

## Assumptions

## Considered Alternatives

## Decision

The `ingroup` message will be removed as it does not yield any notable benefit.

## Rationale

## Implications

The `ingroup` will be removed from all implementations, metakeys and tests.

## Related Decisions

See [Error concept](error_codes.md)

## Notes
