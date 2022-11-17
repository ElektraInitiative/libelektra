# Readonly Keynames

## Problem

It might be a good idea to prevent changes to the keyname after creation.
Currently, key names are automatically read-only when they are contained in a keyset.

There may also be situations where changing the name of a key after its creation is required.
In some situations, it may be wise to reuse a key instead of deleting it and creating a new one.

## Constraints

1. `keyAddName` et al. still have to work up to a certain point
2.
3.

## Assumptions

1.
2.
3.

## Considered Alternatives

### Alternative A

### Alternative B

### Alternative C

## Decision

## Rationale

## Implications

-
-
-

## Related Decisions

- []()
- []()
- []()

## Notes

- [Issue 2202](https://issues.libelektra.org/2202) talks about how unexpected it is that keys will be readonly once in a keyset, but they don't get unlocked when removing them from a keyset.
