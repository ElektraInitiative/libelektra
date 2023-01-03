# Spec's Expressiveness

## Problem

The current implementation of the spec plugin is copying metadata to all other namespaces.
Metadata e.g. `type` or `description` describes a certain key.

Sometimes there are keys with similar or identical specifications.
We want to define those in a single place and not have to manually copy metadata.
`Globbing` expressions in key names comes into place at that time. 
By providing certain wildcard characters e.g. `_` or `#` one can match multiple keys.
This removes the need to explicitly define specification for keys.

To describe the use case where `globbing` expressions make sense, we will use a REST application.
This REST application uses multiple services which the user is not aware of at time of configuration.
It could use a database, message broker, etc. but this is not yet known.
A solution to this is `globbing` i.e. using wildcards in key names.
The thing to be aware of is to define configuration keys which are necessary for every service.
In this example we use `port` and `url`.

The parent key for our specification is `/sw/org/app-name/#0/development`.

```ni
[service/_/port]
meta:/require = true
meta:/default = 8080
meta:/description = "The port of the service."

[service/_/url]
meta:/require = true
meta:/description = "The url of the service."
```
The `_` would match every `service` we are using in our application e.g. `database`, `message broker`, etc.

### Problem 1

This specification uses `_` but we also have `#` as wildcard in `globbing`. 
The problem we face here is overlapping specifications.
A example for this is if we used `[service/#/port]` and `[service/_/port]` in the same specification.
This is currently undefined behaivour in the spec plugin.

### Problem 2

Errors and warnings are strictly defined in Elektra.

### Problem 3

Default values on wildcard globbing is undefined.

## Constraints

## Assumptions

## Considered Alternatives

## Decision

- no defaults for keys ending with globbing keys `sw/_` (**Problem 3**)
- no defaults for `sw/_/keys` (default will not work for `ksLookup(/sw/sthg/key)`)
- The spec plugin should yield should yield errors to user on overlapping keys (**Problem 1**)
- The spec plugin should yield errors and warnings when it detects such situations (**Problem 2**)

## Rationale

## Implications

## Related Decisions

## Notes
