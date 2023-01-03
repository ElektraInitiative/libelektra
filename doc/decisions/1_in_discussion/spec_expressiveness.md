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

But the current implementation has some undefined behaivour when using `globbing`.

This specification uses `_` but we also have `#` as wildcard in `globbing`. 
The problem we face here is overlapping specifications.
A example for this is if we used `[service/#/port]` and `[service/shtg/port]` or `[service/_/port]` in the same specification.
We don't know what metadata should be copied to other namespaces in this case.
Both `[service/#/port]` and `[service/_/port]` existing produces conflicting specification.

For example `[service/#/port]` having as type `meta:/type = string`.
`[service/shtg/port]` having `meta:/type = unsigned_short`.
The spec plugin can not know which of those specification to use.

Besides this problem the `spec` plugin also creates default values.
A plugin responsible for copying metadata to other namespaces should not create new keys.
In this case it creates cascading keys which are then found by `ksLookup`.
In my opinion a separate plugin for handling `default values` should exist.
The validation part of the `spec` plugin can be kept as it is now (`required keys` and `array size validation`).

## Constraints

## Assumptions

## Considered Alternatives

## Decision 

## Rationale

## Implications

## Related Decisions

## Notes
