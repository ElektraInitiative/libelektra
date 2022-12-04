# Spec's Expressiveness

## Problem

The current implementation of the spec plugin is copying metadata from the specification
(`spec` namespace) to all other namespaces. By providing certain metadata e.g. 
`type` or `description`, it can be assured that keys adhere to a given specification. 

For some applications there are keys we don't want to define for every application explicitly.
This is when `globbing` expressions in key names comes into place. By providing certain wildcard
characters e.g. `_` or `#` one can match and specify a number of keys at once without the need
to explicitly state the key in every specification.

To describe the use case where `globbing` expressions make sense, we will use an application
based on microservice architecture as sample.

Imagine we have microservices and each of them communicates to a database to persist data. We
will need some configuration so the applications can connect to the database e.g. `port`. Lets
say we are specifying our configuration by `database/<DIALECT>/...`, where `...` is any name
for a key. If we have multiple dialects e.g. a microservice uses more than one database e.g. `mysql`, `mssql`, etc. it is inconvenient to copy and paste the configuration for every 
dialect we use.

By using `globbing` expression we can overcome this problem.

The key starts with `/sw/org/app-name`.
```ni
[database/_/port]
meta:/require = true
meta:/description = "The database port."
```
The `_` would match every `dialect` we are using in our application e.g. `postgresql`, `mssql`, etc.

## Constraints

TODO

## Assumptions

## Considered Alternatives

## Decision

- no defaults for `sw/_/key` specifications (default will not work for `ksLookup(/sw/sthg/key)`)
- plugins are not allowed to create keys (may change in future; depends on plugin positions)

The spec plugin should yield errors when it detects such situations.

## Rationale

## Implications

## Related Decisions

## Notes
