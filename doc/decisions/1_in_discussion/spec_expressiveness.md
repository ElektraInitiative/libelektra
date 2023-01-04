# Spec's Expressiveness

## Problem

Specifications can help users define certain details about keys and make sure only valid keys are used.

One example of such as specification can be found below:

```ni
[public/ip]
meta:/default = 127.0.0.1
meta:/description = "The IP via which this application is accessible."

[public/port]
meta:/default = 8080
meta:/description = "The port on which this application is accessible."
```

Here one can see `default` and `description` used as metadata. 

The current implementation of the spec plugin is copying this metadata to all other namespaces.
It also assures that if keys do not exist and have `default` metadata, it creates a key in the default namespace.

Assume we have a REST application we want to write a specification for.
This REST application has to use multiple services.

If we have `database`, `broker` and `mailer` services we could use the specification:

```ni
[service/database/port]
meta:/require = true
meta:/default = 8080
meta:/description = "The port of the database service."

[service/database/url]
meta:/require = true
meta:/description = "The url of the database service."

[service/broker/port]
meta:/require = true
meta:/default = 8080
meta:/description = "The port of the broker service."

[service/broker/url]
meta:/require = true
meta:/description = "The url of the broker service."

[service/mailer/port]
meta:/require = true
meta:/default = 8080
meta:/description = "The port of the mailer service."

[service/mailer/url]
meta:/require = true
meta:/description = "The url of the mailer service."
```

This is already very repetitive and error prone. 
But writing the specification becomes completely impossible when the set of services is not known in advance. 
To solve this we need some way to allow for dynamic/unknown parts in the keyname.

`Globbing` expressions is the solution to this problem. 
By providing certain wildcard characters e.g. `_` or `#` one can match multiple keys.
This removes the need to explicitly define specification for keys in our example for every service. 

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

A naive implementation has some undefined behaivor when using `globbing`.

This specification uses `_` as wildcard character.
Unfortunately `_` has a shortcoming, as we do not know the key names in advance.
This prevents us from creating those keys in namespace they do not exist or copying metadata in advance.

But besides `_` we also have `#` as wildcard character.
This wildcard character is used with arrays.
A fixed array size can be defined in the specification, which overcomes the shortcoming of `_`.

The problem we face with those wildcard characters is overlapping specifications.
A example for this is if we used `[service/#/port]` and `[service/shtg/port]` or `[service/_/port]` in the same specification.
We don't know what metadata should be copied to other namespaces in this case.
Defining, both `[service/#/port]` and `[service/_/port]` produces conflicting specification.

For example `[service/#/port]` having as type `meta:/type = string`.
`[service/shtg/port]` having `meta:/type = unsigned_short`.
The spec plugin can not know which of those specification to use.

To address `#` again, it has a shortcoming when compared to `_`.

Lets take a look at this example:

```ni
[service/#/port]
meta:/require = true
meta:/default = 8080
meta:/description = "The port of the service."

[service/#/url]
meta:/require = true
meta:/description = "The url of the service."
```

We completely loose the possibility to have a meaningful name for our key here.
`service/database/port` is more expressive to a user than `service/#0/port`.

We can overcome this problem by having a specification like this:

```ni
[service/#/port]
meta:/require = true
meta:/default = 8080
meta:/description = "The port of the service."

[service/#/url]
meta:/require = true
meta:/description = "The url of the service."

[service/#/name]
meta:/default = ""
meta:/description = "The name of the service."
```

But this adds another key to give us a meaningful name for a given service.

This is why we want to have the wildcard character `_` working correctly.

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
