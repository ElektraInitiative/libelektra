# REST API Documentation

## Problem

A standard way of describing REST APIs offered by tools and plugins for Elektra is required to ease development for and usage of these. Because many good standards for describing APIs are out there already, an existing one shall be used.

## Constraints

- The chosen standard should support Markdown syntax
- API descriptions created within the standard should be human-readable
- Only free software should be used
- To enhance reusability, there should be a separation of data modeling and API description

### Soft-Constraints

- (Nice looking) documentation should be generateable from the plain documentation
- Created documentation should be testable by tools (automatic tests)

## Assumptions

- There is a well-suited standard with enough (free) tools available to satisfy all or most constraints.
- Scenarios created by Elektra REST APIs are simple enough to be allegeable by the chosen standard, also in the future.

## Considered Alternatives

- [Swagger](https://swagger.io/)
- [apiary](https://apiary.io/) with [API blueprints](https://apiblueprint.org/)
- [RAML](https://raml.org/)

## Decision

The decision is to use [API blueprints](https://apiblueprint.org/) together with additional tools from its ecosystem.

## Rationale

**API Blueprints** together with some (free) tools for it support all given constraints and also all soft-constraints. It also fits the current documentation style of the Elektra Initiative the most.

Additionally to modeling data apart from the API, **API Blueprints** also supports schemata modeling, which is more precise than giving examples for requests and responses.

## Implication

- API descriptions should also follow other conventions like the usage of similar error codes.

## Notes

There are many tools available to use with **API Blueprints**, for example the [CLI tool](https://github.com/apiaryio/apiary-client) of apiary. Other tools can be found on the [official website](https://apiblueprint.org/tools.html) of the standard.

Decision discussions have taken place in #917. An API proposal for cluster configurations was made in #912, whereas initial discussion started in #829.
