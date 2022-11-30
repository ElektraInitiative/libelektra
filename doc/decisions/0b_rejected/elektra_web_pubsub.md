# Pub/Sub Communication

## Problem

To develop a [Web UI](https://github.com/ElektraInitiative/libelektra/issues/252), we need to be able to remotely configure Elektra via a network socket.

The idea is to use a Pub/Sub concept to synchronize actions which describe changes in the Elektra state.

## Constraints

- We need to be able to synchronize all changes in Elektra with the Web UI.
- This needs to be done via a network socket due to limitations of the Web.
- That means we need to run an Elektra daemon (`elektrad`) to be able to connect to Elektra at any time.

## Assumptions

## Considered Alternatives

- [ZeroMQ](https://zeromq.org/): small and popular library for pub/sub
- [nanomsg](https://nanomsg.org/): from the same author as ZeroMQ, even smaller - https://nanomsg.org/documentation-zeromq.html
- [redis](https://redis.io/docs/manual/pubsub/): requires a running redis server
- [kafka](https://kafka.apache.org/): seems too big for Elektra
- ZeroMQ with [JSMQ](https://github.com/zeromq/JSMQ).

## Decision

Don't use pubsub at all.

Instead use [REST API](/doc/api_blueprints/elektrad.apib) implemented by [`elektrad`](/src/tools/elektrad).

## Rationale

nanomsg sounds interesting, but isn't as popular as ZeroMQ, which is why there are no browser JS bindings available (only Node.js, which cannot be easily used for the Web UI).

## Implications

## Related Decisions

## Notes
