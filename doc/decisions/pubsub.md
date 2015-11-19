# Pub/Sub Communication

## Issue

To develop a [Web UI](https://github.com/ElektraInitiative/libelektra/issues/252),
we need to be able to remotely configure Elektra via a network socket.

The idea is to use a Pub/Sub concept to synchronize actions which describe
changes in the Elektra state.

## Constraints

- We need to be able to synchronize all changes in Elektra with the Web UI.
- This needs to be done via a network socket due to limitations of the Web.
- That means we need to run an Elektra daemon (`elektrad`) to be able to
 connect to Elektra at any time.

## Assumptions

## Considered Alternatives

- [ZeroMQ](http://zeromq.org/): small and popular library for pub/sub
- [nanomsg](http://nanomsg.org/): from the same author as ZeroMQ, even smaller - http://nanomsg.org/documentation-zeromq.html
- [redis](http://redis.io/topics/pubsub): requires a running redis server
- [kafka](http://kafka.apache.org/): seems too big for Elektra

## Decision

Use ZeroMQ with [JSMQ](https://github.com/zeromq/JSMQ).

## Argument

nanomsg sounds interesting, but isn't as popular as ZeroMQ, which is why there
are no browser JS bindings available (only Node.js, which cannot be easily
used for the Web UI).

## Implications

## Related decisions

## Notes
