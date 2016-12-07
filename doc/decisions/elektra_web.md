# elektra-web structure

## Issue

For elektra-web, there needs to be a way to remotely manage instances and groups
of instances (clusters). The remote configuration of a single instance is
simple: Create a daemon that listens for requests and accesses the KDB.

To manage multiple instances, we need to store the information to access the
daemons, as well as information about the grouping (clusters) of daemons.

## Constraints

- We need to be able to manage single, as well as multiple instances.
- We need to be able to group instances and manage them together.

## Assumptions

## Considered Alternatives

- Accessing the daemons directly
- Using a separate daemon to manage multiple instances
  - Using one of these daemons for each cluster
  - Using one daemon for all clusters

## Decision

Use ZeroMQ with [JSMQ](https://github.com/zeromq/JSMQ).

## Argument

Accessing the daemons directly would require us to store all the information
in the client, which is not a good idea if we want to be able to switch
clients and still be able to access all data. There should be a single point of
entry that the client connects to.

Using a daemon for each cluster would make it harder to create new clusters,
because the user would have to manually set up a new daemon for each cluster.
The problem with this is also that we still don't have a single point of entry,
so we would have to store information to connect to these daemons on the client.

Using one daemon to manage all instances and clusters is the easiest solution,
that way the client simply connects to that daemon, which forwards requests to
single instances or multiple instances at once. In this case, the daemon can
also serve the client, further simplifying the whole structure, because we don't
need to host it on a separate web server.

## Implications

- There needs to be a single point of entry for the web client to connect to.

## Related decisions

## Notes

There are still some other issues, like possible conflicts when adding an
instance multiple times, or assigning an instance to multiple clusters.

The cluster daemon should prevent adding the same instance twice. It also won't
be possible to add an instance to two clusters.

If an instance is part of a cluster, single instances can still be configured,
but all configuration options that are set in the cluster cannot be modified.
