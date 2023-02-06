# Elektra Web structure

## Problem

For Elektra Web, there needs to be a way to remotely manage instances and groups of instances (clusters).
The remote configuration of a single instance is simple.
However, to manage multiple instances, we need to store the information to access the daemons, as well as information about the grouping (clusters) of daemons.

## Constraints

- We need to be able to manage single, as well as multiple instances.
- We need to be able to group instances and manage them together.

## Assumptions

## Considered Alternatives

- Accessing the elektrad daemons directly
- Using a separate daemon (clusterd) to manage multiple instances
  - Using one of these daemons for each cluster
  - Using one daemon for all clusters

## Decision

Use one cluster daemon (clusterd) to manage all clusters and instances.

## Rationale

Accessing the elektrad daemons directly would require us to store all the information in the client, which is not a good idea if we want to be able to switch machines and still be able to access all data.
Thus, there should be a single point of entry that the client connects to.

Using a daemon for each cluster would make it harder to create new clusters, because the user would have to manually set up a new daemon for each cluster.
Another problem with this approach is that we still don't have a single point of entry, so we would have to store information to connect to the daemons on the client.

Using one daemon to manage all instances and clusters is the easiest solution, that way the client simply connects to that daemon, which forwards requests to single instances or multiple instances at once.
In this case, the cluster daemon can also serve the client, further simplifying the whole structure, because we don't need to host the client on a separate web server.

Another advantage of this solution is that both clusterd and elektrad are using nodejs, which means that a wrapper script can set up and start both daemons easily.

There are still some other issues, like possible conflicts when adding an instance multiple times, or assigning an instance to multiple clusters.
The cluster daemon should prevent adding the same instance twice.
It also won't be possible to add an instance to two clusters.
If an instance is part of a cluster, single instances can still be configured, but all configuration options that are set in the cluster cannot be modified.

## Implications

- There needs to be a single point of entry for the web client to connect to.

## Related Decisions

- [Elektra Web Recursive Structure decision](elektra_web_recursive.md)

## Notes

- [Discussion in the pull request](https://github.com/ElektraInitiative/libelektra/pull/1173)
