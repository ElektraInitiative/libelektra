# Elektra Web recursive structure

## Problem

After deciding how to remotely manage instances and groups of instances (clusters) with Elektra Web, there is still the issue of recursively nested clusters (clusters of clusters).

## Constraints

- We need to be able to manage single, as well as multiple instances.
- We need to be able to group instances (into clusters) and manage them together.
- We need to be able to group clusters and manage them together.

## Assumptions

- Use one cluster daemon (clusterd) to manage all clusters and instances.

## Considered Alternatives

- Allowing clusterd instances to add clusterd instances (instead of just elektrad instances)
- Managing the hierarchy in a single clusterd instance

## Decision

Managing the hierarchy in a single clusterd instance.

## Rationale

Accessing clusterd instances from other clusterd instances would mean that there also needs to be some authentication between those, complicating the set-up process.
Furthermore, it would not be as easy to deal with conflicts, as the client might connect to a clusterd instance that belongs to another clusterd, in which case it would not be aware of the constraints of the parent clusterd instance.

There is also still the issue that using a daemon for each cluster would make it harder to create new clusters, because the user would have to manually set up a new daemon for each cluster.

Using one daemon to manage all instances and clusters, including sub-clusters is the easiest solution, that way the client simply connects to that daemon, which forwards requests to single instances or multiple instances at once.
Furthermore, the cluster daemon could deal with conflicts easily as it is aware of the whole network.
In this case, the cluster daemon can also serve the client, further simplifying the whole structure, because we don't need to host the client on a separate web server.

## Implications

- There is a single point of entry for the web client to connect to.

## Related Decisions

- [Elektra Web Structure decision](elektra_web.md)

## Notes
