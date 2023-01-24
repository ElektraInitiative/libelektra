# Continuous Integration Restructure

## Problem

The continuous integration pipeline takes a very long time to report the outcome and does not always produce a consistent result.
This may result into development times taking longer than necessary when the feedback of the CI pipeline is mandatory.

Some stages such as the spell-checker run quite late which is not required.
This can lead into the circumstance, that a commit will be fully built by the CI only to fail at the end because of some misspellings.

Sometimes, changes occur completely independent of the libelektra repository and let the CI fail without any changes.
A recent example for this is the modification of the macOS base image which introduced an incompatible version of python.

Another regularly occurring error is a failure during fetching the docker images which lets also fail the CI.
Although the result is the same, this can have multiple causes such as the `no space left on device` message or a network problem during pulling the images.

## Constraints

- Build context cannot be shared across different distributions safely, although it would drastically reduce build times.
- Changes outside the libelektra cannot be controlled by the Elektra Initiative such as version changes or take-downs of docker images.

## Assumptions

## Solutions

As this topic is quite complex, the considered options are not mutually exclusive, but are intended to be combined instead.
For this reason, some of them are already mentioned within this draft.

### Reducing The Stages

Not every build-stage is required in order to ensure the quality of a commit itself.
For example, the packaging process does not say anything about the code quality itself.

### Reducing Upstream Dependencies

Images with frequent incompatible changes could be removed from the CI or at least not fail the complete CI.

### Introducing a Clean-up Strategy

Introducing a clean-up strategy would lead to much less space wasted on the server disks.
With that in mind, the `no space left on device` error would occur much less frequently.
There exists a [Pull-Request](https://github.com/ElektraInitiative/libelektra/issues/4779) which also mentioned this.

### Introducing a Spelling Threshold

Introducing a spelling threshold can be useful to not let the CI always fail on a single spelling error.
The idea behind this is to fix a certain threshold which let the CI fail if too much spelling errors occur in the repositry.
A badge within the [readme](../../../README.md) can be used to display the current state of that.

## Decision

## Rationale

## Implications

## Notes
