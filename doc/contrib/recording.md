# Session Recording Technical Documentation

The purpose of this document is to explain in detail the concepts and implementation of Elektra's session recording feature.
While the feature is under development, it acts as sort of in-between documentation, roadmap and TODO list.

## What is Session Recording?

After every `kdbSet` action, the changes are calculated using Elektra's powerful changetracking API.
The result of the calculation is an `ElektraDiff` instance we'll call _part diff_ throughout this document.
The session recording plugin merges those _part diffs_ together and creates and persists an overall _session diff_.
Importantly, the _session diff_ is persisted cross-process.
Conceptually, this is depicted in the following image.

![Session vs Part diff](../images/elektra-record/recording-part-diff.svg)

The following namespaces are monitored for changes:

- dir
- system
- user
- spec

## Storage of the Session Diff

The session diff is persisted in the respective namespace under `/elektra/record/session`.
I.e. all keys in the diff of the `system` namespace are under `system:/elektra/record/session`.

The recording plugin needs its own KDB instance to store the session diff within Elektra.
For concurrency and performance reasons, the storage should be a separately mounted file under each namespace.
This means we provide default mountpoints for the

- `dir:/elektra/record/session`,
- `system:/elektra/record/session`,
- `spec:/elektra/record/session` and
- `user:/elektra/record/session`
  keys.

The following list describes some important keys:

- `/elektra/record`
- `/elektra/record/config`
- `/elektra/record/config/active`
  - If the key is present, session recording is active.
  - The keys value is the parent key
- `/elektra/record/session`
  - Contains all the recorded data.
  - Should be mounted into separate files in each namespace.
- `/elektra/record/session/assert`
  - Keys that should be asserted
- `/elektra/record/session/diff`
- `/elektra/record/session/diff/added`
- `/elektra/record/session/diff/modified/old`
  - Contains the _old_ values and metadata for the keys that have been modified
- `/elektra/record/session/diff/modified/new`
  - Contains the _new_ values and metadata for the keys that have been modified
- `/elektra/record/session/diff/removed`

## Calculating the Session Diff

Keys in a diff are divided in different categories:

- Added: the key is new and did not exist before
- Modified: the key existed before but its value (or metadata) has been modified
- Removed: the key has been deleted

Keys that stayed the same and therefore are not represented in a diff are called _untracked_ keys in the following paragraphs.
The diagram below visualizes the state transitions when merging diffs.
The green ovals depict the state of a key in the session diff.
The arrows depict the actions/state of a key in the new part diff.
For example, if a key is in `Added` state in the session diff, and it is in `Removed` state in the new part diff, then the key will be untracked in the new version of the session diff.
The transitions from `untracked` to `Added`, `Modified` or `Removed` are not depicted, as they are quite trivial.

![Key states in recording](../images/elektra-record/recording-key-states.svg)

This new functionality will be implemented as `elektraDiffMerge`.

## Architecture

The core recording feature has two main components:

1. Recording C API (`libelektra-record`): Implements everything the tooling needs.
2. Recording Plugin (`libelektra-recorder`): Gets loaded as a hook plugin and calls the appropriate functions in `libelektra-record`.
