# Session Recording Technical Documentation

A recording session is a period of time during which changes to the Key Database (KDB) are tracked and accumulated.
It begins when recording starts and ends when recording stops.
Throughout the recording session, all changes made to the KDB are recorded, including additions, modifications, and deletions of keys and their associated values.
The recording session provides a complete audit trail of all changes made to the KDB during the specified period of time.

After every `kdbSet`, changes are calculated using Elektra's powerful changetracking API.
The result of the calculation is an `ElektraDiff` instance we'll call _part diff_ throughout this document.
The session recording plugin merges those _part diffs_ together and creates and persists an overall _session diff_.

Conceptually, this is depicted in the following image:

![Session vs Part diff](../images/elektra-record/recording-part-diff.svg)

Importantly, the _session diff_ is shared across processes.
The _session diff_ persistently records all changes made to the whole KDB from any process between the start and end of the session.
Because of the cross-process nature of a _session diff_, it is also susceptible to "simultaneous write" conflicts.

We prevent inconsistent data by using locks.
As long as recording is enabled, there is a global lock on write operations in Elektra.
If a conflict occurs, it looks to the applications the same as if there was a conflict writing configuration data.
The high-level Elektra bindings already resolve such conflicts transparently.

All persistable namespaces are monitored for changes.

An important concept for modified keys is the distinction between _old_ and _new_ keys.
Old keys refer to the keys how they were (values, metadata) _before_ the modifications.
New keys, on the other hand, refer to how the keys are _after_ the modifications.
This concept does not apply to _added_ or _removed_ keys.
You can think of added keys of only having new keys, and removed keys only having old keys.

We currently have no way of recording changes done outside of Elektra, i.e. when the configuration files got edited manually.

## Storage of the Session Diff

The session diff is persisted in the respective namespace under `<namespace>:/elektra/record/session`.
I.e. all keys in the diff of the `system` namespace are under `system:/elektra/record/session`.

The recording plugin needs its own KDB instance to store the session diff within Elektra.
We provide hard coded default mountpoints for the

- `dir:/elektra/record/session`,
- `system:/elektra/record/session`,
- `spec:/elektra/record/session` and
- `user:/elektra/record/session`
  keys.

These mountpoints store the keys in the same storage format as the default mountpoints like `system:/` and `user:/`.
The session storage file called `record-session.cfg` is located in the respective standard directories for their namespace.

The following list describes some important keys:

- `/elektra/record/config/active`
  - If the key is present, session recording is active.
  - The keys value is the parent key of the session.
  - We will only record changes made to keys same or below of this parent key.
- `/elektra/record/session`
  - Contains all the recorded data.
  - Should be mounted into separate files in each namespace.
- `/elektra/record/session/diff/added`
  - Contains all added keys.
- `/elektra/record/session/diff/modified/old`
  - Contains the _old_ values and metadata for the keys that have been modified.
- `/elektra/record/session/diff/modified/new`
  - Contains the _new_ values and metadata for the keys that have been modified.
- `/elektra/record/session/diff/removed`
  - Contains all removed keys.

## Calculating the Session Diff

Keys in a diff are divided into different categories:

- Added: the key is new and did not exist before
- Modified: the key existed before and still exists but its value or metadata has been modified
- Removed: the key has been removed

Keys that stayed the same and therefore are not represented in a diff are called _unchanged_ keys in the following paragraphs.
The diagram below visualizes the state transitions when merging diffs.

The green ovals depict the state of a key in the session diff.
The arrows depict the actions/state of a key in the new part diff.

For example, if a key is in `Added` state in the session diff, and it is in `Removed` state in the new part diff, then the key will be unchanged in the new version of the session diff.
The transitions from `unchanged` to `Added`, `Modified` or `Removed` are not depicted, as they are quite trivial.

![Key states in recording](../images/elektra-record/recording-key-states.svg)

This functionality is quite generic and thus implemented as part of the normal diff API as `elektraDiffAppend`.

## Architecture

The core recording feature has two main components:

1. Recording C API (`libelektra-record`): Implements everything the tooling needs.
2. Recording Plugin (`libelektra-recorder`): Gets loaded as a hook plugin and calls the appropriate functions in `libelektra-record`.
