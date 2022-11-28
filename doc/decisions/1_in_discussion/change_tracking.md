# Change Tracking

## Problem

Currently, a range of Elektra plugins are implementing some sort of change tracking for configuration data.
This includes, but is not limited to, the [internalnotification](/src/plugins/internalnotification/README.md) and [dbus](/src/plugins/dbus/README.md) plugins.
In the near future, Elektra shall also be extended with session recording.

KDB itself also has some rudimentary change tracking (via the `keyNeedsSync` flag) to determine whether `kdbSet` needs to actually do something.

These competing change tracking implementations create multiple problems:

1. pointless waste of resources, as data is duplicated in each plugin,
2. multiplication of code, generating a maintenance burden.
3. various subtle differences in change tracking behavior, e.g., `kdbSet` might write a config file but notification is not sent out
4. the current approach to change tracking in plugins is fragile, which is outlined in [a separate decision about valid kdbGet/kdbSet sequences](operation_sequences.md).

For `KeySet` we need to track which of the keys:

- have been removed
- have been added

For `Key` we need to track:

- original value of the key
- size of the original value (for binary keys)
- metadata, which is a combination of the tracking of keysets and keys
- tracking should only be done on the following namespaces:
  - `system:/`
  - `user:/`
  - `dir:/`
  - `meta:/`
  - `spec:/`

## Constraints

Change tracking must:

- be transparent for applications using the public Elektra API
- be transparent to users changing configuration data
- if overhead is not negligible: only do tracking as required, i.e., a plugin specifically requests it
- have negligible overhead if disabled
- not duplicate data for each plugin that wants change tracking
- work with all allowed sequences of `kdbGet` and `kdbSet` as [per this decision](operation_sequences.md)

We only want to track changes that are done by the user, not changes that are done by plugins.
I.e. the scope of change tracking is on what happens _outside_ of `kdbGet` and `kdbSet`.

The library `libelektra-core` must be kept minimal.

## Assumptions

- It is possible to do change tracking with reasonable memory and computation overhead
- It is possible to design a single change tracking API that is useful for all existing and future plugins
- False positivies are okay

  - this may happend when some keys may have been changed by the user, but have subsequentially been "unchanged" by a transformation plugin
    Scenario: plugin that converts `false`<->`0` and `true`<->`1`
    - `system:/background` is stored with value `false`
    - user gets key `system:/background` with value `0` (after conversion by plugin)
    - user changes it to `false`
    - changetracking detects that value has been changed, because `false` != `0`
    - plugin changes value from `false` to `0`
    - consumers of the changetracking API will now get a false positive if they query whether `system:/background` has been changed

- False negatives are not okay

## Considered Alternatives

### Alternative 1 - Tracking within `libelektra-kdb`

Do the tracking within `libelektra-kdb`, within the `kdbGet` and `kdbSet` operations.

Essentially, we'd have an internal cache for all keys that were used returned by a `kdbGet` operation in the lifetime of the `kdb` instance.
We also need to update it on `kdbSet` so that a future `kdbSet` operation without a `kdbGet` will also work.

This approach does not limit which sequences of `kdbGet` and `kdbSet` calls are valid.

With Elektra's global copy-on-write approach, the memory overhead of this approach shouldn't be too concerning.

### Alternative 2 - Tracking with meta keys

Do the tracking within `libelektra-kdb`, but with meta keys.

Essentially the same approach as above, but instead of deep-duping, we add the original value
as a metakey to every key. Not yet clear how we handle changes to metadata then.

### Alternative 3 - Change tracking within a separate plugin

Outsource the change tracking into a separate plugin.

Essentially the same as (1), just that it is implemented within a plugin and not `libelektra-kdb`.
This will be a hook plugin, and will be called within `kdbGet` and `kdbSet` accordingly.
It will also need to export a hook-method to get the changeset.

The following hooks will be needed:

- `tracking/get`: wil be called at the end of `kdbGet`, directly before the result is returned.
- `tracking/set/preliminary`: will be called at the beginning of `kdbSet` and after every step/phase in `kdbSet`.
  We need this to have change tracking information as soon as possible, so that plugins in any step of the process can use this information.  
   Also, plugins in every step of the process might change data, so it is important to call it after every step.
- `tracking/set/final`: will be called in the `post-commit` phase, after all changes have been written to disk, but before the `notification/send` hook.
  Represents the final, real changes to the KDB.
- `tracking/changeset`: compute the changeset for the requested parent key and return it.

### Alternative 4 - Copy-on-write change tracking within `libelektra-core`

The idea here is that we extend the `KeySet` and `Key` structs with additional fields.

We need to extend `KeySet` with the following info:

- What keys have been removed
- What keys have been added
- Whether tracking is enabled for this KeySet

We need to extend `Key` with the following info:

- Original value of the key
- Size of the original value (for binary keys)
- Whether tracking is enabled for this key

The tracking itself would be done within the `ks*` and `key*` methods, after checking if it is enabled.
It would also transparently work for metadata, as metadata itself is implemented as a keyset with keys.

Downsides of this approach:

- It adds functionality to `libelektra-core` which may violate the constraint above.
  It may, however, be debatable whatever 'minimal' means in this context.
- Adding fields to the structs causes a slight memory overhead, even with tracking turned off.
  While negligible for `KeySet` due to the low amount of keysets in typical applications, it may be noticable for `Key`.
  On a 64-bit system we'd add 16 bytes to it.
  8 bytes for the pointer to the original value, 8 bytes for the size of the original value.
  To put this in perspective, the current size of the `Key` struct is 64 bytes, so we'd add 25% overhead to an empty key.
  However, this percentage will be much lower in a real-world application, as the usefulness of an empty key is very low.
- Another downside here is that it is not so easy to determine what the "original" value is.
  Some part of `libelektra-kdb` would need to mark the keys as original (after transformations etc.)

### Alternative 5 - Use `backendData->keys` for change tracking

Use the `backendData->keys` for change tracking

We already store which keys have been returned by `kdbGet` for each backend within KDB.
Currently, however, this is not a deep copy, as we are returning the internally stored `Key` instances directly.
This means we can not detect changes to the values or metadata of keys right now.
We can, however, rely on this for detecting removed and added keys in its current form.

As we now implement copy-on-write for all keys and keysets within Elektra, we can deep copy what we return in `kdbGet`.

Another problem with this approach is that the internally stored keys are recreated as new instances every time `kdbGet` is called.

## Decision

## Rationale

## Implications

-
-
-

## Related Decisions

- [Valid kdbGet/kdbSet sequences](operation_sequences.md) from [#4574](https://github.com/ElektraInitiative/libelektra/pull/4574).
- [Copy On Write](../2_in_progress/copy_on_write.md)

## Notes

- Issue [#4514](https://issues.libelektra.org/4514) uncovered a problem in the current change tracking approach
- Issue [#4520](https://issues.libelektra.org/4520) already explored some of the considered alternatives
