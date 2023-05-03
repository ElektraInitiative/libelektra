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
4. the current approach to change tracking in plugins is fragile, which is outlined in [a separate decision about valid kdbGet/kdbSet sequences](../0_drafts/operation_sequences.md).

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
- work with all allowed sequences of `kdbGet` and `kdbSet` as [per this decision](../0_drafts/operation_sequences.md)

We only want to track changes that are done by the user, not changes that are done by plugins.
I.e. the scope of change tracking is on what happens _outside_ of `kdbGet` and `kdbSet`.

The library `libelektra-core` must be kept minimal.

## Assumptions

- It is possible to do change tracking with reasonable memory and computation overhead
- It is possible to design a single change tracking API that is useful for all existing and future plugins
- False positives are okay

  - this may happen when some keys have been changed by the user, but have subsequentially been "unchanged" by a transformation plugin
    Scenario: plugin that converts `false`<->`0` and `true`<->`1`
    - `system:/background` is stored with value `false`
    - user gets key `system:/background` with value `0` (after conversion by plugin)
    - user changes it to `false`
    - changetracking detects that value has been changed, because `false` != `0`
    - plugin changes value from `false` to `0`
    - consumers of the changetracking API will now get a false positive if they query whether `system:/background` has been changed
    - We assume consumers of the changetracking API have access to both the previous and the new value of a changed key.
      Therefore, they could detect these false positive cases, if they need to.

- False negatives (missed changes) are not okay

## Solutions - Storage

### Utilize already existing `backendData->keys`

We already store which keys have been returned by `kdbGet` for each backend within KDB.
Currently, those are shallow copies.
To be useful for changetracking, we need to perform deep copies instead.
As keys and keysets are already utilizing copy-on-write, this would not add too much memory overhead.

A problem with this approach is that the internally stored keys are recreated as new instances every time `kdbGet` is called.

### Combine with internal cache

We already decided that we want to have an internal deep-duped keyset of all the keys we returned.
See [internal cache decision](../4_decided/internal_cache.md).

The difference to `backendData->keys` is that this cache is not recreated each time `kdbGet` is called.

### Have a seperate storage for changetracking

Have a global keyset with deep-duped keys that is purely used for changetracking and nothing else.

### Store changes as meta keys

When something changes for the first time, store the original value as a metakey to every key.
Not yet clear how we handle changes to metadata then.
Also not really possible for keysets.

### Implement changetracking as a mechanismn on the `Key` and `KeySet` datastructures

The idea here is that we extend the `KeySet` and `Key` structs with additional fields.

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

## Solutions - Implementation

### Implement directly within `libelektra-kdb`

Implement all the logic for changetracking directly within `libelektra-kdb`.

### Implement as a seperate plugin

Implement changetracking as a hooks plugin that will be called within `kdbGet` and `kdbSet` accordingly.

The following hooks will be needed:

- `tracking/get`: will be called at the end of `kdbGet`, directly before the result is returned.
- `tracking/set`: will be called at the beginning of `kdbSet`.
- `tracking/changeset`: compute the changeset for the requested parent key and return it.

As every hook plugin can define its own contract, in theory all storage forms mentioned in the previous chapter should be possible to implement.
We could just point the plugin to `backendsData->keys` or the internal cache if we go down that route.

## Solutions - Query

### Provide an API within `libelektra-kdb`

The API should be usable by plugins and by applications utilizing Elektra.
It does not matter whether the changetracking is implemented as part of `libelektra-kdb` or as a separate plugin.
The API may look something like this:

```c
bool elektraChangeTrackingIsEnabled (KDB * kdb);
ChangeTrackingContext * elektraChangeTrackingGetContext (KDB * kdb, Key * parentKey);

KeySet * elektraChangeTrackingGetAddedKeys (ChangeTrackingContext * context);
KeySet * elektraChangeTrackingGetRemovedKeys (ChangeTrackingContext * context);
KeySet * elektraChangeTrackingGetModifiedKeys (ChangeTrackingContext * context); // Returns old keys (pre-modification)

bool elektraChangeTrackingValueChanged (ChangeTrackingContext * context, Key * key);
bool elektraChangeTrackingMetaChanged (ChangeTrackingContext * context, Key * key);

KeySet * elektraChangeTrackingGetAddedMetaKeys (ChangeTrackingContext * context, Key * key);
KeySet * elektraChangeTrackingGetRemovedMetaKeys (ChangeTrackingContext * context, Key * key);
KeySet * elektraChangeTrackingGetModifiedMetaKeys (ChangeTrackingContext * context, Key * key); // Returns old meta keys (pre-modification)
```

### Provide query methods as part of a separate plugin

This solution only makes sense if changetrackig is implemented as part of a seperate plugin.
It will be a bit challenging to use for applications, as it would require that applications have access to the plugin contracts.

The changetracking plugin needs to export at least functions for the following things in its contract:

- Get added keys
- Get removed keys
- Get modified keys
- Get added meta keys for a key
- Get removed meta keys for a key
- Get modified meta keys for a key

## Decision

Plugin and application developers can declare that changetracking is required via a contract.
Store deep-duped copy-on-write returned keys in a separate keyset, which we might also use as [internal cache](../4_decided/internal_cache.md).
The whole changetracking logic lives within `libelektra-kdb`.
We provide an API for developers with `libelektra-kdb`.

## Rationale

This decision meets all the constraints.

We are not altering the behavior of already existing functions, so everything is transparent for applications using the public Elektra API.
As changetracking is an opt-in feature, it will create zero runtime overhead and negligible memory overhead in usecases where it is not needed.
There is only a single implementation and storage, so no duplication for each plugin that wants change tracking.
As we are storing our own tracking data, all sequences of `kdbGet` and `kdbSet` should work.

## Implications

- Documentation for changetracking APIs needs to be written
- `struct _KDB` needs to be extended to contain the keyset for changetracking
- Replace all custom changetracking code within Elektra with this unified implementation.
  This should also include the `keyNeedsSync` flag.
  It also includes switching all notification plugins to use the new API.

## Related Decisions

- [Valid kdbGet/kdbSet sequences](../0_drafts/operation_sequences.md) from [#4574](https://github.com/ElektraInitiative/libelektra/pull/4574).
- [Copy On Write](../6_implemented/copy_on_write.md)

## Notes

- Issue [#4514](https://issues.libelektra.org/4514) uncovered a problem in the current change tracking approach
- Issue [#4520](https://issues.libelektra.org/4520) already explored some of the considered alternatives
