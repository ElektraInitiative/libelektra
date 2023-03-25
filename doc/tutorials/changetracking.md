# Changetracking

Elektra provides developers of applications and plugins a way to determine changes made to a `KeySet` relative to the last known state of the key database.
This can be useful if you are writing a plugin that works with changes of the configuration.

## Basics

The two headers you have to use are `kdbchangetracking.h` and `kdbdiff.h`.
Those declare the `elektraChangeTracking*` and `elektraDiff*` functions.

The two main data structures you will encounter are `ChangeTrackingContext` and `ElektraDiff`.

## Getting the difference between KeySets

If all you want to do is get the difference between two in-memory `KeySet` objects, use the function `elektraDiffCalculate`.

```c
KeySet * originalKeySet;
KeySet * modifiedKeySet;
Key * parentKey;

// Calculate the difference of the keys below and including parentKey
ElektraDiff * diff = elektraDiffCalculate (modifiedKeySet, originalKeySet, parentKey);

// Extract useful information, see section 'Working with the diff'

elektraDiffDel (diff);
```

## Getting the changes within a transaction in a plugin

If you are writing a plugin, you can use `elektraChangeTrackingGetContextFromPlugin` to get the current `ChangeTrackingContext`.
Then, use `elektraChangeTrackingCalculateDiff` to calculate the changes to the KDB.

```c
int myPluginSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	const ChangeTrackingContext * ctx = elektraChangeTrackingGetContextFromPlugin (handle);
	ElektraDiff * diff = elektraChangeTrackingCalculateDiff (returned, ctx, parentKey);

	// Don't be impatient, we will soon look into

	elektraDiffDel (diff);
	return 1;
}
```

## Getting the changes as an application developer

If you are elektrifying your app, you can use `elektraChangeTrackingGetContextFromKdb` to get the current `ChangeTrackingContext` for your KDB instance.
Then, use `elektraChangeTrackingCalculateDiff` to calculate the changes to the KDB.

```c
KDB * kdb;
KeySet * myKeySet;

const ChangeTrackingContext * ctx = elektraChangeTrackingGetContextFromKdb (kdb);
ElektraDiff * diff = elektraChangeTrackingCalculateDiff (myKeySet, ctx, parentKey);

// Don't be impatient, we will soon look into

elektraDiffDel (diff);
```

## Working with the diff

Congratulations! You've got your diff!
Now what?
Elektra provides you with different functions to determine added, removed and modified keys, as well as added, removed and modified metadata.

You can use the following methods to reason about changes to the keyset as a whole:

```c
// Get keys that are in modifiedKeySet but not in originalKeySet
KeySet * addedKeys = elektraDiffGetAddedKeys (diff);

// Get keys that are in originalKeySet but not in modifiedKeySet
KeySet * removedKeys = elektraDiffGetRemovedKeys (diff);

// Get keys that are in both KeySets, but have either different values or different metadata.
// The returned keys will have the old values and metadata
KeySet * modifiedKeys = elektraDiffGetModifiedKeys (diff);

ksDel (addedKeys);
ksDel (removedKeys);
ksDel (modifiedKeys);
```

Use these methods to get information about single keys:

```c
Key * myKey; // A key that we want to check.

// whether the value of myKey has been changed
bool valueChanged = elektraDiffKeyValueChanged (diff, myKey);

// determine if only the metadata of myKey has been changed
bool onlyMetaChanged = elektraDiffKeyOnlyMetaChanged (diff, myKey);

// get all metadata that has been added to myKey
KeySet * addedMeta = elektraDiffGetAddedMetaKeys (diff, myKey);

// get all metadata that has been removed from myKey
KeySet * removedMeta = elektraDiffGetRemovedMetaKeys (diff, myKey);

// get all metadata that has been modified.
// the returned keyset will contain the old metadata
KeySet * modifiedMeta = elektraDiffGetModifiedMetaKeys (diff, myKey);

ksDel (addedMeta);
ksDel (removedMeta);
ksDel (modifiedMeta);
```
