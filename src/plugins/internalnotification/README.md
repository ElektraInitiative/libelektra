- infos = Plugin for internal notification
- infos/author = Thomas Wahringer <waht@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = postgetstorage postcommit
- infos/status = global libc unittest concept unfinished experimental nodoc
- infos/metadata =
- infos/description = Plugin for internal notification

## Usage ##

Allows applications to automatically update registered variables when the value of a specified key has changed.
Support for callbacks that fire when a specified key has changed is planned.

Until the wrapper (see [TODOs](#todos)) is finished it may be used as global plugin:
`$ sudo kdb global-mount internalnotification`

## Exported Methods ##

This plugin exports the function `int elektraInternalnotificationRegisterInt (Plugin * handle, Key * key, int * variable)`.
If the given key is contained in a KeySet on a kdbGet or kdbSet operation, its value is
converted from string to integer and the registered variable is updated with the current value.
This also works if the registered key is a cascading key.
The function address is exported as `system/elektra/modules/internalnotification/exports/registerInt`.
The plugin handle required for this plugin is exported as `system/elektra/modules/internalnotification/exports/handle`.
Please note that the plugin API may change as this plugin is experimental.

## Usage ##
Until the wrapper (see [TODOs](#todos)) is finished `elektraInternalnotificationRegisterInt` requires the plugin handle as first parameter.
This handle is normally only used internally, but exported by the plugin as `system/elektra/modules/internalnotification/exports/handle`.
The following snippet extracts the handle and the address of `elektraInternalnotificationRegisterInt` and provides a convenient function for
registering integer variables: `int internalnotificationRegisterInt (KDB * kdb, int * variable, Key * key)`.

```C
int internalnotificationRegisterInt (KDB * kdb, int * variable, Key * key)
{
	typedef int (*elektraInternalnotificationRegisterIntCallback) (void * handle, int * variable, Key * key);

	static size_t address = 0;
	static size_t handle;

	if (address == 0)
	{
		char * NOTIFICATION_BASE = "system/elektra/modules/internalnotification";
		char * EXPORTED_FUNCTION =
			"system/elektra/modules/internalnotification/"
			"exports/elektraInternalnotificationRegisterInt";
		char * EXPORTED_HANDLE = "system/elektra/modules/internalnotification/exports/handle";
		Key * parentKey = keyNew (NOTIFICATION_BASE, KEY_END);

		KeySet * conf = ksNew (20, KS_END);
		kdbGet (kdb, conf, parentKey);

		Key * keyFunction = ksLookupByName (conf, EXPORTED_FUNCTION, 0);
		if (keyFunction == 0 || !keyIsBinary (keyFunction))
		{
			// Key value is not binary
			return -1;
		}
		address = *(size_t *)keyValue (keyFunction);
		if (address == 0)
		{
			return -1;
		}

		Key * keyHandle = ksLookupByName (conf, EXPORTED_HANDLE, 0);
		if (keyHandle == 0 || !keyIsBinary (keyHandle))
		{
			// Key value is not binary
			return -1;
		}
		handle = *(size_t *)keyValue (keyHandle);
		if (handle == 0)
		{
			return -1;
		}
	}

	// Call function
	return ((elektraInternalnotificationRegisterIntCallback)address) ((void *)handle, variable, key);
}
```

## TODOs ##

- [ ] Create wrapper that mounts the plugin as global plugin (e.g. `elektraInternalnotificationInit(KDB * kdb)`) and eases usage of the register function (allow `KDB` handle to be passed e.g. `elektraInternalnotificationRegisterInt(KDB * kdb, int * variable, Key * key)`)
- [ ] Allow different data types for key values:
  - [x] integer: conversion from string value including range check
  - [ ] string: Just update string value
  - [ ] callback: Accept a callback (+ refactor plugin to use callbacks internally for integer and string types)
