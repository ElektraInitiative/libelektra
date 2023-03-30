/**
 * @file
 *
 * @brief Low level functions for access the Key Database.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <stdio.h>

#include <kdbassert.h>

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include <kdbchangetracking.h>
#include <kdbinternal.h>


#define KDB_GET_PHASE_POST_STORAGE_SPEC (KDB_GET_PHASE_POST_STORAGE "/spec")
#define KDB_GET_PHASE_POST_STORAGE_NONSPEC (KDB_GET_PHASE_POST_STORAGE "/nonspec")

/**
 * @internal
 *
 * @brief appends duplicated keys from @p toAppend to @p ks
 *
 * @param ks the KeySet that will receive the Keys
 * @param toAppend the KeySet that provides the Keys that will be duplicated
 */
static void ksAppendDup (KeySet * ks, const KeySet * toAppend)
{
	if (ks == NULL || toAppend == NULL)
	{
		return;
	}

	for (elektraCursor i = 0; i < ksGetSize (toAppend); i++)
	{
		ksAppendKey (ks, keyDup (ksAtCursor (toAppend, i), KEY_CP_ALL));
	}
}

/**
 * @defgroup kdb KDB
 * @brief General methods to access the Key database.
 *
 * To use them:
 * @code
 * #include <kdb.h>
 * @endcode
 *
 * The kdb*() methods are used to access the storage, to get and set
 * @link keyset KeySets@endlink.
 *
 * Parameters common for all these functions are:
 *
 * - *handle*, as returned by kdbOpen(), need to be passed to every call
 * - *parentKey* is used for every call to add warnings and set an
 *   error. For kdbGet() / kdbSet() it is used to specify which keys
 *   should be retrieved/stored.
 *
 * @note The parentKey is an obligation for you, but only an hint for KDB.
 * KDB does not remember anything
 * about the configuration. You need to pass the same configuration
 * back to kdbSet(), otherwise parts of the configuration get
 * lost. Only keys below the parentKey are subject for change, the rest
 * must be left untouched.
 *
 * KDB uses different backend implementations that know the details
 * about how to access the storage.
 * One backend consists of multiple plugins.
 * See @link plugin writing a new plugin @endlink for information
 * about how to write a plugin.
 * Backends are state-less regarding the configuration (because of that
 * you must pass back the whole configuration for every backend), but
 * have a state for:
 *
 * - a two phase-commit
 * - a conflict detection (error C02000) and
 * - optimizations that avoid redoing already done operations.
 *
 * @mermaid{state}
 * @image latex state.png "State"
 *
 * As we see in the figure, kdbOpen() can be called arbitrarily often in any
 * number of threads.
 *
 * For every handle you got from kdbOpen(), for every parentKey with a
 * different name, *only* the shown state transitions
 * are valid. From a freshly opened KDB, only kdbGet() and kdbClose()
 * are allowed, because otherwise conflicts (error C02000) would not be detected.
 *
 * Once kdbGet() was called (for a specific handle+parentKey),
 * any number of kdbGet() and kdbSet() can be
 * used with this handle respective parentKey, unless kdbSet() had
 * a conflict (error C02000) with another application.
 * Every affair with KDB needs to be finished with kdbClose().
 *
 * The name of the parentKey in kdbOpen() and kdbClose() does not matter.
 *
 * In the usual case we just have one parentKey and one handle. In
 * these cases we just have to remember to use kdbGet() before
 * kdbSet():
 *
 * @include kdbintro.c
 *
 * To output warnings, you can use following code:
 *
 * @snippet tests.c warnings
 *
 * To output the error, you can use following code:
 *
 * @snippet tests.c error
 *
 * @{
 */

static bool closeBackends (KeySet * backends, Key * errorKey)
{
	for (elektraCursor i = 0; i < ksGetSize (backends); i++)
	{
		Key * backendKey = ksAtCursor (backends, i);

		// The cast is necessary, as keyValue would return (const *)
		BackendData * backendData = (BackendData *) keyValue (backendKey);

		for (elektraCursor p = 0; p < ksGetSize (backendData->plugins); p++)
		{
			Plugin * plugin = *(Plugin **) keyValue (ksAtCursor (backendData->plugins, p));

			if (plugin == backendData->backend)
			{
				// Set the backend to NULL, so the call to elektraPluginClose (backendData->backend, ...) is a NOP.
				// Otherwise we would cause an error by closing it twice.
				backendData->backend = NULL;
			}

			if (elektraPluginClose (plugin, errorKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
			{
				return false;
			}
		}

		elektraPluginClose (backendData->backend, errorKey);

		ksDel (backendData->plugins);
		ksDel (backendData->keys);
		ksDel (backendData->definition);
	}

	ksDel (backends);
	return true;
}

/**
 * @brief Takes the first key and cuts off this common part
 * for all other keys, instead name will be prepended
 *
 * @return a new allocated keyset with keys in user namespace.
 *
 * The first key is removed in the resulting keyset.
 */
KeySet * ksRenameKeys (KeySet * config, const char * name)
{
	Key * root;
	Key * cur;
	ssize_t rootSize = 0;

	root = ksAtCursor (config, 0);
	rootSize = keyGetNameSize (root);

	keyDel (ksLookup (config, root, KDB_O_POP));

	KeySet * newConfig = ksNew (ksGetSize (config), KS_END);
	if (rootSize == -1) return newConfig;

	while ((cur = ksPop (config)) != 0)
	{
		Key * dupKey = keyDup (cur, KEY_CP_ALL);
		keySetName (dupKey, name);
		keyAddName (dupKey, keyName (cur) + rootSize - 1);
		ksAppendKey (newConfig, dupKey);
		keyDel (cur);
	}

	return newConfig;
}

static void clearErrorAndWarnings (Key * key)
{
	Key * cutRoot = keyNew ("meta:/error", KEY_END);
	ksDel (ksCut (keyMeta (key), cutRoot));
	keySetName (cutRoot, "meta:/warnings");
	ksDel (ksCut (keyMeta (key), cutRoot));
	keyDel (cutRoot);
}

/**
 * Handles the system:/elektra/contract/globalkeyset part of kdbOpen() contracts
 *
 * NOTE: @p contract will be modified
 *
 * @see kdbOpen()
 */
static void ensureContractGlobalKs (KDB * handle, KeySet * contract)
{
	Key * globalKsContractRoot = keyNew ("system:/elektra/contract/globalkeyset", KEY_END);
	Key * globalKsRoot = keyNew ("system:/elektra", KEY_END);

	KeySet * globalKs = ksCut (contract, globalKsContractRoot);

	ksRename (globalKs, globalKsContractRoot, globalKsRoot);

	ksAppend (handle->global, globalKs);

	ksDel (globalKs);
	keyDel (globalKsContractRoot);
	keyDel (globalKsRoot);
}


/**
 * Handles the @p contract argument of kdbOpen().
 *
 * @see kdbOpen()
 */
static bool ensureContract (KDB * handle, const KeySet * contract)
{
	// FIXME [new_backend]: tests needed
	// deep dupContract, so modifications to the keys in contract after kdbOpen() cannot modify the contract
	KeySet * dupContract = ksDeepDup (contract);

	ensureContractGlobalKs (handle, dupContract);

	ksDel (dupContract);

	return true;
}

/**
 * @internal
 *
 * Helper for kdbOpen(). Creates empty KDB instance.
 *
 * @see kdbOpen()
 */
static KDB * kdbNew (Key * errorKey)
{
	KDB * handle = elektraCalloc (sizeof (struct _KDB));
	handle->modules = ksNew (0, KS_END);
	if (elektraModulesInit (handle->modules, errorKey) == -1)
	{
		ELEKTRA_SET_INSTALLATION_ERROR (
			errorKey, "Method 'elektraModulesInit' returned with -1. See other warning or error messages for concrete details");

		ksDel (handle->modules);
		elektraFree (handle);
		return NULL;
	}
	handle->global =
		ksNew (1, keyNew ("system:/elektra/kdb", KEY_BINARY, KEY_SIZE, sizeof (handle), KEY_VALUE, &handle, KEY_END), KS_END);
	handle->backends = ksNew (0, KS_END);

	return handle;
}

/**
 * Adds a new backend to @p backends
 * Takes ownership of the following params, so don't free them until you close the backends:
 *  - @p backend
 *  - @p plugins
 *  - @p definition
 *
 * @param backends the keyset where the new backend will be addeed
 * @param mountpoint the key for the mountpoint. The backend will be set as the data of that key. The key will then be added to @p backends
 * @param backend the backend plugin
 * @param plugins plugins for the backend
 * @param definition configuration for the backend
 */
static void addMountpoint (KeySet * backends, Key * mountpoint, Plugin * backend, KeySet * plugins, KeySet * definition)
{
	BackendData backendData = {
		.backend = backend,
		.keys = ksNew (0, KS_END),
		.plugins = plugins,
		.definition = definition,
		.getSize = 0,
		.initialized = false,
	};
	keySetBinary (mountpoint, &backendData, sizeof (backendData));
	ksAppendKey (backends, mountpoint);
}

static bool addElektraMountpoint (KeySet * backends, KeySet * modules, KeySet * global, Key * errorKey)
{
	// TODO [new_backend]: implement user:/elektra and dir:/elektra
	// TODO [new_backend]: replace KDB_DEFAULT_STORAGE with separate KDB_BOOTSTRAP_STORAGE
	Plugin * storage = elektraPluginOpen (KDB_DEFAULT_STORAGE, modules, ksNew (0, KS_END), errorKey);
	if (storage == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "Could not open boostrap storage plugin ('%s'). See warnings for details.",
						 KDB_DEFAULT_STORAGE);
		return false;
	}
	storage->global = global;

	Plugin * resolver = elektraPluginOpen (KDB_DEFAULT_RESOLVER, modules, ksNew (0, KS_END), errorKey);
	if (resolver == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "Could not open boostrap resolver plugin ('%s'). See warnings for details.",
						 KDB_DEFAULT_RESOLVER);
		elektraPluginClose (resolver, errorKey);
		return false;
	}
	resolver->global = global;

	Plugin * backend = elektraPluginOpen ("backend", modules, ksNew (0, KS_END), errorKey);
	if (backend == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey,
						"Could not open system:/elektra backend during bootstrap. See other warnings for details");
		elektraPluginClose (resolver, errorKey);
		elektraPluginClose (storage, errorKey);
		return false;
	}
	backend->global = global;

	// clang-format off
	KeySet * plugins =
		ksNew (1,
			keyNew ("system:/#0", KEY_BINARY, KEY_SIZE, sizeof (resolver), KEY_VALUE, &resolver, KEY_END),
			keyNew ("system:/#1", KEY_BINARY, KEY_SIZE, sizeof (storage), KEY_VALUE, &storage, KEY_END),
		KS_END);
	KeySet * definition =
		ksNew (3,
			keyNew ("system:/path", KEY_VALUE, KDB_DB_INIT, KEY_END),
			keyNew ("system:/positions/get/resolver", KEY_VALUE, "#0", KEY_END),
			keyNew ("system:/positions/get/storage", KEY_VALUE, "#1", KEY_END),
			keyNew ("system:/positions/set/resolver", KEY_VALUE, "#0", KEY_END),
			keyNew ("system:/positions/set/storage", KEY_VALUE, "#1", KEY_END),
			keyNew ("system:/positions/set/commit", KEY_VALUE, "#0", KEY_END),
			keyNew ("system:/positions/set/rollback", KEY_VALUE, "#0", KEY_END),
		KS_END);
	// clang-format on

	addMountpoint (backends, keyNew (KDB_SYSTEM_ELEKTRA, KEY_END), backend, plugins, definition);

	return true;
}

static KeySet * elektraBoostrap (KDB * handle, Key * errorKey)
{
	KeySet * elektraKs = ksNew (0, KS_END);
	Key * bootstrapParent = keyNew (KDB_SYSTEM_ELEKTRA, KEY_END);

	if (kdbGet (handle, elektraKs, bootstrapParent) == -1)
	{
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey,
						"Bootstrapping failed, please fix '" KDB_DB_SYSTEM "/" KDB_DB_INIT
						"'. If the error persists, please report this bug at https://issues.libelektra.org.");

		Key * warningsRoot = keyNew ("meta:/warnings", KEY_END);
		KeySet * warnings = ksBelow (keyMeta (bootstrapParent), warningsRoot);
		ksAppend (keyMeta (errorKey), warnings);
		ksDel (warnings);
		keyDel (warningsRoot);
		elektraTriggerWarnings (keyString (keyGetMeta (bootstrapParent, "meta:/error/number")), errorKey,
					keyString (keyGetMeta (bootstrapParent, "meta:/error/reason")));
		ksDel (elektraKs);
		keyDel (bootstrapParent);

		return NULL;
	}
	keyDel (bootstrapParent);

	return elektraKs;
}

static bool openPlugins (KeySet * plugins, const Key * pluginsRoot, KeySet * modules, KeySet * global, const KeySet * systemConfig,
			 Key * errorKey)
{
	bool success = true;
	for (elektraCursor i = 0; i < ksGetSize (plugins); i++)
	{
		Key * cur = ksAtCursor (plugins, i);
		if (keyIsDirectlyBelow (pluginsRoot, cur) == 1)
		{
			Key * lookupHelper = keyDup (cur, KEY_CP_NAME);
			keyAddBaseName (lookupHelper, "name");

			Key * nameKey = ksLookup (plugins, lookupHelper, 0);
			const char * pluginName = nameKey == NULL ? NULL : keyString (nameKey);
			if (nameKey == NULL || strlen (pluginName) == 0)
			{
				ELEKTRA_ADD_INSTALLATION_WARNINGF (errorKey,
								   "The plugin definition at '%s' doesn't contain a plugin name. Please "
								   "set '%s/name' to a non-empty string value.",
								   keyName (cur), keyName (cur));
				success = false;
				keyDel (lookupHelper);
				continue;
			}

			keySetBaseName (lookupHelper, "config");
			KeySet * config = ksBelow (plugins, lookupHelper);
			Key * configRoot = keyNew ("user:/", KEY_END);
			ksRename (config, lookupHelper, configRoot);
			keyDel (configRoot);

			ksAppend (config, systemConfig);

			keyDel (lookupHelper);

			Plugin * plugin = elektraPluginOpen (pluginName, modules, config, errorKey);
			if (plugin == NULL)
			{
				ELEKTRA_ADD_INSTALLATION_WARNINGF (
					errorKey, "Could not open the plugin '%s' defined at '%s'. See other warnings for details.",
					pluginName, keyName (cur));
				success = false;
				continue;
			}
			plugin->global = global;

			// create Plugin * key ...
			Key * pluginKey = keyDup (cur, KEY_CP_NAME);
			keySetBinary (pluginKey, &plugin, sizeof (plugin));

			// ... remove definition (includes cur) ...
			ksDel (ksCut (plugins, cur));

			// ... and replace Plugin * key
			ksAppendKey (plugins, pluginKey);
		}
		else
		{
			ELEKTRA_ADD_INSTALLATION_WARNINGF (
				errorKey,
				"The key '%s' doesn't belong to a plugin definition. Keys below '%s' must be part of a plugin definition.",
				keyName (cur), keyName (pluginsRoot));
			success = false;
			continue;
		}
	}

	return success;
}

static KeySet * dupPluginSet (KeySet * plugins, Key * errorKey)
{
	KeySet * dupPlugins = ksNew (ksGetSize (plugins), KS_END);
	bool success = true;
	for (elektraCursor i = 0; i < ksGetSize (plugins); i++)
	{
		Key * cur = ksAtCursor (plugins, i);
		const Plugin * plugin = *(const Plugin **) keyValue (cur);
		Plugin * dup = elektraPluginOpen (plugin->name, plugin->modules, ksDup (plugin->config), errorKey);
		if (dup == NULL)
		{
			ELEKTRA_ADD_INSTALLATION_WARNINGF (
				errorKey, "Could not open the plugin '%s' defined at '%s'. See other warnings for details.", plugin->name,
				keyName (cur));
			success = false;
			continue;
		}
		dup->global = plugin->global;

		Key * dupKey = keyDup (cur, KEY_CP_NAME);
		keySetBinary (dupKey, &dup, sizeof (dup));
		ksAppendKey (dupPlugins, dupKey);
	}

	if (!success)
	{
		ksDel (dupPlugins);
		return NULL;
	}
	else
	{
		return dupPlugins;
	}
}

/**
 * Adds a duplicated mounpoint
 * Creates duplicates of
 *  - @p mountpoint
 *  - @p plugins, including reopening of the plugins
 *  - @p definition
 *
 * @param mountpoints KeySet containing all backends.
 * @param mountpoint The mountpoint where the backend should be mounted.
 * @param plugins KeySet containing all plugins to use, including the backend plugin. The backend plugin has the key system:/backend.
 * @param definition KeySet containing the definition/configuration for the backend plugin.
 * @param errorKey The error key.
 * @return true if the mounting was successful, false otherwise
 */
static bool addDupMountpoint (KeySet * mountpoints, Key * mountpoint, KeySet * plugins, KeySet * definition, Key * errorKey)
{
	KeySet * dupPlugins = dupPluginSet (plugins, errorKey);
	if (dupPlugins == NULL)
	{
		return false;
	}
	Plugin * backendPlugin = *(Plugin **) keyValue (ksLookupByName (dupPlugins, "system:/backend", 0));
	addMountpoint (mountpoints, keyDup (mountpoint, KEY_CP_NAME), backendPlugin, dupPlugins, ksDup (definition));
	return true;
}

static bool parseAndAddMountpoint (KeySet * mountpoints, KeySet * modules, KeySet * elektraKs, KeySet * global, Key * root, Key * errorKey)
{
	// check that the base name is a key name
	Key * mountpoint = keyNew (keyBaseName (root), KEY_END);
	if (mountpoint == NULL)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (errorKey, "'%s' is not a valid key name, but is used for the mountpoint '%s'",
						   keyBaseName (root), keyName (root));
		return false;
	}

	// TODO [new_backend]: reserve /elektra/... in every namespace
	Key * elektraRoot = keyNew (KDB_SYSTEM_ELEKTRA, KEY_END);
	if (keyIsBelowOrSame (elektraRoot, mountpoint) != 0)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (
			errorKey,
			"The mountpoint '%s' (defined at '%s') is not allowed. Everything below '" KDB_SYSTEM_ELEKTRA
			"' is reserved for use by Elektra.",
			keyBaseName (root), keyName (root));

		keyDel (elektraRoot);
		return false;
	}
	keyDel (elektraRoot);
	elektraRoot = NULL;


	// load mountpoint level config
	Key * lookupHelper = keyDup (root, KEY_CP_NAME);
	keyAddBaseName (lookupHelper, "config");
	KeySet * systemConfig = ksBelow (elektraKs, lookupHelper);
	Key * configRoot = keyNew ("system:/", KEY_END);
	ksRename (systemConfig, lookupHelper, configRoot);
	keyDel (configRoot);


	// get the plugin list and remove the common prefix
	keySetBaseName (lookupHelper, "plugins");
	KeySet * plugins = ksBelow (elektraKs, lookupHelper);

	// open all plugins (replaces key values with Plugin *)
	if (!openPlugins (plugins, lookupHelper, modules, global, systemConfig, errorKey))
	{
		keyDel (mountpoint);
		keyDel (lookupHelper);
		ksDel (plugins);
		ksDel (systemConfig);
		return false;
	}

	// TODO [new_backend]: read and process config/needs from contract
	ksDel (systemConfig);

	Key * pluginsRoot = keyNew ("system:/", KEY_END);
	ksRename (plugins, lookupHelper, pluginsRoot);
	keyDel (pluginsRoot);

	// declare early because of goto error
	KeySet * definition = NULL;

	// find backend plugin
	Key * backendPluginKey = ksLookupByName (plugins, "system:/backend", 0);
	if (backendPluginKey == NULL)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (errorKey, "The mountpoint '%s' defined in '%s' does not specify a backend plugin.",
						   keyName (mountpoint), keyName (root));
		keyDel (lookupHelper);

		goto error;
	}

	// get definition section
	keySetBaseName (lookupHelper, "definition");
	definition = ksBelow (elektraKs, lookupHelper);
	Key * definitionRoot = keyNew ("system:/", KEY_END);
	ksRename (definition, lookupHelper, definitionRoot);
	keyDel (definitionRoot);

	keyDel (lookupHelper);

	// create mountpoint
	if (keyGetNamespace (mountpoint) == KEY_NS_CASCADING)
	{
		keySetNamespace (mountpoint, KEY_NS_SYSTEM);
		if (!addDupMountpoint (mountpoints, mountpoint, plugins, definition, errorKey))
		{
			goto error;
		}

		keySetNamespace (mountpoint, KEY_NS_USER);
		if (!addDupMountpoint (mountpoints, mountpoint, plugins, definition, errorKey))
		{
			goto error;
		}

		keySetNamespace (mountpoint, KEY_NS_DIR);
		if (!addDupMountpoint (mountpoints, mountpoint, plugins, definition, errorKey))
		{
			goto error;
		}

		keySetNamespace (mountpoint, KEY_NS_PROC);
		if (!addDupMountpoint (mountpoints, mountpoint, plugins, definition, errorKey))
		{
			goto error;
		}

		// addDupMounptoint duplicates everything, including reopening the plugins
		// so we have to close the originals
		for (elektraCursor it = 0; it < ksGetSize (plugins); it++)
		{
			Plugin * plugin = *(Plugin **) keyValue (ksAtCursor (plugins, it));
			elektraPluginClose (plugin, errorKey);
		}

		ksDel (plugins);
		ksDel (definition);
		keyDel (mountpoint);

		return true;
	}

	Plugin * backendPlugin = *(Plugin **) keyValue (backendPluginKey);

	addMountpoint (mountpoints, mountpoint, backendPlugin, plugins, definition);
	keyDel (mountpoint);
	// Don't delete plugins, definition as addMountpoint takes ownership of it

	return true;

error:
	for (elektraCursor it = 0; it < ksGetSize (plugins); it++)
	{
		Plugin * plugin = *(Plugin **) keyValue (ksAtCursor (plugins, it));
		elektraPluginClose (plugin, errorKey);
	}

	ksDel (plugins);
	ksDel (definition);
	keyDel (mountpoint);
	return false;
}

// FIXME [new_backend]: tests needed
KeySet * elektraMountpointsParse (KeySet * elektraKs, KeySet * modules, KeySet * global, Key * errorKey)
{
	KeySet * mountpoints = ksNew (0, KS_END);

	Key * mountpointsRoot = keyNew (KDB_SYSTEM_ELEKTRA "/mountpoints", KEY_END);

	bool error = false;
	for (elektraCursor end, i = ksFindHierarchy (elektraKs, mountpointsRoot, &end); i < end;)
	{
		Key * cur = ksAtCursor (elektraKs, i);
		if (keyIsDirectlyBelow (mountpointsRoot, cur) == 1)
		{
			if (!parseAndAddMountpoint (mountpoints, modules, elektraKs, global, cur, errorKey))
			{
				error = true;
			}

			// skip over the keys we just parsed
			Key * lookup = keyDup (cur, KEY_CP_NAME);
			ksFindHierarchy (elektraKs, lookup, &i);
			keyDel (lookup);
		}
		else
		{
			// TODO [new_backend]: use real mountpoint in error message
			ELEKTRA_ADD_INSTALLATION_WARNINGF (
				errorKey,
				"The key '%s' is below 'system:/elektra/mountpoints', but doesn't belong to a mountpoint configuration. To "
				"define a mountpoint for the parent e.g. 'user:/mymountpoint' the key "
				"'system:/elektra/user:\\/mymountpoint' must exist and be set to an arbitrary (possibly empty) value.",
				keyName (cur));
			++i;
		}
	}

	keyDel (mountpointsRoot);

	if (error)
	{
		closeBackends (mountpoints, errorKey);
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Some mountpoints couldn't be parsed. See warnings for details.");
		return NULL;
	}

	return mountpoints;
}

static bool addRootMountpoint (KeySet * backends, elektraNamespace ns, KeySet * modules, KeySet * global, Key * errorKey)
{
	Key * rootKey = keyNew ("/", KEY_END);
	keySetNamespace (rootKey, ns);
	if (ksLookup (backends, rootKey, 0) != NULL)
	{
		// already present
		keyDel (rootKey);
		return true;
	}

	Plugin * defaultResolver = elektraPluginOpen (KDB_RESOLVER, modules, ksNew (0, KS_END), errorKey);
	if (defaultResolver == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Could not open default resolver plugin. See warnings for details.");
		return false;
	}
	defaultResolver->global = global;

	Plugin * defaultStorage = elektraPluginOpen (KDB_STORAGE, modules, ksNew (0, KS_END), errorKey);
	if (defaultStorage == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Could not open default storage plugin. See warnings for details.");
		elektraPluginClose (defaultResolver, errorKey);
		return false;
	}
	defaultStorage->global = global;

	// clang-format off
	KeySet * rootPlugins =
		ksNew (2,
			keyNew ("system:/resolver", KEY_BINARY, KEY_SIZE, sizeof (defaultResolver), KEY_VALUE, &defaultResolver, KEY_END),
			keyNew ("system:/storage", KEY_BINARY, KEY_SIZE, sizeof (defaultStorage), KEY_VALUE, &defaultStorage, KEY_END),
		KS_END);

	KeySet * rootDefinition =
		ksNew (7,
			keyNew ("system:/path", KEY_VALUE, KDB_DB_FILE, KEY_END),
			keyNew ("system:/positions/get/resolver", KEY_VALUE, "resolver", KEY_END),
			keyNew ("system:/positions/get/storage", KEY_VALUE, "storage", KEY_END),
			keyNew ("system:/positions/set/resolver", KEY_VALUE, "resolver", KEY_END),
			keyNew ("system:/positions/set/storage", KEY_VALUE, "storage", KEY_END),
			keyNew ("system:/positions/set/commit", KEY_VALUE, "resolver", KEY_END),
			keyNew ("system:/positions/set/rollback", KEY_VALUE, "resolver", KEY_END),
		KS_END);
	// clang-format on

	Plugin * root = elektraPluginOpen ("backend", modules, ksNew (0, KS_END), errorKey);
	if (root == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Could not open default backend. See warnings for details.");

		ksDel (rootPlugins);
		ksDel (rootDefinition);
		elektraPluginClose (defaultResolver, errorKey);
		elektraPluginClose (defaultStorage, errorKey);
		return false;
	}
	root->global = global;

	addMountpoint (backends, rootKey, root, rootPlugins, rootDefinition);
	return true;
}

static bool addModulesMountpoint (KDB * handle, Key * mountpoint, Key * errorKey)
{
	Plugin * modules = elektraPluginOpen ("modules", handle->modules, ksNew (0, KS_END), errorKey);
	if (modules == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (
			errorKey, "Could not open 'modules' plugin for mountpoint 'system:/elektra/modules/%s'. See warnings for details.",
			keyBaseName (mountpoint));
		return false;
	}

	Plugin * plugin = elektraPluginOpen (keyBaseName (mountpoint), handle->modules,
					     ksNew (1, keyNew ("system:/module", KEY_END), KS_END), errorKey);
	if (plugin == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (
			errorKey, "Could not open '%s' plugin for mountpoint 'system:/elektra/modules/%s'. See warnings for details.",
			keyBaseName (mountpoint), keyBaseName (mountpoint));
		return false;
	}
	modules->global = handle->global;
	addMountpoint (handle->backends, mountpoint, modules,
		       ksNew (1, keyNew ("system:/plugin", KEY_BINARY, KEY_SIZE, sizeof (plugin), KEY_VALUE, &plugin, KEY_END), KS_END),
		       ksNew (0, KS_END));
	return true;
}

static bool addHardcodedMountpoints (KDB * handle, Key * errorKey)
{
	if (!addElektraMountpoint (handle->backends, handle->modules, handle->global, errorKey))
	{
		return false;
	}

	if (!addRootMountpoint (handle->backends, KEY_NS_SPEC, handle->modules, handle->global, errorKey))
	{
		return false;
	}
	if (!addRootMountpoint (handle->backends, KEY_NS_SYSTEM, handle->modules, handle->global, errorKey))
	{
		return false;
	}
	if (!addRootMountpoint (handle->backends, KEY_NS_USER, handle->modules, handle->global, errorKey))
	{
		return false;
	}
	if (!addRootMountpoint (handle->backends, KEY_NS_DIR, handle->modules, handle->global, errorKey))
	{
		return false;
	}
	if (!addRootMountpoint (handle->backends, KEY_NS_PROC, handle->modules, handle->global, errorKey))
	{
		return false;
	}

	Key * modulesRoot = keyNew (KDB_SYSTEM_ELEKTRA "/modules", KEY_END);
	Plugin * modules = elektraPluginOpen ("modules", handle->modules, ksNew (0, KS_END), errorKey);
	if (modules == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Could not open system:/elektra/modules backend. See warnings for details.");
		return false;
	}
	modules->global = handle->global;
	addMountpoint (handle->backends, modulesRoot, modules, ksNew (0, KS_END), ksNew (0, KS_END));

	for (elektraCursor i = 0; i < ksGetSize (handle->modules); i++)
	{
		Key * cur = ksAtCursor (handle->modules, i);
		if (keyIsDirectlyBelow (modulesRoot, cur) != 1)
		{
			continue;
		}

		if (!addModulesMountpoint (handle, keyDup (cur, KEY_CP_NAME), errorKey))
		{
			return false;
		}
	}

	Plugin * version = elektraPluginOpen ("version", handle->modules, ksNew (0, KS_END), errorKey);
	if (version == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Could not open system:/elektra/version backend. See warnings for details.");
		return false;
	}
	version->global = handle->global;
	addMountpoint (handle->backends, keyNew (KDB_SYSTEM_ELEKTRA "/version", KEY_END), version, ksNew (0, KS_END), ksNew (0, KS_END));

	return true;
}

/**
 * Opens the session with the Key database.
 *
 * @pre errorKey must be a valid key, e.g. created with keyNew()
 *
 * You must always call this method before retrieving or committing any
 * keys to the database. At the end of a program, after using the Key database (KDB),
 * you must not forget to call kdbClose() to free resources.
 *
 * The method will bootstrap itself in the following way.
 * The first step is to open the default backend. With it
 * `system:/elektra/mountpoints` will be loaded and all needed
 * libraries and mountpoints will be determined.
 * Then the global plugins and global keyset data from the @p contract
 * is processed.
 * Finally, the libraries for backends will be loaded and with it the
 * @p KDB data structure will be initialized.
 *
 * The pointer to the @p KDB structure returned will be initialized
 * like described above, and it must be passed along on any kdb*()
 * method your application calls.
 *
 * Get a @p KDB handle for every thread using elektra. Don't share the
 * handle across threads, and also not the pointer accessing it:
 *
 * @snippet kdbopen.c open
 *
 * You don't need kdbOpen() if you only want to
 * manipulate plain in-memory Key or KeySet objects.
 *
 * @pre errorKey must be a valid key, e.g. created with keyNew()
 *
 * @param contract the contract that should be ensured before opening the KDB
 *                 all data is copied and the KeySet can safely be used for
 *                 e.g. kdbGet() later
 * @param errorKey the key which holds errors and warnings which were issued
 *
 * @return handle to the newly created KDB on success
 * @retval NULL on failure
 *
 * @since 1.0.0
 * @ingroup kdb
 * @see kdbClose() to close the session of a Key database opened by kdbOpen()
 */
KDB * kdbOpen (const KeySet * contract, Key * errorKey)
{
	if (!errorKey)
	{
		ELEKTRA_LOG_WARNING ("no error key passed");
		return 0;
	}

	ELEKTRA_LOG ("called with %s", keyName (errorKey));
	Key * initialParent = keyDup (errorKey, KEY_CP_ALL);

	int errnosave = errno;

	// Step 1: create empty KDB instance
	KDB * handle = kdbNew (errorKey);
	if (handle == NULL)
	{
		goto error;
	}

	// Step 2: configure for bootstrap
	if (!addElektraMountpoint (handle->backends, handle->modules, handle->global, errorKey))
	{
		goto error;
	}

	// Step 3: execute bootstrap
	KeySet * elektraKs = elektraBoostrap (handle, errorKey);
	if (elektraKs == NULL)
	{
		goto error;
	}

	// Step 4: process contract and set up hooks
	// TODO (atmaxinger): improve
	// TODO: combine with ensureContract below
	if (initHooks (handle, elektraKs, handle->modules, contract, errorKey) == -1)
	{
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Initializing hooks failed. Please see warning of concrete plugin");
		ksDel (elektraKs);
		goto error;
	}

	if (contract != NULL && !ensureContract (handle, contract))
	{
		ksDel (elektraKs);
		goto error;
	}

	ksAppendKey (handle->global, ksLookupByName (elektraKs, ELEKTRA_RECORD_CONFIG_ACTIVE_KEY, 0));

	// Step 5: parse mountpoints
	KeySet * backends = elektraMountpointsParse (elektraKs, handle->modules, handle->global, errorKey);
	if (backends == NULL)
	{
		ksDel (elektraKs);
		goto error;
	}

	// Step 6: switch from boostrap to real config
	ksDel (elektraKs);
	keyCopy (errorKey, initialParent, KEY_CP_NAME | KEY_CP_VALUE);

	if (!closeBackends (handle->backends, errorKey))
	{
		goto error;
	}

	handle->backends = backends;

	// Step 7: add hardcoded mountpoints
	if (!addHardcodedMountpoints (handle, errorKey))
	{
		goto error;
	}

	keyCopy (errorKey, initialParent, KEY_CP_NAME | KEY_CP_VALUE);
	keyDel (initialParent);
	errno = errnosave;

	handle->allKeys = ksNew (0, KS_END);
	ksIncRef (handle->allKeys);
	handle->changeTrackingContext.oldKeys = handle->allKeys;

	return handle;

error:
	if (handle != NULL)
	{
		Key * closeKey = keyNew ("/", KEY_END);
		kdbClose (handle, closeKey);
		keyDel (closeKey);
	}

	keyCopy (errorKey, initialParent, KEY_CP_NAME | KEY_CP_VALUE);
	keyDel (initialParent);
	errno = errnosave;
	return NULL;
}


/**
 * Closes the session with the Key database.
 *
 * @pre The handle must be a valid handle as returned from kdbOpen()
 * @pre errorKey must be a valid key, e.g. created with keyNew()
 *
 * This is the counterpart of kdbOpen().
 *
 * You must call this method when you are finished working with the Key
 * database. You can manipulate Key and KeySet objects also after
 * kdbClose(), but you must not use any kdb*() call afterwards.
 *
 * The @p handle parameter will be finalized and all resources associated to it
 * will be freed. After a kdbClose(), the @p handle cannot be used anymore.
 *
 * @param handle contains internal information of
 *               @link kdbOpen() opened @endlink key database
 * @param errorKey the key which holds error/warning information
 *
 * @retval 0 on success
 * @retval -1 on NULL pointer
 *
 * @since 1.0.0
 * @ingroup kdb
 * @see kdbOpen() for opening a session with a Key database
 */
int kdbClose (KDB * handle, Key * errorKey)
{
	if (!handle)
	{
		return -1;
	}

	Key * initialParent = keyDup (errorKey, KEY_CP_ALL);
	int errnosave = errno;

	if (handle->allKeys)
	{
		ksDecRef (handle->allKeys);
		ksDel (handle->allKeys);
		handle->allKeys = NULL;
	}

	if (handle->backends)
	{

		closeBackends (handle->backends, errorKey);
		handle->backends = NULL;
	}

	freeHooks (handle, errorKey);

	if (handle->modules)
	{
		elektraModulesClose (handle->modules, errorKey);
		ksDel (handle->modules);
	}
	else
	{
		ELEKTRA_ADD_RESOURCE_WARNING (errorKey, "Could not close modules: modules were not open");
	}

	if (handle->global)
	{
		ksDel (handle->global);
	}

	elektraFree (handle);

	keyCopy (errorKey, initialParent, KEY_CP_NAME | KEY_CP_VALUE);
	keyDel (initialParent);
	errno = errnosave;
	return 0;
}

#if 2 == 0
// TODO [new_backend]: re-implement cache
static int elektraCacheCheckParent (KeySet * global, Key * cacheParent, Key * initialParent)
{
	const char * cacheName = keyGetNamespace (cacheParent) == KEY_NS_DEFAULT ? "" : keyName (cacheParent);

	// first check if parentkey matches
	Key * lastParentName = ksLookupByName (global, KDB_CACHE_PREFIX "/lastParentName", KDB_O_NONE);
	ELEKTRA_LOG_DEBUG ("LAST PARENT name: %s", keyString (lastParentName));
	ELEKTRA_LOG_DEBUG ("KDBG PARENT name: %s", cacheName);
	if (!lastParentName || elektraStrCmp (keyString (lastParentName), cacheName)) return -1;

	const char * cacheValue = keyGetNamespace (cacheParent) == KEY_NS_DEFAULT ? "default" : keyString (cacheParent);

	Key * lastParentValue = ksLookupByName (global, KDB_CACHE_PREFIX "/lastParentValue", KDB_O_NONE);
	ELEKTRA_LOG_DEBUG ("LAST PARENT value: %s", keyString (lastParentValue));
	ELEKTRA_LOG_DEBUG ("KDBG PARENT value: %s", cacheValue);
	if (!lastParentValue || elektraStrCmp (keyString (lastParentValue), cacheValue)) return -1;

	Key * lastInitalParentName = ksLookupByName (global, KDB_CACHE_PREFIX "/lastInitialParentName", KDB_O_NONE);
	Key * lastInitialParent = keyNew (keyString (lastInitalParentName), KEY_END);
	ELEKTRA_LOG_DEBUG ("LAST initial PARENT name: %s", keyName (lastInitialParent));
	ELEKTRA_LOG_DEBUG ("CURR initial PARENT name: %s", keyName (initialParent));

	if (!keyIsBelowOrSame (lastInitialParent, initialParent))
	{
		ELEKTRA_LOG_DEBUG ("CACHE initial PARENT: key is not below or same");
		keyDel (lastInitialParent);
		return -1;
	}

	keyDel (lastInitialParent);
	return 0;
}

static void elektraCacheCutMeta (KDB * handle)
{
	Key * parentKey = keyNew (KDB_CACHE_PREFIX, KEY_END);
	ksDel (ksCut (handle->global, parentKey));
	keyDel (parentKey);
}

KeySet * elektraCutProc (KeySet * ks)
{
	Key * parentKey = keyNew ("proc:/", KEY_END);
	KeySet * ret = ksCut (ks, parentKey);
	keyDel (parentKey);
	return ret;
}

static void elektraRestoreProc (KeySet * ks, KeySet * proc)
{
	ksAppend (ks, proc);
	ksDel (proc);
}

static void elektraCacheLoad (KDB * handle, KeySet * cache, Key * parentKey, Key * initialParent ELEKTRA_UNUSED, Key * cacheParent)
{
	// prune old cache info
	elektraCacheCutMeta (handle);

	if (elektraGlobalGet (handle, cache, cacheParent, PREGETCACHE, MAXONCE) != ELEKTRA_PLUGIN_STATUS_SUCCESS)
	{
		ELEKTRA_LOG_DEBUG ("CACHE MISS: could not fetch cache");
		elektraCacheCutMeta (handle);
		return;
	}

	keySetName (parentKey, keyName (initialParent));
	if (!(elektraStrCmp (keyName (initialParent), keyName (parentKey)) == 0))
	{
		ELEKTRA_LOG_WARNING ("parentKey name (%s) differs from initial (%s)", keyName (parentKey), keyName (initialParent));
	}
	ELEKTRA_ASSERT (elektraStrCmp (keyName (initialParent), keyName (parentKey)) == 0, "parentKey name (%s) differs from initial (%s)",
			keyName (parentKey), keyName (initialParent));
	if (elektraCacheCheckParent (handle->global, cacheParent, parentKey) != 0)
	{
		// parentKey in cache does not match, needs rebuild
		ELEKTRA_LOG_DEBUG ("CACHE WRONG PARENTKEY");
		elektraCacheCutMeta (handle);
		return;
	}
}

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
/**
 * @brief Deletes the OPMPHM.
 *
 * Clears and frees all memory in Opmphm.
 *
 * @param opmphm the OPMPHM
 */
static void cacheOpmphmDel (Opmphm * opmphm)
{
	ELEKTRA_NOT_NULL (opmphm);
	if (opmphm && opmphm->size && !test_bit (opmphm->flags, OPMPHM_FLAG_MMAP_GRAPH))
	{
		elektraFree (opmphm->graph);
	}
	if (opmphm->rUniPar && !test_bit (opmphm->flags, OPMPHM_FLAG_MMAP_HASHFUNCTIONSEEDS))
	{
		elektraFree (opmphm->hashFunctionSeeds);
	}
	if (!test_bit (opmphm->flags, OPMPHM_FLAG_MMAP_STRUCT)) elektraFree (opmphm);
}

/**
 * @brief Deletes the OpmphmPredictor.
 *
 * Clears and frees all memory in OpmphmPredictor.
 *
 * @param op the OpmphmPredictor
 */
static void cacheOpmphmPredictorDel (OpmphmPredictor * op)
{
	ELEKTRA_NOT_NULL (op);
	if (!test_bit (op->flags, OPMPHM_PREDICTOR_FLAG_MMAP_PATTERNTABLE)) elektraFree (op->patternTable);
	if (!test_bit (op->flags, OPMPHM_PREDICTOR_FLAG_MMAP_STRUCT)) elektraFree (op);
}
#endif

static int elektraCacheLoadSplit (KDB * handle, Split * split, KeySet * ks, KeySet ** cache, Key ** cacheParent, Key * parentKey,
				  Key * initialParent, int debugGlobalPositions)
{
	ELEKTRA_LOG_DEBUG ("CACHE parentKey: %s, %s", keyName (*cacheParent), keyString (*cacheParent));

	if (splitCacheCheckState (split, handle->global) == -1)
	{
		ELEKTRA_LOG_DEBUG ("FAIL, have to discard cache because split state / SIZE FAIL, or file mismatch");
		elektraCacheCutMeta (handle);
		return -1;
	}

	ELEKTRA_LOG_DEBUG ("CACHE HIT");
	if (splitCacheLoadState (split, handle->global) != 0) return -1;

	if (debugGlobalPositions)
	{
		keySetName (parentKey, keyName (initialParent));
		elektraGlobalGet (handle, *cache, parentKey, PREGETSTORAGE, INIT);
		elektraGlobalGet (handle, *cache, parentKey, PREGETSTORAGE, MAXONCE);
		elektraGlobalGet (handle, *cache, parentKey, PREGETSTORAGE, DEINIT);
	}

	keySetName (parentKey, keyName (initialParent));
	// TODO: there are no error checks here, see kdbGet
	elektraGlobalGet (handle, *cache, parentKey, PROCGETSTORAGE, INIT);
	elektraGlobalGet (handle, *cache, parentKey, PROCGETSTORAGE, MAXONCE);
	elektraGlobalGet (handle, *cache, parentKey, PROCGETSTORAGE, DEINIT);

	// replace ks with cached keyset
	ksRewind (*cache);
	if (ks->size == 0)
	{
		ELEKTRA_LOG_DEBUG ("replacing keyset with cached keyset");
#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
		if (ks->opmphm) cacheOpmphmDel (ks->opmphm);
		if (ks->opmphmPredictor) cacheOpmphmPredictorDel (ks->opmphmPredictor);
#endif
		ksClose (ks);
		ks->array = (*cache)->array;
		ks->size = (*cache)->size;
		ks->alloc = (*cache)->alloc;
		ks->flags = (*cache)->flags;
#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
		ks->opmphm = (*cache)->opmphm;
		ks->opmphmPredictor = (*cache)->opmphmPredictor;
#endif
		elektraFree (*cache);
		*cache = 0;
	}
	else
	{
		ELEKTRA_LOG_DEBUG ("appending cached keyset (ks was not empty)");
		ksAppend (ks, *cache);
		ksDel (*cache);
		*cache = 0;
	}
	keyDel (*cacheParent);
	*cacheParent = 0;

	if (debugGlobalPositions)
	{
		keySetName (parentKey, keyName (initialParent));
		elektraGlobalGet (handle, ks, parentKey, POSTGETSTORAGE, INIT);
		elektraGlobalGet (handle, ks, parentKey, POSTGETSTORAGE, MAXONCE);
		elektraGlobalGet (handle, ks, parentKey, POSTGETSTORAGE, DEINIT);
	}

	return 0;
}
#endif

static const char * phaseName (ElektraKdbPhase phase)
{
	switch (phase)
	{
	case ELEKTRA_KDB_GET_PHASE_RESOLVER: // ELEKTRA_KDB_SET_PHASE_RESOLVER
		return "RESOLVER";
	case ELEKTRA_KDB_GET_PHASE_CACHECHECK:
		return "CACHECHECK";
	case ELEKTRA_KDB_GET_PHASE_PRE_STORAGE: // ELEKTRA_KDB_SET_PHASE_PRE_STORAGE
		return "PRE_STORAGE";
	case ELEKTRA_KDB_GET_PHASE_STORAGE: // ELEKTRA_KDB_SET_PHASE_STORAGE
		return "STORAGE";
	case ELEKTRA_KDB_GET_PHASE_POST_STORAGE: // ELEKTRA_KDB_SET_PHASE_POST_STORAGE
		return "POST_STORAGE";
	case ELEKTRA_KDB_SET_PHASE_PRE_COMMIT:
		return "PRE_COMMIT";
	case ELEKTRA_KDB_SET_PHASE_COMMIT:
		return "COMMIT";
	case ELEKTRA_KDB_SET_PHASE_POST_COMMIT:
		return "POST_COMMIT";
	case ELEKTRA_KDB_SET_PHASE_PRE_ROLLBACK:
		return "PRE_ROLLBACK";
	case ELEKTRA_KDB_SET_PHASE_ROLLBACK:
		return "ROLLBACK";
	case ELEKTRA_KDB_SET_PHASE_POST_ROLLBACK:
		return "POST_ROLLBACK";
	default:
		ELEKTRA_LOG_DEBUG ("Unknown phase converted to string: %02x", phase);
		return "???";
	}
}

static void setBackendPhase (BackendData * backendData, ElektraKdbPhase phase)
{
	ksAppendKey (backendData->backend->global, keyNew ("system:/elektra/kdb/backend/phase", KEY_BINARY, KEY_SIZE,
							   sizeof (ElektraKdbPhase), KEY_VALUE, &phase, KEY_END));
}

static bool initBackends (KeySet * backends, Key * parentKey)
{
	bool success = true;
	for (elektraCursor i = 0; i < ksGetSize (backends); i++)
	{
		// TODO [new_backend]: lazy open plugins here instead of opening all mountpoints in kdbOpen

		Key * backendKey = ksAtCursor (backends, i);
		keySetMeta (backendKey, "meta:/internal/kdbreadonly", NULL);

		BackendData * backendData = (BackendData *) keyValue (backendKey);

		if (backendData->initialized)
		{
			// already initialized
			continue;
		}

		kdbInitPtr initFn = backendData->backend->kdbInit;
		if (initFn == NULL)
		{
			ELEKTRA_ADD_INTERFACE_WARNINGF (
				parentKey, "The mountpoint '%s' defined a plugin ('%s') without a kdbInit function as a backend.",
				keyName (backendKey), backendData->backend->name);
			success = false;
			continue;
		}

		// set up parentKey and global keyset
		keySetName (parentKey, KDB_SYSTEM_ELEKTRA "/mountpoints");
		keyAddBaseName (parentKey, keyName (backendKey));
		ksAppendKey (backendData->backend->global,
			     keyNew ("system:/elektra/kdb/backend/plugins", KEY_BINARY, KEY_SIZE, sizeof (backendData->plugins), KEY_VALUE,
				     &backendData->plugins, KEY_END));
		parentKey->hasReadOnlyName = true;

		int ret = initFn (backendData->backend, backendData->definition, parentKey);

		// restore parentKey
		parentKey->hasReadOnlyName = false;

		// check return code
		switch (ret)
		{
		case ELEKTRA_PLUGIN_STATUS_SUCCESS:
			// successfully initialized
			backendData->initialized = true;
			break;
		case ELEKTRA_PLUGIN_STATUS_NO_UPDATE:
			// successfully initialized as read-only
			backendData->initialized = true;
			keySetMeta (backendKey, "meta:/internal/kdbreadonly", "1");
			break;
		case ELEKTRA_PLUGIN_STATUS_ERROR:
			// handle error
			ELEKTRA_ADD_INTERFACE_WARNINGF (
				parentKey, "Calling the kdbInit function for the backend plugin ('%s') of the mountpoint '%s' has failed.",
				backendData->backend->name, keyName (backendKey));
			success = false;
			continue;
		default:
			// unknown result -> treat as error
			ELEKTRA_ADD_INTERFACE_WARNINGF (
				parentKey,
				"The kdbInit function for the backend plugin ('%s') of the mountpoint '%s' returned "
				"an unknown result code '%d'. Treating the call as failed.",
				backendData->backend->name, keyName (backendKey), ret);
			success = false;
			continue;
		}
	}

	if (!success)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "The init phase of kdbGet() has failed. See warnings for details.");
	}

	return success;
}

static bool resolveBackendsForGet (KeySet * backends, Key * parentKey)
{
	bool success = true;
	for (elektraCursor i = 0; i < ksGetSize (backends); i++)
	{
		Key * backendKey = ksAtCursor (backends, i);
		keySetMeta (backendKey, "meta:/internal/kdbmountpoint", NULL);
		keySetMeta (backendKey, "meta:/internal/kdbneedsupdate", NULL);

		if (keyGetNamespace (backendKey) == KEY_NS_PROC)
		{
			// proc:/ backends only run in poststorage
			// TODO [new_backend]: allow proc:/ backends for more than poststorage
			keySetMeta (backendKey, "meta:/internal/kdbneedsupdate", "1");
			continue;
		}

		BackendData * backendData = (BackendData *) keyValue (backendKey);

		// check if get function exists
		kdbGetPtr getFn = backendData->backend->kdbGet;
		if (getFn == NULL)
		{
			ELEKTRA_ADD_INTERFACE_WARNINGF (
				parentKey, "The mountpoint '%s' defined a plugin ('%s') without a kdbGet function as a backend.",
				keyName (backendKey), backendData->backend->name);
			success = false;
			continue;
		}

		// set up parentKey and global keyset for plugin
		keyCopy (parentKey, backendKey, KEY_CP_NAME);
		keySetString (parentKey, "");
		setBackendPhase (backendData, ELEKTRA_KDB_GET_PHASE_RESOLVER);
		ksAppendKey (backendData->backend->global,
			     keyNew ("system:/elektra/kdb/backend/plugins", KEY_BINARY, KEY_SIZE, sizeof (backendData->plugins), KEY_VALUE,
				     &backendData->plugins, KEY_END));
		parentKey->hasReadOnlyName = true;

		int ret = getFn (backendData->backend, backendData->keys, parentKey);

		// restore parentKey
		parentKey->hasReadOnlyName = false;

		// check return code
		switch (ret)
		{
		case ELEKTRA_PLUGIN_STATUS_SUCCESS:
			// Store returned mountpoint ID and mark for update
			keySetMeta (backendKey, "meta:/internal/kdbmountpoint", keyString (parentKey));
			keySetMeta (backendKey, "meta:/internal/kdbneedsupdate", "1");
			break;
		case ELEKTRA_PLUGIN_STATUS_NO_UPDATE:
			// no update needed
			keySetMeta (backendKey, "meta:/internal/kdbmountpoint", keyString (parentKey));
			break;
		case ELEKTRA_PLUGIN_STATUS_ERROR:
			// handle error
			ELEKTRA_ADD_INTERFACE_WARNINGF (parentKey,
							"Calling the kdbGet function for the backend plugin ('%s') of the mountpoint '%s' "
							"has failed during the %s phase.",
							backendData->backend->name, keyName (backendKey),
							phaseName (ELEKTRA_KDB_GET_PHASE_RESOLVER));
			success = false;
			continue;
		default:
			// unknown result -> treat as error
			ELEKTRA_ADD_INTERFACE_WARNINGF (parentKey,
							"The kdbGet function for the backend plugin ('%s') of the mountpoint '%s' returned "
							"an unknown result code '%d' during the %s phase. Treating the call as failed.",
							backendData->backend->name, keyName (backendKey), ret,
							phaseName (ELEKTRA_KDB_GET_PHASE_RESOLVER));
			success = false;
			continue;
		}
	}

	if (!success)
	{
		ELEKTRA_SET_INTERFACE_ERRORF (parentKey, "The %s phase of kdbGet() has failed. See warnings for details.",
					      phaseName (ELEKTRA_KDB_GET_PHASE_RESOLVER));
	}

	return success;
}

static const uint16_t ELEKTRA_KDB_GET_PHASE_POST_STORAGE_SPEC = 1 << 8 | ELEKTRA_KDB_GET_PHASE_POST_STORAGE;
static const uint16_t ELETKRA_KDB_GET_PHASE_POST_STORAGE_NONSPEC = ELEKTRA_KDB_GET_PHASE_POST_STORAGE;

static bool runGetPhase (KeySet * backends, Key * parentKey, uint16_t phase)
{
	bool speconly = false;
	bool skipspec = false;
	if ((phase & 0xFF) == ELEKTRA_KDB_GET_PHASE_POST_STORAGE)
	{
		speconly = phase == ELEKTRA_KDB_GET_PHASE_POST_STORAGE_SPEC;
		skipspec = !speconly;
		phase = ELEKTRA_KDB_GET_PHASE_POST_STORAGE;
	}

	bool success = true;
	for (elektraCursor i = 0; i < ksGetSize (backends); i++)
	{
		Key * backendKey = ksAtCursor (backends, i);
		BackendData * backendData = (BackendData *) keyValue (backendKey);

		if (speconly && keyGetNamespace (backendKey) != KEY_NS_SPEC)
		{
			continue;
		}

		if (skipspec && keyGetNamespace (backendKey) == KEY_NS_SPEC)
		{
			continue;
		}

		if (keyGetNamespace (backendKey) == KEY_NS_PROC && phase != ELEKTRA_KDB_GET_PHASE_POST_STORAGE)
		{
			// proc:/ backends only run in poststorage phase
			// TODO [new_backend]: allow proc:/ backends for more than poststorage
			continue;
		}

		// check if get function exists
		kdbGetPtr getFn = backendData->backend->kdbGet;
		if (getFn == NULL)
		{
			ELEKTRA_ADD_INTERFACE_WARNINGF (
				parentKey, "The mountpoint '%s' defined a plugin ('%s') without a kdbGet function as a backend.",
				keyName (backendKey), backendData->backend->name);
			success = false;
			continue;
		}

		// set up parentKey and global keyset for plugin
		keyCopy (parentKey, backendKey, KEY_CP_NAME);
		keyCopy (parentKey, keyGetMeta (backendKey, "meta:/internal/kdbmountpoint"), KEY_CP_STRING);
		setBackendPhase (backendData, phase);
		ksAppendKey (backendData->backend->global,
			     keyNew ("system:/elektra/kdb/backend/plugins", KEY_BINARY, KEY_SIZE, sizeof (backendData->plugins), KEY_VALUE,
				     &backendData->plugins, KEY_END));
		// TODO [new_backend]: should lock value, but fcrypt needs to change the parentKey value after the resolver has run
		// set_bit (parentKey->flags, KEY_FLAG_RO_NAME | KEY_FLAG_RO_VALUE);

		// START fcrypt workaround
		parentKey->hasReadOnlyValue = false;
		parentKey->hasReadOnlyName = true;
		// END fcrypt workaround

		int ret = getFn (backendData->backend, backendData->keys, parentKey);

		// restore parentKey
		parentKey->hasReadOnlyName = false;
		parentKey->hasReadOnlyValue = false;

		// check return code
		switch (ret)
		{
		case ELEKTRA_PLUGIN_STATUS_SUCCESS:
		case ELEKTRA_PLUGIN_STATUS_NO_UPDATE:
			// success

			// START fcrypt workaround
			keySetMeta (backendKey, "meta:/internal/kdbmountpoint", keyString (parentKey));
			// END fcrypt workaround
			break;
		case ELEKTRA_PLUGIN_STATUS_ERROR:
			// handle error
			ELEKTRA_ADD_INTERFACE_WARNINGF (parentKey,
							"Calling the kdbGet function for the backend plugin ('%s') of the mountpoint '%s' "
							"has failed during the %s phase.",
							backendData->backend->name, keyName (backendKey), phaseName (phase));
			success = false;
			continue;
		default:
			// unknown result -> treat as error
			ELEKTRA_ADD_INTERFACE_WARNINGF (parentKey,
							"The kdbGet function for the backend plugin ('%s') of the mountpoint '%s' returned "
							"an unknown result code '%d' during the %s phase. Treating the call as failed.",
							backendData->backend->name, keyName (backendKey), ret, phaseName (phase));
			success = false;
			continue;
		}
	}

	if (!success)
	{
		ELEKTRA_SET_INTERFACE_ERRORF (parentKey, "The %s phase of kdbGet() has failed. See warnings for details.",
					      phaseName (phase));
	}

	return success;
}


/**
 * Retrieve Keys from the Key database in an atomic and universal way.
 *
 * @pre The @p handle must be a valid KDB handle as returned from kdbOpen().
 * @pre The KeySet @p returned must be a valid KeySet, i.e., constructed with ksNew().
 * @pre The KeySet @p returned must contain keys only from the `spec:/`, `dir:/`, `user:/`, `system:/`, `default:/` or `proc:/` namespaces.
 * @pre The Key @p parentKey must be a valid Key, i.e., constructed with keyNew().
 * @pre The Key @p parentKey must not have read-only name, value or metadata.
 * @pre The Key @p parentKey must use the `spec:/`, `dir:/`, `user:/`, `system:/`,
 * 	`default:/`, `proc:/` or cascading namespace.
 *
 * If you pass `NULL` or a key with read-only metadata as @p parentKey,
 * kdbGet() will fail immediately without doing anything.
 * If you pass another invalid @p parentKey, or `NULL` as @p ks or @p handle,
 * kdbGet() will set an error on @p parentKey and then return immediately.
 *
 * @note If you pass a non-NULL @p parentKey with writable metadata, kdbGet() will **always** remove
 *       any existing errors and warnings from @p parentKey.
 *
 * @warning If you later call kdbSet() with the same @p handle you must make sure to
 *          pass all keys from @p ks, which you do not want to remove.
 *
 * @par Loadable Namespaces
 *
 * Not all namespace can be loaded.
 *
 * - `spec:/`, `dir:/`, `user:/` and `system:/` can be loaded via kdbGet().
 * - `proc:/` keys can be loaded via kdbGet(), but are not persisted or cached.
 * - `default:/` keys can be inserted by kdbGet() but they will always stem from a specification in `spec:/` keys.
 * - If @p ks contains a key with any other namespace, an error will be returned.
 *
 * @par Parent Key
 *
 * The @p parentKey defines which parts of @p ks will be loaded.
 * Everything that is at or below @p parentKey wil be loaded together with any key
 * that shares a backend with such a key. Backends are always loaded as an atomic unit.
 *
 * @note If @p parentKey is in the cascading namespace, keys of all loadable namespaces (see above) will be loaded.
 *       This is generally the recommended approach.
 *
 * Upon sucessfully returning kdbGet() also sets the value of @p parentKey to the storage identifier used
 * by the backend that contains (or would contain) @p parentKey. For file-based backends this is the absolute
 * path of the underlying file. Other backends may use different identifiers, but it always uniquely identifies
 * the underlying storage unit.
 *
 * @note If @p parentKey is in the cascading, `default:/` or `proc:/ namespace, the value of @p parentKey
 *       will be set to an empty string. This is done, because those namespaces are not persistable (see kdbSet())
 *       and therfore have no storage identifier.
 *
 * @par KeySet Modifications
 *
 * Below or at @p parentKey, the KeySet @p ks will mostly contain keys loaded from backends.
 * The only exception are `proc:/` and `spec:/` keys that were already present, before kdbGet()
 * was called and do not overlap with an existing backend (for those namespaces).
 * This can be used to provide a hard-coded fallback specifications and/or process-specific data.
 *
 * Keys not below (or at) @p parentKey that were present when kdbGet() was called, may still be removed.
 * For example, this could be because they overlap with a backend that also has keys below @p parentKey
 * (backends are atomic units).
 *
 * @par Example:
 * This example demonstrates the typical usecase within an application
 * (without error handling).
 *
 * @include kdbget.c
 *
 * When a backend fails kdbGet() will return -1 with all
 * error and warning information in the @p parentKey.
 * The parameter @p returned will not be changed.
 *
 * @par Optimization:
 * In the first run of kdbGet all requested (or more) Keys are retrieved. On subsequent
 * calls only the Keys are retrieved where something was changed
 * inside the Key database. The other Keys stay in the
 * KeySet returned as passed.
 *
 * It is your responsibility to save the original KeySet if you
 * need it afterwards.
 *
 * If you want to be sure to get a fresh KeySet again, you need to open a
 * second handle to the Key database using kdbOpen().
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param parentKey Keys below @p parentKey will be retrieved from @p handle.
 * It is also used to add warnings and set error information.
 * @param ks the (pre-initialized) KeySet returned with all keys found
 * 	will not be changed on error or if no update is required
 *
 * @retval 2 if only `proc:/` backends were executed. This means no data was loaded from storage.
 * There might be warnings attached to the parentKey! Depending on your use case, you might need to treat them as erorrs!
 * @retval 1 if the Keys were retrieved successfully. There might be warnings attached to the parentKey! Depending on your use case, you
 * might need to treat them as errors!
 * @retval 0 if there was no update at all - no changes are made to the KeySet then. There might be warnings attached to the parentKey!
 * Depending on your use case, you might need to treat them as erorrs!
 * @retval -1 on failure - no changes are made to the KeySet then
 *
 * @since 1.0.0
 * @ingroup kdb
 * @see ksLookup(), ksLookupByName() for powerful lookups after the KeySet was
 * retrieved
 * @see kdbOpen() which needs to be called before
 * @see kdbSet() to save the configuration afterwards
 * @see kdbClose() to finish affairs with the key database.
 */
int kdbGet (KDB * handle, KeySet * ks, Key * parentKey)
{
	// TODO [new_backend]: handle all namespaces according to kdb-operations.md
	//  including checking for non-loadable namespace (see above) keys in ks
	//  including (not) removing keys from ks as described above
	//  including checking the namespace of parentKey

	// Step 0: check preconditions
	if (parentKey == NULL)
	{
		ELEKTRA_LOG_WARNING ("parentKey == NULL");
		return -1;
	}

	if (parentKey->hasReadOnlyMeta)
	{
		ELEKTRA_LOG_WARNING ("parentKey KEY_FLAG_RO_META");
		return -1;
	}

	clearErrorAndWarnings (parentKey);

	if (parentKey->hasReadOnlyName)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "parentKey with read-only name passed");
		ELEKTRA_LOG ("parentKey KEY_FLAG_RO_NAME");
		return -1;
	}

	if (parentKey->hasReadOnlyValue)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "parentKey with read-only value passed");
		ELEKTRA_LOG ("parentKey KEY_FLAG_RO_VALUE");
		return -1;
	}

	if (keyGetNamespace (parentKey) == KEY_NS_META)
	{
		ELEKTRA_SET_INTERFACE_ERRORF (parentKey, "parentKey with meta:/ name passed ('%s')", keyName (parentKey));
		return -1;
	}

	if (handle == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "NULL pointer passed for handle");
		return -1;
	}

	if (ks == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "NULL pointer passed for KeySet");
		return -1;
	}

	int errnosave = errno;
	Key * initialParent = keyDup (parentKey, KEY_CP_ALL);

	ELEKTRA_LOG ("now in new kdbGet (%s)", keyName (parentKey));

	// Step 1: find backends for parentKey
	KeySet * backends = backendsForParentKey (handle->backends, parentKey);

	bool goptsActive = handle->hooks.gopts.plugin != NULL;
	if (goptsActive)
	{
		// HACK: for gopts; generates keys outside backend
		// TODO [new_backend]: this captures the keys generated by gopts, but we also need to handle keys generated elsewhere
		// (should cause a warning)
		ksAppendKey (backends, ksLookupByName (handle->backends, "proc:/", 0));
	}
	KeySet * allBackends = ksDup (backends);

	// Step 2: run open operation where needed (happens within step 3)
	// Step 3: run init phase where needed
	if (!initBackends (backends, parentKey))
	{
		keyCopy (parentKey, initialParent, KEY_CP_NAME | KEY_CP_VALUE);
		keyDel (initialParent);

		errno = errnosave;
		return -1;
	}

	parentKey->hasReadOnlyName = false;
	parentKey->hasReadOnlyValue = false;

	// Step 4: run resolver phase
	if (!resolveBackendsForGet (backends, parentKey))
	{
		goto error;
	}

	// Step 5: remove up-to-date backends
	for (elektraCursor i = 0; i < ksGetSize (backends); i++)
	{
		if (keyGetMeta (ksAtCursor (backends, i), "meta:/internal/kdbneedsupdate") == NULL)
		{
			elektraKsPopAtCursor (backends, i);
			--i;
		}
	}

	bool procOnly = keyGetNamespace (ksAtCursor (backends, 0)) == KEY_NS_PROC &&
			keyGetNamespace (ksAtCursor (backends, ksGetSize (backends) - 1)) == KEY_NS_PROC;

	// Step 6: return if no backends left
	// HACK: for gopts
	if (ksGetSize (backends) == 0 || (goptsActive && procOnly && ksGetSize (backends) == 1))
	{
		keyCopy (parentKey, initialParent, KEY_CP_NAME | KEY_CP_VALUE);
		keyDel (initialParent);

		keyCopy (parentKey, keyGetMeta (backendsFindParent (allBackends, parentKey), "meta:/internal/kdbmountpoint"),
			 KEY_CP_STRING);

		ksDel (backends);
		ksDel (allBackends);
		errno = errnosave;
		return 2;
	}

	// check if cache is enabled, Steps 7-9 only run with cache
	// TODO [new_backend]: implement cache
	bool cacheEnabled = false;
	if (cacheEnabled)
	{
		// Step 7: get cache entry IDs
		// TODO [new_backend]: implement cache

		// Step 8: run cachecheck phase
		// TODO [new_backend]: implement cache

		// Step 9: retrieve cache data
		// TODO [new_backend]: implement cache
	}

	// Step 10a: run prestorage phase
	if (!runGetPhase (backends, parentKey, ELEKTRA_KDB_GET_PHASE_PRE_STORAGE))
	{
		goto error;
	}

	// Step 10b: discard data that plugins may have produced
	for (elektraCursor i = 0; i < ksGetSize (backends); i++)
	{
		const BackendData * backendData = keyValue (ksAtCursor (backends, i));
		ksClear (backendData->keys);
	}

	// Step 10c: run storage phase
	if (!runGetPhase (backends, parentKey, ELEKTRA_KDB_GET_PHASE_STORAGE))
	{
		goto error;
	}

	// Step 11: run poststorage phase for spec:/
	Key * specRoot = keyNew ("spec:/", KEY_END);
	if (!runGetPhase (backends, parentKey, ELEKTRA_KDB_GET_PHASE_POST_STORAGE_SPEC))
	{
		keyDel (specRoot);
		goto error;
	}
	keyDel (specRoot);

	// Step 12: merge data from all backends
	KeySet * dataKs = ksNew (ksGetSize (ks), KS_END);
	backendsMerge (backends, dataKs);

	SendNotificationHook * sendNotificationHook = handle->hooks.sendNotification;
	while (sendNotificationHook != NULL)
	{
		if (sendNotificationHook->get != NULL)
		{
			sendNotificationHook->get (sendNotificationHook->plugin, dataKs, parentKey);
		}

		sendNotificationHook = sendNotificationHook->next;
	}

	// Step 13: run gopts (if enabled)
	keyCopy (parentKey, initialParent, KEY_CP_NAME);
	keySetNamespace (parentKey, KEY_NS_CASCADING);

	parentKey->hasReadOnlyName = true;
	parentKey->hasReadOnlyValue = true;
	if (goptsActive && !handle->hooks.gopts.get (handle->hooks.gopts.plugin, dataKs, parentKey))
	{
		parentKey->hasReadOnlyName = false;
		parentKey->hasReadOnlyValue = false;
		ksDel (dataKs);
		goto error;
	}
	parentKey->hasReadOnlyName = false;
	parentKey->hasReadOnlyValue = false;

	keySetNamespace (parentKey, keyGetNamespace (initialParent));

	// Step 14: run spec plugin
	parentKey->hasReadOnlyName = true;
	parentKey->hasReadOnlyValue = true;
	if (handle->hooks.spec.plugin && handle->hooks.spec.copy (handle->hooks.spec.plugin, dataKs, parentKey, true) == -1)
	{
		parentKey->hasReadOnlyName = false;
		parentKey->hasReadOnlyValue = false;
		ksDel (dataKs);
		goto error;
	}
	parentKey->hasReadOnlyName = false;
	parentKey->hasReadOnlyValue = false;

	// TODO (atmaxinger): should we have a default:/ backend?
	Key * defaultCutpoint = keyNew ("default:/", KEY_END);
	KeySet * defaults = ksCut (dataKs, defaultCutpoint);

	// Step 15: split dataKs for poststorage phase
	if (!backendsDivide (backends, dataKs))
	{
		ELEKTRA_SET_INTERNAL_ERROR (parentKey,
					    "Couldn't divide keys into mountpoints before poststorage. Please report this bug at "
					    "https://issues.libelektra.org.");
		keyDel (defaultCutpoint);
		ksDel (dataKs);
		ksDel (defaults);
		goto error;
	}

	// Step 16: run poststorage phase for non-spec:/
	if (!runGetPhase (backends, parentKey, ELETKRA_KDB_GET_PHASE_POST_STORAGE_NONSPEC))
	{
		keyDel (defaultCutpoint);
		ksDel (dataKs);
		ksDel (defaults);
		goto error;
	}

	// Step 17: remove the parts of ks we read from backends
	// Note: we need to do this, so that in a second kdbGet() keys
	//       removed from the backend are removed from ks as well
	for (elektraCursor i = 0; i < ksGetSize (backends); i++)
	{
		ksDel (ksCut (ks, ksAtCursor (backends, i)));
	}

	// Step 18: merge data into ks and return
	ksClear (dataKs);
	backendsMerge (backends, dataKs);
	ksAppend (ks, dataKs);

	// TODO (atmaxinger): should we have a default:/ backend?
	ksAppend (ks, defaults);
	ksAppend (dataKs, defaults);
	ksDel (defaults);
	keyDel (defaultCutpoint);

	// Step 19: update cache
	// TODO [new_backend]: implement cache

	keyCopy (parentKey, initialParent, KEY_CP_NAME | KEY_CP_VALUE);
	keyDel (initialParent);

	elektraNamespace parentNs = keyGetNamespace (parentKey);
	if (parentNs == KEY_NS_CASCADING || parentNs == KEY_NS_PROC || parentNs == KEY_NS_DEFAULT)
	{
		keySetString (parentKey, "");
	}
	else
	{
		keyCopy (parentKey, keyGetMeta (backendsFindParent (allBackends, parentKey), "meta:/internal/kdbmountpoint"),
			 KEY_CP_STRING);
	}

	ksDel (backends);
	ksDel (allBackends);

	if (handle->allKeys != NULL)
	{
		ksDel (ksCut (handle->allKeys, parentKey));
		ksAppendDup (handle->allKeys, dataKs);
	}
	ksDel (dataKs);

	errno = errnosave;
	return procOnly ? 2 : 1;

error:
	ELEKTRA_LOG_DEBUG ("now in error state");

	keyCopy (parentKey, initialParent, KEY_CP_NAME);
	keyDel (initialParent);

	keyCopy (parentKey, keyGetMeta (backendsFindParent (allBackends, parentKey), "meta:/internal/kdbmountpoint"), KEY_CP_STRING);

	ksDel (backends);
	ksDel (allBackends);

	errno = errnosave;
	return -1;
}

static bool resolveBackendsForSet (KeySet * backends, Key * parentKey)
{
	bool success = true;
	for (elektraCursor i = 0; i < ksGetSize (backends); i++)
	{
		Key * backendKey = ksAtCursor (backends, i);
		keySetMeta (backendKey, "meta:/internal/kdbmountpoint", NULL);

		BackendData * backendData = (BackendData *) keyValue (backendKey);

		// check if set function exists
		kdbSetPtr setFn = backendData->backend->kdbSet;
		if (setFn == NULL)
		{
			ELEKTRA_ADD_INTERFACE_WARNINGF (
				parentKey, "The mountpoint '%s' defined a plugin ('%s') without a kdbSet function as a backend.",
				keyName (backendKey), backendData->backend->name);
			success = false;
			continue;
		}

		// set up parentKey and global keyset for plugin
		keyCopy (parentKey, backendKey, KEY_CP_NAME);
		keySetString (parentKey, "");
		setBackendPhase (backendData, ELEKTRA_KDB_SET_PHASE_RESOLVER);
		ksAppendKey (backendData->backend->global,
			     keyNew ("system:/elektra/kdb/backend/plugins", KEY_BINARY, KEY_SIZE, sizeof (backendData->plugins), KEY_VALUE,
				     &backendData->plugins, KEY_END));
		parentKey->hasReadOnlyName = true;

		int ret = setFn (backendData->backend, backendData->keys, parentKey);

		// restore parentKey
		parentKey->hasReadOnlyName = false;

		// check return code
		switch (ret)
		{
		case ELEKTRA_PLUGIN_STATUS_NO_UPDATE:
			ELEKTRA_ADD_INTERFACE_WARNINGF (
				parentKey,
				"Calling the kdbSet function for the backend plugin ('%s') of the mountpoint '%s' returned "
				"ELEKTRA_PLUGIN_STATUS_NO_UPDATE in the '%s' phase. This is interpreted the same way as "
				"ELEKTRA_PLUGIN_STATUS_SUCCESS, i.e. the mountpoint will still go through the rest of kdbSet()'s phases.",
				backendData->backend->name, keyName (backendKey), phaseName (ELEKTRA_KDB_SET_PHASE_RESOLVER));
			// FALLTHROUGH
		case ELEKTRA_PLUGIN_STATUS_SUCCESS:
			// Store returned mountpoint ID and mark for update
			keySetMeta (backendKey, "meta:/internal/kdbmountpoint", keyString (parentKey));
			break;
		case ELEKTRA_PLUGIN_STATUS_ERROR:
			// handle error
			ELEKTRA_ADD_INTERFACE_WARNINGF (parentKey,
							"Calling the kdbSet function for the backend plugin ('%s') of the mountpoint '%s' "
							"has failed during the %s phase.",
							backendData->backend->name, keyName (backendKey),
							phaseName (ELEKTRA_KDB_SET_PHASE_RESOLVER));
			success = false;
			continue;
		default:
			// unknown result -> treat as error
			ELEKTRA_ADD_INTERFACE_WARNINGF (parentKey,
							"The kdbSet function for the backend plugin ('%s') of the mountpoint '%s' returned "
							"an unknown result code '%d' during the %s phase. Treating the call as failed.",
							backendData->backend->name, keyName (backendKey), ret,
							phaseName (ELEKTRA_KDB_SET_PHASE_RESOLVER));
			success = false;
			continue;
		}
	}

	if (!success)
	{
		ELEKTRA_SET_INTERFACE_ERRORF (parentKey, "The %s phase of kdbSet() has failed. See warnings for details.",
					      phaseName (ELEKTRA_KDB_SET_PHASE_RESOLVER));
	}

	return success;
}

enum KdbSetFn
{
	KDB_SET_FN_SET,
	KDB_SET_FN_COMMIT,
	KDB_SET_FN_ERROR,
};

static bool runSetPhase (KeySet * backends, Key * parentKey, ElektraKdbPhase phase, bool blockErrors, enum KdbSetFn function)
{
	bool existingError = keyGetMeta (parentKey, "error") != NULL;

	if (blockErrors && !existingError)
	{
		// set a dummy value to block errors
		// any errors that occur will be converted into warnings
		keySetMeta (parentKey, "error", "blocked");
	}

	bool success = true;
	for (elektraCursor i = 0; i < ksGetSize (backends); i++)
	{
		Key * backendKey = ksAtCursor (backends, i);
		BackendData * backendData = (BackendData *) keyValue (backendKey);

		// check if function exists
		if (function == KDB_SET_FN_SET && backendData->backend->kdbSet == NULL)
		{
			ELEKTRA_ADD_INTERFACE_WARNINGF (parentKey,
							"The mountpoint '%s' defined a plugin ('%s') without a kdbSet function as a "
							"backend and the plugin didn't initialize the mountpoint as read-only.",
							keyName (backendKey), backendData->backend->name);
			success = false;
			continue;
		}
		if (function == KDB_SET_FN_COMMIT && backendData->backend->kdbCommit == NULL)
		{
			ELEKTRA_ADD_INTERFACE_WARNINGF (parentKey,
							"The mountpoint '%s' defined a plugin ('%s') without a kdbCommit function as a "
							"backend and the plugin didn't initialize the mountpoint as read-only.",
							keyName (backendKey), backendData->backend->name);
			success = false;
			continue;
		}
		if (function == KDB_SET_FN_ERROR && backendData->backend->kdbError == NULL)
		{
			ELEKTRA_ADD_INTERFACE_WARNINGF (parentKey,
							"The mountpoint '%s' defined a plugin ('%s') without a kdbError function as a "
							"backend and the plugin didn't initialize the mountpoint as read-only.",
							keyName (backendKey), backendData->backend->name);
			success = false;
			continue;
		}

		// set up parentKey and global keyset for plugin
		keyCopy (parentKey, backendKey, KEY_CP_NAME);
		keyCopy (parentKey, keyGetMeta (backendKey, "meta:/internal/kdbmountpoint"), KEY_CP_STRING);
		setBackendPhase (backendData, phase);
		ksAppendKey (backendData->backend->global,
			     keyNew ("system:/elektra/kdb/backend/plugins", KEY_BINARY, KEY_SIZE, sizeof (backendData->plugins), KEY_VALUE,
				     &backendData->plugins, KEY_END));
		parentKey->hasReadOnlyName = true;
		parentKey->hasReadOnlyValue = true;

		int ret;
		switch (function)
		{
		case KDB_SET_FN_SET:
			ret = backendData->backend->kdbSet (backendData->backend, backendData->keys, parentKey);
			break;
		case KDB_SET_FN_COMMIT:
			ret = backendData->backend->kdbCommit (backendData->backend, backendData->keys, parentKey);
			break;
		case KDB_SET_FN_ERROR:
			ret = backendData->backend->kdbError (backendData->backend, backendData->keys, parentKey);
			break;
		}

		// restore parentKey
		parentKey->hasReadOnlyName = false;
		parentKey->hasReadOnlyValue = false;

		// check return code
		switch (ret)
		{
		case ELEKTRA_PLUGIN_STATUS_SUCCESS:
		case ELEKTRA_PLUGIN_STATUS_NO_UPDATE:
			// success
			break;
		case ELEKTRA_PLUGIN_STATUS_ERROR:
			// handle error
			ELEKTRA_ADD_INTERFACE_WARNINGF (parentKey,
							"Calling the kdbSet function for the backend plugin ('%s') of the mountpoint '%s' "
							"has failed during the %s phase.",
							backendData->backend->name, keyName (backendKey), phaseName (phase));
			success = false;
			continue;
		default:
			// unknown result -> treat as error
			ELEKTRA_ADD_INTERFACE_WARNINGF (parentKey,
							"The kdbSet function for the backend plugin ('%s') of the mountpoint '%s' returned "
							"an unknown result code '%d' during the %s phase. Treating the call as failed.",
							backendData->backend->name, keyName (backendKey), ret, phaseName (phase));
			success = false;
			continue;
		}
	}

	if (!success)
	{
		ELEKTRA_SET_INTERFACE_ERRORF (parentKey, "The %s phase of kdbSet() has failed. See warnings for details.",
					      phaseName (phase));
	}

	if (blockErrors)
	{
		if (!existingError)
		{
			// remove dummy error again
			keySetMeta (parentKey, "error", NULL);
		}

		if (!success)
		{
			ELEKTRA_ADD_INTERFACE_WARNINGF (parentKey,
							"Errors in %s are ignored. The error that occurred was converted into a warning.",
							phaseName (phase));
		}
	}

	return success;
}

/**
 * Set Keys to the Key database in an atomic and universal way.
 *
 * @pre kdbGet() must be called before kdbSet():
 *   	 - initially (after kdbOpen())
 *   	 - after conflict errors in kdbSet().
 * @pre The KeySet @p returned must be a valid KeySet, i.e., constructed with ksNew().
 * @pre The KeySet @p returend must only contain only keys in the `spec:/`,
 * 	`dir:/`, `user:/`, `system:/`, `default:/` or `proc:/` namespaces.
 * @pre The Key @p parentKey must be a valid Key, e.g. constructed with keyNew().
 * @pre The Key @p parentKey must not have read-only name, value or metadata.
 * @pre The Key @p parentKey must use the `spec:/`, `dir:/`, `user:/`, `system:/`,
 * 	`default:/`, `proc:/` or cascading namespace.
 *
 * If you pass `NULL` or a key with read-only metadata as @p parentKey,
 * kdbSet() will fail immediately without doing anything.
 * If you pass another invalid @p parentKey, or `NULL` as @p ks or @p handle,
 * kdbSet() will set an error on @p parentKey and then return immediately.
 *
 * @note If you pass a non-NULL @p parentKey with writable metadata, kdbSet() will **always** remove
 *       any existing errors and warnings from @p parentKey.
 *
 * @par Persistable Namespaces
 *
 * Not all namespace can be persisted.
 *
 * - `spec:/`, `dir:/`, `user:/` and `system:/` will be persisted by kdbSet().
 * - `default:/` and `proc:/` keys are ignored by kdbSet().
 * - If @p ks contains a key with any other namespace, an error will be returned.
 *
 * In general it is recommended to use a @p parentKey in the cascading namespace to cover all namespaces at once.
 *
 * @par Parent Key
 *
 * The @p parentKey defines which parts of @p ks will be stored.
 * Everything that is at or below @p parentKey wil be persisted together with any key
 * that shares a backend with such a key. Backends are always stored as an atomic unit.
 *
 * @note If @p parentKey is in the cascading namespace, keys of all persistable
 *       namspaces (see above) will be stored. This is generally the recommended approach.
 *
 * @par KeySet modifications
 *
 * The contents of @p ks will mostly not be modified by kdbSet().
 * The only modifications made are those caused by applying the specification in
 * `spec:/` to `dir:/`, `user:/` and `system:/`.
 *
 * @par Errors
 * If `parentKey == NULL` or @p parentKey has read-only metadata, kdbSet() will
 * immediately return the error code -1. In all other error cases the following happens:
 * - Error information will be written into the metadata of the parent key, if possible.
 * - None of the keys are actually committed in this situation, i.e. no configuration file will be modified.
 *
 * In case of errors you should present the error message to the user and let the user decide what
 * to do. Possible solutions are:
 * - remove the problematic key and use kdbSet() again (for validation or type errors)
 * - change the value of the problematic key and use kdbSet() again (for validation errors)
 * - do a kdbGet() (for conflicts, i.e. error C02000) and then
 *   - set the same keyset again (in favour of what was set by this user)
 *   - drop the old keyset (in favour of what was set from another application)
 *   - merge the original, your own and the other keyset
 * - export the configuration into a file (for unresolvable errors)
 * - repeating the same kdbSet() might be of limited use, if the user does
 *   not explicitly request it, because temporary errors are rare and its
 *   unlikely that they fix themselves (e.g. disc full, permission problems)
 *
 * @par Optimization
 * Only backends that
 * - contain at least changed key according to elektraDiffCalculate(),
 * - contain fewer keys than at the end of kdbGet()
 * will be called.
 * There won't be an unnecessary write for unchanged keys.
 *
 * If none of the backends need an update, kdbSet() returns 0 and does nothing.
 *
 * @snippet kdbset.c set
 *
 * showElektraErrorDialog() and doElektraMerge() need to be implemented
 * by the user of Elektra. For doElektraMerge a 3-way merge algorithm exists in
 * libelektra-tools.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param ks a KeySet which should contain changed keys, otherwise nothing is done
 * @param parentKey Keys below @p parentKey will be set to @p handle.
 * It is also used to add warnings and set error information.
 *
 * @retval 1 on success
 * @retval 0 if nothing had to be done, no changes in KDB
 * @retval -1 on failure, no changes in KDB, an error will be set on
 * @p parentKey if possible (see "Errors" above)
 *
 * @since 1.0.0
 * @ingroup kdb
 * @see kdbOpen() for getting @p handle
 * @see kdbClose() that must be called afterwards
 * @see ksCurrent() contains the error Key
 */
int kdbSet (KDB * handle, KeySet * ks, Key * parentKey)
{
	// TODO [new_backend]: handle all namespaces according to kdb-operations.md
	//    including returning an error, as described above (Persistable Namespaces)
	//    including checking the namespace of parentKey

	// Step 0: check preconditions
	if (parentKey == NULL)
	{
		ELEKTRA_LOG_WARNING ("parentKey == NULL");
		return -1;
	}

	if (parentKey->hasReadOnlyMeta)
	{
		ELEKTRA_LOG_WARNING ("parentKey KEY_FLAG_RO_META");
		return -1;
	}

	clearErrorAndWarnings (parentKey);

	if (parentKey->hasReadOnlyName)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "parentKey with read-only name passed");
		ELEKTRA_LOG ("parentKey KEY_FLAG_RO_NAME");
		return -1;
	}

	if (parentKey->hasReadOnlyValue)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "parentKey with read-only value passed");
		ELEKTRA_LOG ("parentKey KEY_FLAG_RO_VALUE");
		return -1;
	}

	if (keyGetNamespace (parentKey) == KEY_NS_META)
	{
		ELEKTRA_SET_INTERFACE_ERRORF (parentKey, "parentKey with meta:/ name passed ('%s')", keyName (parentKey));
		return -1;
	}

	if (handle == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "NULL pointer passed for handle");
		return -1;
	}

	if (ks == NULL)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "NULL pointer passed for KeySet");
		return -1;
	}

	ELEKTRA_LOG ("now in new kdbSet (%s) %p %zd", keyName (parentKey), (void *) handle, ksGetSize (ks));

	int errnosave = errno;
	Key * initialParent = keyDup (parentKey, KEY_CP_ALL);

	// Step 1: find backends for parentKey
	KeySet * backends = backendsForParentKey (handle->backends, parentKey);

	// Step 2: check that backends are initialized
	bool backendsInit = true;
	for (elektraCursor i = 0; i < ksGetSize (backends); i++)
	{
		Key * backendKey = ksAtCursor (backends, i);
		const BackendData * backendData = keyValue (backendKey);

		// check that backend is initialized
		if (!backendData->initialized)
		{
			ELEKTRA_ADD_INTERFACE_WARNINGF (
				parentKey, "The mountpoint '%s' has not been initialized. You need to call kdbGet() before kdbSet().",
				keyName (backendKey));
			backendsInit = false;
			continue;
		}
	}


	// created early for error branch
	KeySet * setKs = ksNew (0, KS_END);

	if (!backendsInit)
	{
		ELEKTRA_SET_INTERFACE_ERROR (
			parentKey,
			"One or more mountpoints have not been initialized. Have you called kdbGet()? See warnings for details.");
		goto error;
	}

	// Step 3: run spec to add metadata

	if (handle->hooks.spec.plugin && handle->hooks.spec.copy (handle->hooks.spec.plugin, ks, parentKey, false) == -1)
	{
		goto error;
	}

	// Step 4: create deep-copy of ks
	// Note: This is needed so that ks retains its in-process state,
	//       after we transform the data into its on-disk state.
	for (elektraCursor i = 0; i < ksGetSize (backends); i++)
	{
		Key * backendKey = ksAtCursor (backends, i);

		KeySet * below = ksBelow (ks, backendKey);
		KeySet * deepDupedBelow = ksDeepDup (below);
		ksAppend (setKs, deepDupedBelow);
		ksDel (below);
		ksDel (deepDupedBelow);
	}

	// Step 5: split ks (for resolver and prestorage phases)
	if (!backendsDivide (backends, setKs))
	{
		ELEKTRA_SET_INTERNAL_ERROR (parentKey,
					    "Couldn't divide keys into mountpoints at start of kdbSet. Please report this bug at "
					    "https://issues.libelektra.org.");
		goto error;
	}

	// Step 6: remove read-only backends and backends that haven't changed since kdbGet()
	for (elektraCursor i = 0; i < ksGetSize (backends); i++)
	{
		Key * backendKey = ksAtCursor (backends, i);
		const BackendData * backendData = keyValue (backendKey);

		ElektraDiff * diff = elektraDiffCalculate (backendData->keys, handle->allKeys, backendKey);

		bool readOnly = keyGetMeta (backendKey, "meta:/internal/kdbreadonly") != NULL;
		bool changed = !elektraDiffIsEmpty (diff) || backendData->getSize != (size_t) ksGetSize (backendData->keys);

		elektraDiffDel (diff);

		// issue warning, if readonly but changed
		if (readOnly && changed)
		{
			ELEKTRA_ADD_INTERFACE_WARNINGF (parentKey,
							"The data under the mountpoint '%s' was changed since kdbGet(), but the mountpoint "
							"was intialized as read-only. The changes will not be stored.",
							keyName (backendKey));
		}

		// remove if read-only or unchanged
		if (readOnly || !changed)
		{
			elektraKsPopAtCursor (backends, i);
			--i;
		}
	}

	if (ksGetSize (backends) == 0)
	{
		keyCopy (parentKey, initialParent, KEY_CP_NAME | KEY_CP_VALUE);
		keyDel (initialParent);
		ksDel (backends);
		ksDel (setKs);
		errno = errnosave;

		return 0;
	}

	// Step 7a: resolve backends
	if (!resolveBackendsForSet (backends, parentKey))
	{
		goto rollback;
	}

	// Step 7b: run prestorage phase
	if (!runSetPhase (backends, parentKey, ELEKTRA_KDB_SET_PHASE_PRE_STORAGE, false, KDB_SET_FN_SET))
	{
		goto rollback;
	}

	// Step 8: merge data from all backends (for spec removal)
	ksClear (setKs);
	backendsMerge (backends, setKs);

	// there is no guarantee that the keys in the spec namespace are included in setKs
	// adding it before removing metadata
	Key * specNamespace = keyNew ("spec:/", KEY_END);
	KeySet * allKeysInSpecNamespace = ksBelow (ks, specNamespace);
	ksAppend (allKeysInSpecNamespace, setKs);

	// Step 9: run the spec plugin to remove copied metadata
	if (handle->hooks.spec.plugin && handle->hooks.spec.remove (handle->hooks.spec.plugin, allKeysInSpecNamespace, parentKey) == -1)
	{
		ksDel (allKeysInSpecNamespace);
		keyDel (specNamespace);

		goto rollback;
	}

	ksDel (allKeysInSpecNamespace);
	keyDel (specNamespace);

	// Step 10: split setKs for remaining phases
	if (!backendsDivide (backends, setKs))
	{
		ELEKTRA_SET_INTERNAL_ERROR (parentKey,
					    "Couldn't divide keys into mountpoints after spec removal. Please report this bug at "
					    "https://issues.libelektra.org.");
		goto rollback;
	}

	// Step 11a: run storage phase
	if (!runSetPhase (backends, parentKey, ELEKTRA_KDB_SET_PHASE_STORAGE, false, KDB_SET_FN_SET))
	{
		goto rollback;
	}

	// Step 11b: run poststorage phase
	if (!runSetPhase (backends, parentKey, ELEKTRA_KDB_SET_PHASE_POST_STORAGE, false, KDB_SET_FN_SET))
	{
		goto rollback;
	}

	// Step 12a: run precommit phase
	if (!runSetPhase (backends, parentKey, ELEKTRA_KDB_SET_PHASE_PRE_COMMIT, false, KDB_SET_FN_COMMIT))
	{
		goto rollback;
	}

	// Step 12b: run commit phase
	if (!runSetPhase (backends, parentKey, ELEKTRA_KDB_SET_PHASE_COMMIT, false, KDB_SET_FN_COMMIT))
	{
		goto rollback;
	}

	// Step 12c: run postcommit phase
	runSetPhase (backends, parentKey, ELEKTRA_KDB_SET_PHASE_POST_COMMIT, true, KDB_SET_FN_COMMIT);

	SendNotificationHook * sendNotificationHook = handle->hooks.sendNotification;
	while (sendNotificationHook != NULL)
	{
		if (sendNotificationHook->set != NULL)
		{
			// TODO (atmaxinger): Is setKs really the correct KeySet?
			sendNotificationHook->set (sendNotificationHook->plugin, setKs, parentKey);
		}

		sendNotificationHook = sendNotificationHook->next;
	}

	if (handle->hooks.record.record != NULL)
	{
		handle->hooks.record.record (handle->hooks.record.plugin, setKs, parentKey);
	}

	keyCopy (parentKey, initialParent, KEY_CP_NAME | KEY_CP_VALUE);
	keyDel (initialParent);
	ksDel (setKs);
	ksDel (backends);

	if (handle->allKeys != NULL)
	{
		ksDel (ksCut (handle->allKeys, parentKey));
		ksAppendDup (handle->allKeys, ks);
	}

	errno = errnosave;

	return 1;

rollback:
	// Step E1: run prerollback phase
	runSetPhase (backends, parentKey, ELEKTRA_KDB_SET_PHASE_PRE_ROLLBACK, true, KDB_SET_FN_ERROR);

	// Step E2: run rollback phase
	runSetPhase (backends, parentKey, ELEKTRA_KDB_SET_PHASE_ROLLBACK, true, KDB_SET_FN_ERROR);

	// Step E3: run postrollback phase
	runSetPhase (backends, parentKey, ELEKTRA_KDB_SET_PHASE_POST_ROLLBACK, true, KDB_SET_FN_ERROR);

error:
	keyCopy (parentKey, initialParent, KEY_CP_NAME | KEY_CP_VALUE);
	keyDel (initialParent);
	ksDel (setKs);
	ksDel (backends);

	errno = errnosave;

	return -1;
}

/**
 * @}
 */
