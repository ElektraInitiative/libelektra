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

#if DEBUG && defined(HAVE_STDIO_H)
#include <stdio.h>
#endif

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

#include <kdbinternal.h>


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
 * @image html state.png "State"
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


/**
 * @internal
 * Helper which iterates over MetaKeys from key
 * and removes all MetaKeys starting with
 * searchfor.
 */
void elektraRemoveMetaData (Key * key, const char * searchfor)
{
	const Key * iter_key;
	keyRewindMeta (key);
	while ((iter_key = keyNextMeta (key)) != 0)
	{
		/*startsWith*/
		if (strncmp (searchfor, keyName (iter_key), strlen (searchfor)) == 0)
		{
			keySetMeta (key, keyName (iter_key), 0);
		}
	}
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

	ksRewind (config);

	root = ksNext (config);
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


/**
 * @brief Bootstrap, first phase with fallback
 * @internal
 *
 * @param handle already allocated, but without defaultBackend
 * @param [out] keys for bootstrapping
 * @param errorKey key to add errors too
 *
 * @retval -1 failure: cannot initialize defaultBackend
 * @retval 0 warning: could not get initial config
 * @retval 1 success
 * @retval 2 success in fallback mode
 */
int elektraOpenBootstrap (KDB * handle, KeySet * keys, Key * errorKey)
{
	handle->defaultBackend = backendOpenDefault (handle->modules, handle->global, KDB_DB_INIT, errorKey);
	if (!handle->defaultBackend) return -1;

	handle->split = splitNew ();
	splitAppend (handle->split, handle->defaultBackend, keyNew (KDB_SYSTEM_ELEKTRA, KEY_END), 2);

	keySetName (errorKey, KDB_SYSTEM_ELEKTRA);
	keySetString (errorKey, "kdbOpen(): get");

	int funret = kdbGet (handle, keys, errorKey) != -1;
	elektraRemoveMetaData (errorKey, "error"); // fix errors from kdbGet()
	return funret;
}

/**
 * Checks whether the same instance of the list plugin is mounted in the global (maxonce) positions:
 *
 * pregetstorage, procgetstorage, postgetstorage, postgetcleanup,
 * presetstorage, presetcleanup, precommit, postcommit,
 * prerollback and postrollback
 *
 * @param handle the KDB handle to check
 * @param errorKey used for error reporting
 *
 * @retval 1 if list is mounted everywhere
 * @retval 0 otherwise
 */
static int ensureListPluginMountedEverywhere (KDB * handle, Key * errorKey)
{
	GlobalpluginPositions expectedPositions[] = { PREGETSTORAGE,
						      PROCGETSTORAGE,
						      POSTGETSTORAGE,
						      POSTGETCLEANUP,
						      PRESETSTORAGE,
						      PRESETCLEANUP,
						      PRECOMMIT,
						      POSTCOMMIT,
						      PREROLLBACK,
						      POSTROLLBACK,
						      -1 };

	Plugin * list = handle->globalPlugins[expectedPositions[0]][MAXONCE];
	if (list == NULL || elektraStrCmp (list->name, "list") != 0)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "list plugin not mounted at position %s/maxonce",
						 GlobalpluginPositionsStr[expectedPositions[0]]);
		return 0;
	}

	for (int i = 1; expectedPositions[i] > 0; ++i)
	{
		Plugin * plugin = handle->globalPlugins[expectedPositions[i]][MAXONCE];
		if (plugin != list)
		{
			// must always be the same instance
			ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "list plugin not mounted at position %s/maxonce",
							 GlobalpluginPositionsStr[expectedPositions[i]]);
			return 0;
		}
	}

	return 1;
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
 * Handles the system:/elektra/contract/mountglobal part of kdbOpen() contracts
 *
 * NOTE: @p contract will be modified
 *
 * @see kdbOpen()
 */
static int ensureContractMountGlobal (KDB * handle, KeySet * contract, Key * parentKey)
{
	if (!ensureListPluginMountedEverywhere (handle, parentKey))
	{
		return -1;
	}

	Plugin * listPlugin = handle->globalPlugins[PREGETSTORAGE][MAXONCE];
	typedef int (*mountPluginFun) (Plugin *, const char *, KeySet *, Key *);
	mountPluginFun listAddPlugin = (mountPluginFun) elektraPluginGetFunction (listPlugin, "mountplugin");
	typedef int (*unmountPluginFun) (Plugin *, const char *, Key *);
	unmountPluginFun listRemovePlugin = (unmountPluginFun) elektraPluginGetFunction (listPlugin, "unmountplugin");


	Key * mountContractRoot = keyNew ("system:/elektra/contract/mountglobal", KEY_END);
	Key * pluginConfigRoot = keyNew ("user:/", KEY_END);

	for (elektraCursor it = ksFindHierarchy (contract, mountContractRoot, NULL); it < ksGetSize (contract); it++)
	{
		Key * cur = ksAtCursor (contract, it);
		if (keyIsDirectlyBelow (mountContractRoot, cur) == 1)
		{
			const char * pluginName = keyBaseName (cur);
			KeySet * pluginConfig = ksCut (contract, cur);

			// increment ref count, because cur is part of pluginConfig and
			// we hold a reference to cur that is still needed (via pluginName)
			keyIncRef (cur);
			ksRename (pluginConfig, cur, pluginConfigRoot);

			int ret = listRemovePlugin (listPlugin, pluginName, parentKey);
			if (ret != ELEKTRA_PLUGIN_STATUS_ERROR)
			{
				ret = listAddPlugin (listPlugin, pluginName, pluginConfig, parentKey);
			}

			// we ned to delete cur separately, because it was ksCut() from contract
			// we also need to decrement the ref count, because it was incremented above
			keyDecRef (cur);
			keyDel (cur);

			if (ret == ELEKTRA_PLUGIN_STATUS_ERROR)
			{
				ELEKTRA_SET_INSTALLATION_ERRORF (
					parentKey, "The plugin '%s' couldn't be mounted globally (via the 'list' plugin).", pluginName);
				return -1;
			}

			// adjust cursor, because we removed the current key
			--it;
		}
	}

	keyDel (mountContractRoot);
	keyDel (pluginConfigRoot);

	return 0;
}

/**
 * Handles the @p contract argument of kdbOpen().
 *
 * @see kdbOpen()
 */
static int ensureContract (KDB * handle, const KeySet * contract, Key * parentKey)
{
	// TODO: tests
	// deep dup, so modifications to the keys in contract after kdbOpen() cannot modify the contract
	KeySet * dup = ksDeepDup (contract);

	ensureContractGlobalKs (handle, dup);
	int ret = ensureContractMountGlobal (handle, dup, parentKey);

	ksDel (dup);

	return ret;
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
		ELEKTRA_LOG ("no parent key passed");
		return 0;
	}

	ELEKTRA_LOG ("called with %s", keyName (errorKey));

	int errnosave = errno;
	KDB * handle = elektraCalloc (sizeof (struct _KDB));
	Key * initialParent = keyDup (errorKey, KEY_CP_ALL);

	handle->global = ksNew (0, KS_END);
	ksAppendKey (handle->global, keyNew ("system:/elektra/kdb", KEY_BINARY, KEY_SIZE, sizeof (handle), KEY_VALUE, &handle, KEY_END));
	handle->modules = ksNew (0, KS_END);
	if (elektraModulesInit (handle->modules, errorKey) == -1)
	{
		ksDel (handle->global);
		ksDel (handle->modules);
		elektraFree (handle);
		ELEKTRA_SET_INSTALLATION_ERROR (
			errorKey, "Method 'elektraModulesInit' returned with -1. See other warning or error messages for concrete details");

		keySetName (errorKey, keyName (initialParent));
		keySetString (errorKey, keyString (initialParent));
		keyDel (initialParent);
		errno = errnosave;
		return 0;
	}

	KeySet * keys = ksNew (0, KS_END);
	int inFallback = 0;
	switch (elektraOpenBootstrap (handle, keys, errorKey))
	{
	case -1:
		ksDel (handle->global);
		ksDel (handle->modules);
		elektraFree (handle);
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey,
						"Could not open default backend. See other warning or error messages for concrete details");

		keySetName (errorKey, keyName (initialParent));
		keySetString (errorKey, keyString (initialParent));
		keyDel (initialParent);
		errno = errnosave;
		ksDel (keys);
		return 0;
	case 0:
		ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Initial 'kdbGet()' failed, you should either fix " KDB_DB_INIT
							    " or the fallback " KDB_DB_FILE);
		break;
	case 2:
		ELEKTRA_LOG ("entered fallback code for bootstrapping");
		inFallback = 1;
		break;
	}

	keySetString (errorKey, "kdbOpen(): mountGlobals");

	if (mountGlobals (handle, ksDup (keys), handle->modules, errorKey) == -1)
	{
		// mountGlobals also sets a warning containing the name of the plugin that failed to load
		ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Mounting global plugins failed. Please see warning of concrete plugin");
	}

	keySetName (errorKey, keyName (initialParent));
	keySetString (errorKey, "kdbOpen(): backendClose");

	backendClose (handle->defaultBackend, errorKey);
	splitDel (handle->split);
	handle->split = 0;
	handle->defaultBackend = 0;
	handle->trie = 0;

#ifdef HAVE_LOGGER
	if (inFallback) ELEKTRA_LOG_WARNING ("fallback for bootstrapping: you might want to run `kdb upgrade-bootstrap`");

	Key * key;

	ksRewind (keys);
	for (key = ksNext (keys); key; key = ksNext (keys))
	{
		ELEKTRA_LOG_DEBUG ("config for createTrie name: %s value: %s", keyName (key), keyString (key));
	}
#endif

	if (contract != NULL)
	{
		keySetString (errorKey, "kdbOpen(): ensureContract");

		if (ensureContract (handle, contract, errorKey) != 0)
		{
			// error is set by ensureContract
			keySetString (errorKey, "kdbOpen(): close");
			kdbClose (handle, errorKey);

			keySetName (errorKey, keyName (initialParent));
			keySetString (errorKey, keyString (initialParent));
			keyDel (initialParent);
			errno = errnosave;
			return 0;
		}
	}

	handle->split = splitNew ();

	keySetString (errorKey, "kdbOpen(): mountOpen");
	// Open the trie, keys will be deleted within mountOpen
	if (mountOpen (handle, keys, handle->modules, errorKey) == -1)
	{
		ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Initial loading of trie did not work");
	}

	keySetString (errorKey, "kdbOpen(): mountDefault");
	if (mountDefault (handle, handle->modules, inFallback, errorKey) == -1)
	{
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "Could not reopen and mount default backend");
		keySetString (errorKey, "kdbOpen(): close");
		kdbClose (handle, errorKey);

		keySetName (errorKey, keyName (initialParent));
		keySetString (errorKey, keyString (initialParent));
		keyDel (initialParent);
		errno = errnosave;
		return 0;
	}

	keySetString (errorKey, "kdbOpen(): mountVersion");
	mountVersion (handle, errorKey);

	keySetString (errorKey, "kdbOpen(): mountModules");
	if (mountModules (handle, handle->modules, errorKey) == -1)
	{
		ELEKTRA_ADD_INTERNAL_WARNING (errorKey, "Mounting modules did not work");
	}

	keySetName (errorKey, keyName (initialParent));
	keySetString (errorKey, keyString (initialParent));
	keyDel (initialParent);
	errno = errnosave;

	return handle;
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

	if (handle->split)
	{
		splitDel (handle->split);
	}

	trieClose (handle->trie, errorKey);

	backendClose (handle->defaultBackend, errorKey);
	handle->defaultBackend = 0;

	// not set in fallback mode, so lets check:
	if (handle->initBackend)
	{
		backendClose (handle->initBackend, errorKey);
		handle->initBackend = 0;
	}

	for (int i = 0; i < NR_GLOBAL_POSITIONS; ++i)
	{
		for (int j = 0; j < NR_GLOBAL_SUBPOSITIONS; ++j)
		{
			elektraPluginClose (handle->globalPlugins[i][j], errorKey);
		}
	}

	if (handle->modules)
	{
		elektraModulesClose (handle->modules, errorKey);
		ksDel (handle->modules);
	}
	else
	{
		ELEKTRA_ADD_RESOURCE_WARNING (errorKey, "Could not close modules: modules were not open");
	}

	if (handle->global) ksDel (handle->global);

	elektraFree (handle);

	keySetName (errorKey, keyName (initialParent));
	keySetString (errorKey, keyString (initialParent));
	keyDel (initialParent);
	errno = errnosave;
	return 0;
}

/**
 * @internal
 *
 * @brief Check if an update is needed at all
 *
 * @retval -2 cache hit
 * @retval -1 an error occurred
 * @retval 0 no update needed
 * @retval number of plugins which need update
 */
static int elektraGetCheckUpdateNeeded (Split * split, Key * parentKey)
{
	int updateNeededOccurred = 0;
	size_t cacheHits = 0;
	for (size_t i = 0; i < split->size; i++)
	{
		int ret = -1;
		Backend * backend = split->handles[i];
		clear_bit (split->syncbits[i], (splitflag_t) SPLIT_FLAG_SYNC);

		Plugin * resolver = backend->getplugins[RESOLVER_PLUGIN];
		if (resolver && resolver->kdbGet)
		{
			ksRewind (split->keysets[i]);
			keySetName (parentKey, keyName (split->parents[i]));
			keySetString (parentKey, "");
			ret = resolver->kdbGet (resolver, split->keysets[i], parentKey);
			// store resolved filename
			keySetString (split->parents[i], keyString (parentKey));
			// no keys in that backend
			ELEKTRA_LOG_DEBUG ("backend: %s,%s ;; ret: %d", keyName (split->parents[i]), keyString (split->parents[i]), ret);

			backendUpdateSize (backend, split->parents[i], 0);
		}
		else
		{
<<<<<<< HEAD
			ELEKTRA_SET_ERROR (ELEKTRA_PLUGIN_STATUS_ERROR, parentKey, "kdbGet was missing in the resolver plugin");
=======
>>>>>>> f726eafad (Added some error check to kdbc elektraCacheLoadSplit and checkUpdateNeeded)
			ret = ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		switch (ret)
		{
		case ELEKTRA_PLUGIN_STATUS_CACHE_HIT:
			// Keys in cache are up-to-date
			++cacheHits;
			// Set sync flag, needed in case of cache miss
			// FALLTHROUGH
		case ELEKTRA_PLUGIN_STATUS_SUCCESS:
			// Seems like we need to sync that
			set_bit (split->syncbits[i], SPLIT_FLAG_SYNC);
			++updateNeededOccurred;
			break;
		case ELEKTRA_PLUGIN_STATUS_NO_UPDATE:
			// Nothing to do here
			break;
		default:
			ELEKTRA_ASSERT (0, "resolver did not return 1 0 -1, but %d", ret);
		case ELEKTRA_PLUGIN_STATUS_ERROR:
			// Ohh, an error occurred, lets stop the
			// process.
			return -1;
		}
	}

	if (cacheHits == split->size)
	{
		ELEKTRA_LOG_DEBUG ("all backends report cache is up-to-date");
		return -2;
	}

	return updateNeededOccurred;
}

typedef enum
{
	FIRST,
	LAST
} UpdatePass;

/**
 * @internal
 * @brief Do the real update.
 *
 * @retval -1 on error
 * @retval 0 on success
 */
static int elektraGetDoUpdate (Split * split, Key * parentKey)
{
	const int bypassedSplits = 1;
	for (size_t i = 0; i < split->size - bypassedSplits; i++)
	{
		if (!test_bit (split->syncbits[i], SPLIT_FLAG_SYNC))
		{
			// skip it, update is not needed
			continue;
		}
		Backend * backend = split->handles[i];
		ksRewind (split->keysets[i]);
		keySetName (parentKey, keyName (split->parents[i]));
		keySetString (parentKey, keyString (split->parents[i]));

		for (size_t p = 1; p < NR_OF_PLUGINS; ++p)
		{
			int ret = 0;
			if (backend->getplugins[p] && backend->getplugins[p]->kdbGet)
			{
				ret = backend->getplugins[p]->kdbGet (backend->getplugins[p], split->keysets[i], parentKey);
			}

			if (ret == -1)
			{
				// Ohh, an error occurred,
				// lets stop the process.
				return -1;
			}
		}
	}
	return 0;
}

static KeySet * prepareGlobalKS (KeySet * ks, Key * parentKey)
{
	ksRewind (ks);
	Key * cutKey = keyNew ("/", KEY_END);
	keyAddName (cutKey, strchr (keyName (parentKey), '/'));
	KeySet * cutKS = ksCut (ks, cutKey);
	Key * specCutKey = keyNew ("spec:/", KEY_END);
	KeySet * specCut = ksCut (cutKS, specCutKey);
	ksRewind (specCut);
	Key * cur;
	while ((cur = ksNext (specCut)) != NULL)
	{
		if (keyGetNamespace (cur) == KEY_NS_CASCADING)
		{
			ksAppendKey (cutKS, cur);
			keyDel (ksLookup (specCut, cur, KDB_O_POP));
		}
	}
	ksAppend (ks, specCut);
	ksDel (specCut);
	keyDel (specCutKey);
	keyDel (cutKey);
	ksRewind (cutKS);
	return cutKS;
}

static int elektraGetDoUpdateWithGlobalHooks (KDB * handle, Split * split, KeySet * ks, Key * parentKey, Key * initialParent,
					      UpdatePass run)
{
	const int bypassedSplits = 1;

	switch (run)
	{
	case FIRST:
		keySetName (parentKey, keyName (initialParent));
		if (elektraGlobalGet (handle, ks, parentKey, GETSTORAGE, INIT) == ELEKTRA_PLUGIN_ERROR)
		{
			return -1;
		}
		if (elektraGlobalGet (handle, ks, parentKey, GETSTORAGE, MAXONCE) == ELEKTRA_PLUGIN_ERROR)
		{
			return -1;
		}
		break;
	case LAST:
		keySetName (parentKey, keyName (initialParent));
		if (elektraGlobalGet (handle, ks, parentKey, PROCGETSTORAGE, INIT) == ELEKTRA_PLUGIN_ERROR)
		{
			return -1;
		}
		if (elektraGlobalGet (handle, ks, parentKey, PROCGETSTORAGE, MAXONCE) == ELEKTRA_PLUGIN_ERROR)
		{
			return -1;
		}
		elektraGlobalError (handle, ks, parentKey, PROCGETSTORAGE, DEINIT);
		break;
	default:
		break;
	}

	// elektraGlobalGet (handle, ks, parentKey, POSTGETSTORAGE, INIT);

	for (size_t i = 0; i < split->size - bypassedSplits; i++)
	{
		Backend * backend = split->handles[i];
		ksRewind (split->keysets[i]);
		keySetName (parentKey, keyName (split->parents[i]));
		keySetString (parentKey, keyString (split->parents[i]));
		int start, end;
		if (run == FIRST)
		{
			start = 1;
			end = STORAGE_PLUGIN + 1;
		}
		else
		{
			start = STORAGE_PLUGIN + 1;
			end = NR_OF_PLUGINS;
		}
		for (int p = start; p < end; ++p)
		{
			int ret = 0;

			if (p == (STORAGE_PLUGIN + 1) && handle->globalPlugins[PROCGETSTORAGE][FOREACH])
			{
				keySetName (parentKey, keyName (initialParent));
				ksRewind (ks);
				handle->globalPlugins[PROCGETSTORAGE][FOREACH]->kdbGet (handle->globalPlugins[PROCGETSTORAGE][FOREACH], ks,
											parentKey);
				keySetName (parentKey, keyName (split->parents[i]));
			}
			if (p == (STORAGE_PLUGIN + 2) && handle->globalPlugins[POSTGETSTORAGE][FOREACH])
			{
				keySetName (parentKey, keyName (initialParent));
				ksRewind (ks);
				handle->globalPlugins[POSTGETSTORAGE][FOREACH]->kdbGet (handle->globalPlugins[POSTGETSTORAGE][FOREACH], ks,
											parentKey);
				keySetName (parentKey, keyName (split->parents[i]));
			}
			else if (p == (NR_OF_PLUGINS - 1) && handle->globalPlugins[POSTGETCLEANUP][FOREACH])
			{
				keySetName (parentKey, keyName (initialParent));
				ksRewind (ks);
				handle->globalPlugins[POSTGETCLEANUP][FOREACH]->kdbGet (handle->globalPlugins[POSTGETCLEANUP][FOREACH], ks,
											parentKey);
				keySetName (parentKey, keyName (split->parents[i]));
			}

			if (backend->getplugins[p] && backend->getplugins[p]->kdbGet)
			{
				if (p <= STORAGE_PLUGIN)
				{
					if (!test_bit (split->syncbits[i], SPLIT_FLAG_SYNC))
					{
						// skip it, update is not needed
						continue;
					}

					ret = backend->getplugins[p]->kdbGet (backend->getplugins[p], split->keysets[i], parentKey);
				}
				else
				{
					KeySet * cutKS = prepareGlobalKS (ks, parentKey);
					ret = backend->getplugins[p]->kdbGet (backend->getplugins[p], cutKS, parentKey);
					ksAppend (ks, cutKS);
					ksDel (cutKS);
				}
			}

			if (ret == -1)
			{
				keySetName (parentKey, keyName (initialParent));
				// Ohh, an error occurred,
				// lets stop the process.
				elektraGlobalError (handle, ks, parentKey, GETSTORAGE, DEINIT);
				// elektraGlobalError (handle, ks, parentKey, POSTGETSTORAGE, DEINIT);
				return -1;
			}
		}
	}

	if (run == FIRST)
	{
		keySetName (parentKey, keyName (initialParent));
		elektraGlobalGet (handle, ks, parentKey, GETSTORAGE, DEINIT);
		// elektraGlobalGet (handle, ks, parentKey, POSTGETSTORAGE, DEINIT);
	}
	return 0;
}

static int copyError (Key * dest, Key * src)
{
	keyRewindMeta (src);
	const Key * metaKey = keyGetMeta (src, "error");
	if (!metaKey) return 0;
	keySetMeta (dest, keyName (metaKey), keyString (metaKey));
	while ((metaKey = keyNextMeta (src)) != NULL)
	{
		if (strncmp (keyName (metaKey), "error/", 6)) break;
		keySetMeta (dest, keyName (metaKey), keyString (metaKey));
	}
	return 1;
}
static void clearError (Key * key)
{
	keySetMeta (key, "error", 0);
	keySetMeta (key, "error/number", 0);
	keySetMeta (key, "error/description", 0);
	keySetMeta (key, "error/reason", 0);
	keySetMeta (key, "error/module", 0);
	keySetMeta (key, "error/file", 0);
	keySetMeta (key, "error/line", 0);
	keySetMeta (key, "error/configfile", 0);
	keySetMeta (key, "error/mountpoint", 0);
}

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
	Key * oldError = keyNew (keyName (parentKey), KEY_END);
	copyError (oldError, parentKey);
	int errnosave = errno;

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
<<<<<<< HEAD
	// TODO: there are no error checks here, see kdbGet
=======

	if (elektraGlobalGet (handle, ks, parentKey, PREGETSTORAGE, INIT) == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		goto error;
	}
	if (elektraGlobalGet (handle, ks, parentKey, PREGETSTORAGE, MAXONCE) == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		goto error;
	}
	if (elektraGlobalGet (handle, ks, parentKey, PREGETSTORAGE, DEINIT) == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		goto error;
	}
>>>>>>> f726eafad (Added some error check to kdbc elektraCacheLoadSplit and checkUpdateNeeded)

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

error:
	ELEKTRA_LOG_DEBUG ("now in error state");
	if (cacheParent) keyDel (cacheParent);
	if (cache) ksDel (cache);
	keySetName (parentKey, keyName (initialParent));
	elektraGlobalError (handle, ks, parentKey, POSTGETSTORAGE, INIT);
	elektraGlobalError (handle, ks, parentKey, POSTGETSTORAGE, MAXONCE);
	elektraGlobalError (handle, ks, parentKey, POSTGETSTORAGE, DEINIT);

	keySetName (parentKey, keyName (initialParent));
	if (handle) splitUpdateFileName (split, handle, parentKey);
	keyDel (initialParent);
	keyDel (oldError);
	splitDel (split);
	errno = errnosave;
	return -1;
}


/**
 * Retrieve Keys from a Key database in an atomic and universal way.
 *
 * @pre The @p handle must be passed as returned from kdbOpen().
 * @pre The @p returned KeySet must be a valid KeySet, e.g. constructed
 *     with ksNew().
 * @pre The @p parentKey Key must be a valid Key, e.g. constructed with
 *     keyNew().
 *
 * If you pass NULL on any parameter, kdbGet() will fail immediately without doing anything.
 *
 * The returned KeySet @p ks may already contain some keys, e.g. from previous
 * kdbGet() calls. The newly retrieved Keys will be appended using
 * ksAppendKey().
 *
 * If not done earlier, kdbGet() will fully retrieve all keys under the @p parentKey
 * folder recursively (See Optimization below when it will not be done).
 * Cascading Keys (starting with /) will retrieve the same path in all namespaces.
 * `/` will retrieve all Keys in @p handle.
 *
 * It is recommended to use the most specific @p parentKey possible. (e.g. using `system:/` is rarely the most specific)
 *
 * @note kdbGet() might retrieve more Keys than requested which are not
 *     below parentKey or even in a different namespace.
 *     These keys, except of `proc:/` keys, must be passed to calls of kdbSet(),
 *     otherwise they will be lost. This stems from the fact that the
 *     user has the only copy of the whole configuration and backends
 *     only write configuration that was passed to them.
 *     For example, if you kdbGet() "system:/mountpoint/interest"
 *     you will not only get all Keys below system:/mountpoint/interest,
 *     but also all Keys below system:/mountpoint (if system:/mountpoint
 *     is a mountpoint as the name suggests, but
 *     system:/mountpoint/interest is not a mountpoint).
 *     Make sure to not touch or remove Keys outside the Keys of interest,
 *     because others may need them!
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
 * @retval 1 if the Keys were retrieved successfully. There might be warnings attached to the parentKey! Depending on your use case, you
 * might need to treat them as errors!
 * @retval 0 if there was no update - no changes are made to the KeySet then. There might be warnings attached to the parentKey! Depending
 * on your use case, you might need to treat them as erorrs!
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
	elektraNamespace ns = keyGetNamespace (parentKey);
	if (ns == KEY_NS_NONE)
	{
		return -1;
	}

	Key * oldError = keyNew (keyName (parentKey), KEY_END);
	copyError (oldError, parentKey);

	if (ns == KEY_NS_META)
	{
		clearError (parentKey);
		keyDel (oldError);
		ELEKTRA_SET_INTERFACE_ERRORF (parentKey, "Metakey with name '%s' passed to kdbGet as parentkey", keyName (parentKey));
		return -1;
	}

	int errnosave = errno;
	Key * initialParent = keyDup (parentKey, KEY_CP_ALL);

	ELEKTRA_LOG ("now in new kdbGet (%s)", keyName (parentKey));

	Split * split = splitNew ();

	KeySet * cache = 0;
	Key * cacheParent = 0;
	int debugGlobalPositions = 0;

#ifdef DEBUG
	if (keyGetMeta (parentKey, "debugGlobalPositions") != 0)
	{
		debugGlobalPositions = 1;
	}
#endif

	if (!handle)
	{
		clearError (parentKey);
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "NULL pointer passed for handle");
		goto error;
	}

	if (!ks)
	{
		clearError (parentKey);
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "NULL pointer passed for KeySet");
		goto error;
	}

	if (splitBuildup (split, handle, parentKey) == -1)
	{
		clearError (parentKey);
		ELEKTRA_SET_INTERNAL_ERROR (parentKey, "Error in splitBuildup");
		goto error;
	}

	keySetName (parentKey, keyName (initialParent));
	cache = ksNew (0, KS_END);
	cacheParent = keyDup (mountGetMountpoint (handle, keyName (parentKey)), KEY_CP_ALL);
	if (cacheParent == NULL)
	{
		cacheParent = keyNew ("default:/", KEY_VALUE, "default", KEY_END);
	}
	if (ns == KEY_NS_CASCADING) keySetMeta (cacheParent, "cascading", "");
	keySetName (parentKey, keyName (initialParent));
	if (handle->globalPlugins[PREGETCACHE][MAXONCE])
	{
		elektraCacheLoad (handle, cache, parentKey, initialParent, cacheParent); // parentkey different from initialParent
	}

	int hasProcGetStorage = handle->globalPlugins[PROCGETSTORAGE][INIT] || handle->globalPlugins[PROCGETSTORAGE][MAXONCE] ||
				handle->globalPlugins[PROCGETSTORAGE][DEINIT];

	// Check if a update is needed at all
	switch (elektraGetCheckUpdateNeeded (split, parentKey))
	{
	case -2: // We have a cache hit
		// TODO: cache breaks procgetstorage
		if (hasProcGetStorage ||
		    elektraCacheLoadSplit (handle, split, ks, &cache, &cacheParent, parentKey, initialParent, debugGlobalPositions) != 0)
		{
			goto cachemiss;
		}

		keySetName (parentKey, keyName (initialParent));
		splitUpdateFileName (split, handle, parentKey);
		keyDel (initialParent);
		splitDel (split);
		errno = errnosave;
		keyDel (oldError);
		return 1;
	case 0: // We don't need an update so let's do nothing

		if (debugGlobalPositions)
		{
			keySetName (parentKey, keyName (initialParent));
			if (elektraGlobalGet (handle, ks, parentKey, PREGETSTORAGE, INIT) == ELEKTRA_PLUGIN_STATUS_ERROR)
			{
				goto error;
			}
			if (elektraGlobalGet (handle, ks, parentKey, PREGETSTORAGE, MAXONCE) == ELEKTRA_PLUGIN_STATUS_ERROR)
			{
				goto error;
			}
			if (elektraGlobalGet (handle, ks, parentKey, PREGETSTORAGE, DEINIT) == ELEKTRA_PLUGIN_STATUS_ERROR)
			{
				goto error;
			}

			keySetName (parentKey, keyName (initialParent));
			if (elektraGlobalGet (handle, ks, parentKey, PROCGETSTORAGE, INIT) == ELEKTRA_PLUGIN_STATUS_ERROR)
			{
				goto error;
			}
			if (elektraGlobalGet (handle, ks, parentKey, PROCGETSTORAGE, MAXONCE) == ELEKTRA_PLUGIN_STATUS_ERROR)
			{
				goto error;
			}
			if (elektraGlobalGet (handle, ks, parentKey, PROCGETSTORAGE, DEINIT) == ELEKTRA_PLUGIN_STATUS_ERROR)
			{
				goto error;
			}
		}

		ksDel (cache);
		cache = 0;
		keyDel (cacheParent);
		cacheParent = 0;

		if (debugGlobalPositions)
		{
			keySetName (parentKey, keyName (initialParent));
			if (elektraGlobalGet (handle, ks, parentKey, POSTGETSTORAGE, INIT) == ELEKTRA_PLUGIN_STATUS_ERROR)
			{
				goto error;
			}
			if (elektraGlobalGet (handle, ks, parentKey, POSTGETSTORAGE, MAXONCE) == ELEKTRA_PLUGIN_STATUS_ERROR)
			{
				goto error;
			}
			if (elektraGlobalGet (handle, ks, parentKey, POSTGETSTORAGE, DEINIT) == ELEKTRA_PLUGIN_STATUS_ERROR)
			{
				goto error;
			}
		}

		keySetName (parentKey, keyName (initialParent));
		splitUpdateFileName (split, handle, parentKey);
		keyDel (initialParent);
		splitDel (split);
		errno = errnosave;
		keyDel (oldError);
		return 0;
	case -1:
		goto error;
		// otherwise fall trough
	}

cachemiss:
	ksDel (cache);
	cache = 0;

	if (elektraGlobalGet (handle, ks, parentKey, PREGETSTORAGE, INIT) == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		goto error;
	}
	if (elektraGlobalGet (handle, ks, parentKey, PREGETSTORAGE, MAXONCE) == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		goto error;
	}
	if (elektraGlobalGet (handle, ks, parentKey, PREGETSTORAGE, DEINIT) == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		goto error;
	}

	// Appoint keys (some in the bypass)
	if (splitAppoint (split, handle, ks) == -1)
	{
		clearError (parentKey);
		ELEKTRA_SET_INTERNAL_ERROR (parentKey, "Error in splitAppoint");
		goto error;
	}

	if (handle->globalPlugins[POSTGETSTORAGE][FOREACH] || handle->globalPlugins[POSTGETCLEANUP][FOREACH] ||
	    handle->globalPlugins[PROCGETSTORAGE][FOREACH] || hasProcGetStorage)
	{
		clearError (parentKey);
		if (elektraGetDoUpdateWithGlobalHooks (handle, split, ks, parentKey, initialParent, FIRST) == -1)
		{
			goto error;
		}
		else
		{
			copyError (parentKey, oldError);
		}

		keySetName (parentKey, keyName (initialParent));

		if (splitGet (split, parentKey, handle) == -1)
		{
			ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (parentKey, "Wrong keys in postprocessing: %s", keyName (ksCurrent (ks)));
			// continue, because sizes are already updated
		}
		ksClear (ks);
		splitMergeBackends (split, ks);

		if (elektraGlobalGet (handle, ks, parentKey, POSTGETSTORAGE, INIT) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			goto error;
		}
		if (elektraGlobalGet (handle, ks, parentKey, POSTGETSTORAGE, MAXONCE) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			goto error;
		}
		if (elektraGlobalGet (handle, ks, parentKey, POSTGETSTORAGE, DEINIT) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			goto error;
		}

		clearError (parentKey);
		if (elektraGetDoUpdateWithGlobalHooks (handle, split, ks, parentKey, initialParent, LAST) == -1)
		{
			goto error;
		}
		else
		{
			copyError (parentKey, oldError);
		}
	}
	else
	{

		/* Now do the real updating,
		   but not for bypassed keys in split->size-1 */
		clearError (parentKey);
		// do everything up to position get_storage
		if (elektraGetDoUpdate (split, parentKey) == -1)
		{
			goto error;
		}
		else
		{
			copyError (parentKey, oldError);
		}

		/* Now post-process the updated keysets */
		if (splitGet (split, parentKey, handle) == -1)
		{
			ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (parentKey, "Wrong keys in postprocessing: %s", keyName (ksCurrent (ks)));
			// continue, because sizes are already updated
		}

		ksClear (ks);
		splitMergeBackends (split, ks);

		keySetName (parentKey, keyName (initialParent));

		if (elektraGlobalGet (handle, ks, parentKey, POSTGETSTORAGE, INIT) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			goto error;
		}
		if (elektraGlobalGet (handle, ks, parentKey, POSTGETSTORAGE, MAXONCE) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			goto error;
		}
		if (elektraGlobalGet (handle, ks, parentKey, POSTGETSTORAGE, DEINIT) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			goto error;
		}
	}

	keySetName (parentKey, keyName (initialParent));

	if (handle->globalPlugins[POSTGETCACHE][MAXONCE])
	{
		splitCacheStoreState (handle, split, handle->global, cacheParent, initialParent);
		KeySet * proc = elektraCutProc (ks); // remove proc keys before caching
		if (elektraGlobalSet (handle, ks, cacheParent, POSTGETCACHE, MAXONCE) != ELEKTRA_PLUGIN_STATUS_SUCCESS)
		{
			ELEKTRA_LOG_DEBUG ("CACHE ERROR: could not store cache");
			// we must remove the stored split state from the global keyset
			// if there was an error, otherwise we get erroneous cache hits
			elektraCacheCutMeta (handle);
		}
		elektraRestoreProc (ks, proc);
	}
	else
	{
		elektraCacheCutMeta (handle);
	}
	keyDel (cacheParent);
	cacheParent = 0;

	// the default split is not handled by POSTGETSTORAGE
	splitMergeDefault (split, ks);

	ksRewind (ks);

	keySetName (parentKey, keyName (initialParent));

	splitUpdateFileName (split, handle, parentKey);
	keyDel (initialParent);
	keyDel (oldError);
	splitDel (split);
	errno = errnosave;
	return 1;

error:
	ELEKTRA_LOG_DEBUG ("now in error state");
	if (cacheParent) keyDel (cacheParent);
	if (cache) ksDel (cache);
	keySetName (parentKey, keyName (initialParent));
	elektraGlobalError (handle, ks, parentKey, POSTGETSTORAGE, INIT);
	elektraGlobalError (handle, ks, parentKey, POSTGETSTORAGE, MAXONCE);
	elektraGlobalError (handle, ks, parentKey, POSTGETSTORAGE, DEINIT);

	keySetName (parentKey, keyName (initialParent));
	if (handle) splitUpdateFileName (split, handle, parentKey);
	keyDel (initialParent);
	keyDel (oldError);
	splitDel (split);
	errno = errnosave;
	return -1;
}

/**
 * @internal
 * @brief Does all set steps but not commit
 *
 * @param split all information for iteration
 * @param parentKey to add warnings (also passed to plugins for the same reason)
 * @param [out] errorKey may point to which key caused the error or 0 otherwise
 *
 * @retval -1 on error
 * @retval 0 on success
 */
static int elektraSetPrepare (Split * split, Key * parentKey, Key ** errorKey, Plugin * hooks[][NR_GLOBAL_SUBPOSITIONS])
{
	int any_error = 0;
	for (size_t i = 0; i < split->size; i++)
	{
		for (size_t p = 0; p < COMMIT_PLUGIN; ++p)
		{
			int ret = 0; // last return value

			Backend * backend = split->handles[i];
			ksRewind (split->keysets[i]);
			if (backend->setplugins[p] && backend->setplugins[p]->kdbSet)
			{
				if (p != 0)
				{
					keySetString (parentKey, keyString (split->parents[i]));
				}
				else
				{
					keySetString (parentKey, "");
				}
				keySetName (parentKey, keyName (split->parents[i]));
				ret = backend->setplugins[p]->kdbSet (backend->setplugins[p], split->keysets[i], parentKey);

#if VERBOSE && DEBUG
				printf ("Prepare %s with keys %zd in plugin: %zu, split: %zu, ret: %d\n", keyName (parentKey),
					ksGetSize (split->keysets[i]), p, i, ret);
#endif

				if (p == 0)
				{
					if (ret == 0)
					{
						// resolver says that sync is
						// not needed, so we
						// skip other pre-commit
						// plugins
						break;
					}
					keySetString (split->parents[i], keyString (parentKey));
				}
			}

			if (p == 0)
			{
				if (hooks[PRESETSTORAGE][FOREACH])
				{
					ksRewind (split->keysets[i]);
					hooks[PRESETSTORAGE][FOREACH]->kdbSet (hooks[PRESETSTORAGE][FOREACH], split->keysets[i], parentKey);
				}
			}
			else if (p == (STORAGE_PLUGIN - 1))
			{
				if (hooks[PRESETCLEANUP][FOREACH])
				{
					ksRewind (split->keysets[i]);
					hooks[PRESETCLEANUP][FOREACH]->kdbSet (hooks[PRESETCLEANUP][FOREACH], split->keysets[i], parentKey);
				}
			}

			if (ret == -1)
			{
				// do not
				// abort because it might
				// corrupt the KeySet
				// and leads to warnings
				// because of .tmp files not
				// found
				*errorKey = ksCurrent (split->keysets[i]);

				// so better keep going, but of
				// course we will not commit
				any_error = -1;
			}
		}
	}
	return any_error;
}

/**
 * @internal
 * @brief Does the commit
 *
 * @param split all information for iteration
 * @param parentKey to add warnings (also passed to plugins for the same reason)
 */
static void elektraSetCommit (Split * split, Key * parentKey)
{
	for (size_t p = COMMIT_PLUGIN; p < NR_OF_PLUGINS; ++p)
	{
		for (size_t i = 0; i < split->size; i++)
		{
			int ret = 0;
			Backend * backend = split->handles[i];

			if (backend->setplugins[p] && backend->setplugins[p]->kdbSet)
			{
				if (p != COMMIT_PLUGIN)
				{
					keySetString (parentKey, keyString (split->parents[i]));
				}
				keySetName (parentKey, keyName (split->parents[i]));
#if DEBUG && VERBOSE
				printf ("elektraSetCommit: %p # %zu with %s - %s\n", backend, p, keyName (parentKey),
					keyString (parentKey));
#endif
				ksRewind (split->keysets[i]);
				if (p == COMMIT_PLUGIN)
				{
					ret = backend->setplugins[p]->kdbCommit (backend->setplugins[p], split->keysets[i], parentKey);
					// name of non-temp file
					keySetString (split->parents[i], keyString (parentKey));
				}
				else
				{
					ret = backend->setplugins[p]->kdbSet (backend->setplugins[p], split->keysets[i], parentKey);
				}
			}

			if (ret == -1)
			{
				ELEKTRA_ADD_INTERNAL_WARNINGF (parentKey, "Error during commit. This means backend is broken: %s",
							       keyName (backend->mountpoint));
			}
		}
	}
}

/**
 * @internal
 * @brief Does the rollback
 *
 * @param split all information for iteration
 * @param parentKey to add warnings (also passed to plugins for the same reason)
 */
static void elektraSetRollback (Split * split, Key * parentKey)
{
	for (size_t p = 0; p < NR_OF_PLUGINS; ++p)
	{
		for (size_t i = 0; i < split->size; i++)
		{
			int ret = 0;
			Backend * backend = split->handles[i];

			ksRewind (split->keysets[i]);
			if (backend->errorplugins[p])
			{
				keySetName (parentKey, keyName (split->parents[i]));
				ret = backend->errorplugins[p]->kdbError (backend->errorplugins[p], split->keysets[i], parentKey);
			}

			if (ret == -1)
			{
				ELEKTRA_ADD_INTERNAL_WARNINGF (parentKey, "Error during rollback. This means backend is broken: %s",
							       keyName (backend->mountpoint));
			}
		}
	}
}


/**
 * Set Keys to a Key database in an atomic and universal way.
 *
 * @pre kdbGet() must be called before kdbSet():
 *    - initially (after kdbOpen())
 *    - after conflict errors in kdbSet().
 * @pre The @p returned KeySet must be a valid KeySet, e.g. constructed
 *     with ksNew().
 * @pre The @p parentKey Key must be a valid Key, e.g. constructed with
 *     keyNew(). It must not have read-only name, value or metadata.
 *
 * If you pass NULL on any parameter, kdbSet() will fail immediately without doing anything.
 *
 * With @p parentKey you can specify which part of the given keyset
 * is of interest for you. Then you promise to only modify or
 * remove keys below this key. All others would be passed back
 * as they were retrieved by kdbGet().
 * Cascading keys (starting with /) will set the path in all namespaces.
 * `/` will commit all keys.
 * This parameter is an optimization toonly save keys of mountpoints affected by the specified @p parentKey. This does not necessarily mean
 * that only changes to keys below that @p parentKey are saved. Meta-names in @p parentKey will be rejected (error C01320). Empty/Invalid
 * Keys will also be rejected (error C01320).
 *
 * @par Errors
 * If `parentKey == NULL` or @p parentKey has read-only metadata, kdbSet() will
 * immediately return the error code -1. In all other error cases the following happens:
 * - kdbSet() will leave the KeySet's * internal cursor on the key that generated the error.
 * - Error information will be written into the metadata of
 *   the parent key, if possible.
 * - None of the keys are actually committed in this situation, i.e. no
 *   configuration file will be modified.
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
 * - repeat the same kdbSet might be of limited use if the user does
 *   not explicitly request it, because temporary
 *   errors are rare and its unlikely that they fix themselves
 *   (e.g. disc full, permission problems)
 *
 * @par Optimization
 * Each key is checked with keyNeedSync() before being actually committed.
 * If no key of a backend needs to be synced
 * any affairs to backends are omitted and 0 is returned.
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
	if (parentKey == NULL)
	{
		ELEKTRA_LOG ("parentKey == NULL");
		return -1;
	}

	if (test_bit (parentKey->flags, KEY_FLAG_RO_META))
	{
		ELEKTRA_LOG ("parentKey KEY_FLAG_RO_META");
		return -1;
	}

	if (test_bit (parentKey->flags, KEY_FLAG_RO_NAME))
	{
		clearError (parentKey);
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "parentKey with read-only name passed");
		ELEKTRA_LOG ("parentKey KEY_FLAG_RO_NAME");
		return -1;
	}

	if (test_bit (parentKey->flags, KEY_FLAG_RO_VALUE))
	{
		clearError (parentKey);
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "parentKey with read-only value passed");
		ELEKTRA_LOG ("parentKey KEY_FLAG_RO_VALUE");
		return -1;
	}

	if (handle == NULL)
	{
		clearError (parentKey);
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "KDB handle null pointer passed");
		ELEKTRA_LOG ("handle == NULL");
		return -1;
	}

	if (ks == NULL)
	{
		clearError (parentKey);
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "KeySet null pointer passed");
		ELEKTRA_LOG ("ks == NULL");
		return -1;
	}

	elektraNamespace ns = keyGetNamespace (parentKey);
	Key * oldError = keyNew (keyName (parentKey), KEY_END);
	copyError (oldError, parentKey);

	if (ns == KEY_NS_META)
	{
		clearError (parentKey); // clear previous error to set new one
		ELEKTRA_SET_INTERFACE_ERRORF (parentKey, "Metakey with name '%s' passed to kdbSet as parentkey", keyName (parentKey));
		keyDel (oldError);
		ELEKTRA_LOG ("ns == KEY_NS_META");
		return -1;
	}

	int errnosave = errno;
	Key * initialParent = keyDup (parentKey, KEY_CP_ALL);

	ELEKTRA_LOG ("now in new kdbSet (%s) %p %zd", keyName (parentKey), (void *) handle, ksGetSize (ks));

	elektraGlobalSet (handle, ks, parentKey, PRESETSTORAGE, INIT);
	elektraGlobalSet (handle, ks, parentKey, PRESETSTORAGE, MAXONCE);
	elektraGlobalSet (handle, ks, parentKey, PRESETSTORAGE, DEINIT);

	ELEKTRA_LOG ("after presetstorage maxonce(%s) %p %zd", keyName (parentKey), (void *) handle, ksGetSize (ks));

	Split * split = splitNew ();
	Key * errorKey = 0;

	if (splitBuildup (split, handle, parentKey) == -1)
	{
		clearError (parentKey); // clear previous error to set new one
		ELEKTRA_SET_INTERNAL_ERROR (parentKey, "Error in splitBuildup");
		goto error;
	}
	ELEKTRA_LOG ("after splitBuildup");

	// 1.) Search for syncbits
	int syncstate = splitDivide (split, handle, ks);
	if (syncstate == -1)
	{
		clearError (parentKey); // clear previous error to set new one
		ELEKTRA_SET_INSTALLATION_ERRORF (parentKey, "No default backend found, but should be. Keyname: %s",
						 keyName (ksCurrent (ks)));
		goto error;
	}
	ELEKTRA_ASSERT (syncstate == 0 || syncstate == 1, "syncstate not 0 or 1, but %d", syncstate);
	ELEKTRA_LOG ("after 1.) Search for syncbits");

	// 2.) Search for changed sizes
	syncstate |= splitSync (split);
	ELEKTRA_ASSERT (syncstate <= 1, "syncstate not equal or below 1, but %d", syncstate);
	if (syncstate != 1)
	{
		/* No update is needed */
		ELEKTRA_LOG ("No update is needed");
		keySetName (parentKey, keyName (initialParent));
		if (syncstate < 0) clearError (parentKey); // clear previous error to set new one
		if (syncstate == -1)
		{
			ELEKTRA_SET_INTERNAL_ERROR (parentKey, "Assert failed: invalid namespace");
			ELEKTRA_LOG ("syncstate == -1");
		}
		else if (syncstate < -1)
		{
			ELEKTRA_SET_CONFLICTING_STATE_ERRORF (
				parentKey, "Sync state is wrong, maybe 'kdbSet()' is executed without prior 'kdbGet()' on %s",
				keyName (split->parents[-syncstate - 2]));
			ELEKTRA_LOG ("syncstate < -1");
		}
		keyDel (initialParent);
		splitDel (split);
		errno = errnosave;
		keyDel (oldError);
		ELEKTRA_LOG ("return: %d", syncstate == 0 ? 0 : -1);
		return syncstate == 0 ? 0 : -1;
	}
	ELEKTRA_ASSERT (syncstate == 1, "syncstate not 1, but %d", syncstate);
	ELEKTRA_LOG ("after 2.) Search for changed sizes");

	splitPrepare (split);

	clearError (parentKey); // clear previous error to set new one
	if (elektraSetPrepare (split, parentKey, &errorKey, handle->globalPlugins) == -1)
	{
		goto error;
	}
	else
	{
		// no error, restore old error
		copyError (parentKey, oldError);
	}
	keySetName (parentKey, keyName (initialParent));

	elektraGlobalSet (handle, ks, parentKey, PRECOMMIT, INIT);
	elektraGlobalSet (handle, ks, parentKey, PRECOMMIT, MAXONCE);
	elektraGlobalSet (handle, ks, parentKey, PRECOMMIT, DEINIT);

	elektraSetCommit (split, parentKey);

	elektraGlobalSet (handle, ks, parentKey, COMMIT, INIT);
	elektraGlobalSet (handle, ks, parentKey, COMMIT, MAXONCE);
	elektraGlobalSet (handle, ks, parentKey, COMMIT, DEINIT);

	splitUpdateSize (split);

	keySetName (parentKey, keyName (initialParent));

	elektraGlobalSet (handle, ks, parentKey, POSTCOMMIT, INIT);
	elektraGlobalSet (handle, ks, parentKey, POSTCOMMIT, MAXONCE);
	elektraGlobalSet (handle, ks, parentKey, POSTCOMMIT, DEINIT);

	for (size_t i = 0; i < ks->size; ++i)
	{
		// remove all flags from all keys
		clear_bit (ks->array[i]->flags, (keyflag_t) KEY_FLAG_SYNC);
	}

	keySetName (parentKey, keyName (initialParent));
	keyDel (initialParent);
	splitDel (split);

	keyDel (oldError);
	errno = errnosave;
	ELEKTRA_LOG ("before RETURN 1");
	return 1;

error:
	keySetName (parentKey, keyName (initialParent));

	elektraGlobalError (handle, ks, parentKey, PREROLLBACK, INIT);
	elektraGlobalError (handle, ks, parentKey, PREROLLBACK, MAXONCE);
	elektraGlobalError (handle, ks, parentKey, PREROLLBACK, DEINIT);

	elektraSetRollback (split, parentKey);

	if (errorKey)
	{
		Key * found = ksLookup (ks, errorKey, 0);
		if (!found)
		{
			ELEKTRA_ADD_INTERNAL_WARNINGF (parentKey, "Error key %s not found in keyset even though it was found before",
						       keyName (errorKey));
		}
	}

	keySetName (parentKey, keyName (initialParent));

	elektraGlobalError (handle, ks, parentKey, POSTROLLBACK, INIT);
	elektraGlobalError (handle, ks, parentKey, POSTROLLBACK, MAXONCE);
	elektraGlobalError (handle, ks, parentKey, POSTROLLBACK, DEINIT);

	keySetName (parentKey, keyName (initialParent));
	keyDel (initialParent);
	splitDel (split);
	errno = errnosave;
	keyDel (oldError);
	return -1;
}

/**
 * @}
 */
