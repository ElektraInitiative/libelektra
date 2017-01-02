/**
 * @file
 *
 * @brief Low level functions for access the Key Database.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
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
 *   error. For kdbGet() / kdbSet() it is used to give an hint which keys
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
 * - a conflict detection (error 30) and
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
 * are allowed, because otherwise conflicts (error 30) would not be detected.
 *
 * Once kdbGet() was called (for a specific handle+parentKey),
 * any number of kdbGet() and kdbSet() can be
 * used with this handle respective parentKey, unless kdbSet() had
 * a conflict (error 30) with another application.
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
 * @internal
 * Helper functions to execute global plugins
 */
void elektraGlobalGet (KDB * handle, KeySet * ks, Key * parentKey, int position, int subPosition)
{
	Plugin * plugin;
	if (handle && (plugin = handle->globalPlugins[position][subPosition]))
	{
		plugin->kdbGet (plugin, ks, parentKey);
	}
}

void elektraGlobalSet (KDB * handle, KeySet * ks, Key * parentKey, int position, int subPosition)
{
	Plugin * plugin;
	if (handle && (plugin = handle->globalPlugins[position][subPosition]))
	{
		plugin->kdbSet (plugin, ks, parentKey);
	}
}

void elektraGlobalError (KDB * handle, KeySet * ks, Key * parentKey, int position, int subPosition)
{
	Plugin * plugin;
	if (handle && (plugin = handle->globalPlugins[position][subPosition]))
	{
		plugin->kdbError (plugin, ks, parentKey);
	}
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
	handle->defaultBackend = backendOpenDefault (handle->modules, KDB_DB_INIT, errorKey);
	if (!handle->defaultBackend) return -1;

	handle->split = splitNew ();
	splitAppend (handle->split, handle->defaultBackend, keyNew (KDB_SYSTEM_ELEKTRA, KEY_END), 2);

	keySetName (errorKey, KDB_SYSTEM_ELEKTRA);
	keySetString (errorKey, "kdbOpen(): get");

	int funret = 1;
	int ret = kdbGet (handle, keys, errorKey);
	int fallbackret = 0;
	if (ret == 0 || ret == -1)
	{
		// could not get KDB_DB_INIT, try KDB_DB_FILE
		// first cleanup:
		ksClear (keys);
		backendClose (handle->defaultBackend, errorKey);
		splitDel (handle->split);

		// then create new setup:
		handle->defaultBackend = backendOpenDefault (handle->modules, KDB_DB_FILE, errorKey);
		if (!handle->defaultBackend)
		{
			elektraRemoveMetaData (errorKey, "error"); // fix errors from kdbGet()
			return -1;
		}
		handle->split = splitNew ();
		splitAppend (handle->split, handle->defaultBackend, keyNew (KDB_SYSTEM_ELEKTRA, KEY_END), 2);

		keySetName (errorKey, KDB_SYSTEM_ELEKTRA);
		keySetString (errorKey, "kdbOpen(): get fallback");
		fallbackret = kdbGet (handle, keys, errorKey);
		keySetName (errorKey, "system/elektra/mountpoints");

		KeySet * cutKeys = ksCut (keys, errorKey);
		if (fallbackret == 1 && ksGetSize (cutKeys) != 0)
		{
			funret = 2;
		}
		ksAppend (keys, cutKeys);
		ksDel (cutKeys);
	}

	if (ret == -1 && fallbackret == -1)
	{
		funret = 0;
	}

	elektraRemoveMetaData (errorKey, "error"); // fix errors from kdbGet()
	return funret;
}


/**
 * @brief Opens the session with the Key database.
 *
 * @pre errorKey must be a valid key, e.g. created with keyNew()
 *
 * The method will bootstrap itself the following way.
 * The first step is to open the default backend. With it
 * system/elektra/mountpoints will be loaded and all needed
 * libraries and mountpoints will be determined.
 * These libraries for backends will be loaded and with it the
 * @p KDB datastructure will be initialized.
 *
 * You must always call this method before retrieving or committing any
 * keys to the database. In the end of the program,
 * after using the key database, you must not forget to kdbClose().
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
 * @param errorKey the key which holds errors and warnings which were issued
 * @see kdbGet(), kdbClose() to end all affairs to the key database.
 * @retval handle on success
 * @retval NULL on failure
 * @ingroup kdb
 */
KDB * kdbOpen (Key * errorKey)
{
	if (!errorKey)
	{
		ELEKTRA_LOG ("no error key passed");
		return 0;
	}

	ELEKTRA_LOG ("called with %s", keyName (errorKey));

	int errnosave = errno;
	KDB * handle = elektraCalloc (sizeof (struct _KDB));
	Key * initialParent = keyDup (errorKey);

	handle->modules = ksNew (0, KS_END);
	if (elektraModulesInit (handle->modules, errorKey) == -1)
	{
		ksDel (handle->modules);
		elektraFree (handle);
		ELEKTRA_SET_ERROR (94, errorKey, "elektraModulesInit returned with -1");

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
		ksDel (handle->modules);
		elektraFree (handle);
		ELEKTRA_SET_ERROR (40, errorKey, "could not open default backend");

		keySetName (errorKey, keyName (initialParent));
		keySetString (errorKey, keyString (initialParent));
		keyDel (initialParent);
		errno = errnosave;
		return 0;
	case 0:
		ELEKTRA_ADD_WARNING (17, errorKey,
				     "Initial kdbGet() failed, you should either fix " KDB_DB_INIT " or the fallback " KDB_DB_FILE);
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
		ELEKTRA_ADD_WARNING (139, errorKey, "Mounting global plugins failed");
	}

	keySetName (errorKey, keyName (initialParent));
	keySetString (errorKey, "kdbOpen(): backendClose");

	backendClose (handle->defaultBackend, errorKey);
	splitDel (handle->split);
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

	handle->split = splitNew ();

	keySetString (errorKey, "kdbOpen(): mountOpen");
	// Open the trie, keys will be deleted within mountOpen
	if (mountOpen (handle, keys, handle->modules, errorKey) == -1)
	{
		ELEKTRA_ADD_WARNING (93, errorKey, "Initial loading of trie did not work");
	}

	keySetString (errorKey, "kdbOpen(): mountDefault");
	if (mountDefault (handle, handle->modules, inFallback, errorKey) == -1)
	{
		ELEKTRA_SET_ERROR (40, errorKey, "could not reopen and mount default backend");
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
		ELEKTRA_ADD_WARNING (92, errorKey, "Mounting modules did not work");
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
 *
 * @pre errorKey must be a valid key, e.g. created with keyNew()
 *
 * This is the counterpart of kdbOpen().
 *
 * You must call this method when you finished your affairs with the key
 * database. You can manipulate Key and KeySet objects also after
 * kdbClose(), but you must not use any kdb*() call afterwards.
 *
 * The @p handle parameter will be finalized and all resources associated to it
 * will be freed. After a kdbClose(), the @p handle cannot be used anymore.
 *
 * @param handle contains internal information of
 *               @link kdbOpen() opened @endlink key database
 * @param errorKey the key which holds error/warning information
 * @retval 0 on success
 * @retval -1 on NULL pointer
 * @ingroup kdb
 */
int kdbClose (KDB * handle, Key * errorKey)
{
	if (!handle)
	{
		return -1;
	}

	Key * initialParent = keyDup (errorKey);
	int errnosave = errno;
	splitDel (handle->split);

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
		ELEKTRA_ADD_WARNING (47, errorKey, "modules were not open");
	}

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
 * @retval -1 an error occurred
 * @retval 0 no update needed
 * @retval number of plugins which need update
 */
static int elektraGetCheckUpdateNeeded (Split * split, Key * parentKey)
{
	// GLOBAL: getresolver [init]
	// GLOBAL: getresolver [max once]
	int updateNeededOccurred = 0;
	for (size_t i = 0; i < split->size; i++)
	{
		int ret = -1;
		Backend * backend = split->handles[i];
		clear_bit (split->syncbits[i], 1);

		if (backend->getplugins[RESOLVER_PLUGIN] && backend->getplugins[RESOLVER_PLUGIN]->kdbGet)
		{
			// GLOBAL: getresolver [foreach]
			ksRewind (split->keysets[i]);
			keySetName (parentKey, keyName (split->parents[i]));
			keySetString (parentKey, "");
			ret = backend->getplugins[RESOLVER_PLUGIN]->kdbGet (backend->getplugins[RESOLVER_PLUGIN], split->keysets[i],
									    parentKey);
			// store resolved filename
			keySetString (split->parents[i], keyString (parentKey));
			// no keys in that backend
			backendUpdateSize (backend, split->parents[i], 0);
		}
		// TODO: set error in else case!

		switch (ret)
		{
		case 1:
			// Seems like we need to sync that
			set_bit (split->syncbits[i], SPLIT_FLAG_SYNC);
			++updateNeededOccurred;
			break;
		case 0:
			// Nothing to do here
			break;
		default:
			ELEKTRA_ASSERT (0, "resolver did not return 1 0 -1, but %d", ret);
		case -1:
			// Ohh, an error occurred, lets stop the
			// process.
			return -1;
		}
	}
	// GLOBAL: getresolver [deinit]
	return updateNeededOccurred;
}

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

typedef enum { FIRST, LAST } UpdatePass;

static KeySet * prepareGlobalKS (KeySet * ks, Key * parentKey)
{
	ksRewind (ks);
	Key * cutKey = keyNew ("/", KEY_CASCADING_NAME, KEY_END);
	keyAddName (cutKey, strchr (keyName (parentKey), '/'));
	KeySet * cutKS = ksCut (ks, cutKey);
	Key * specCutKey = keyNew ("spec", KEY_END);
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
	int pgs_done = 0;
	int pgc_done = 0;

	elektraGlobalGet(handle, ks, parentKey, GETSTORAGE, INIT);
	elektraGlobalGet(handle, ks, parentKey, GETSTORAGE, MAXONCE);

	// GLOBAL: postgetstorage [init]
	// GLOBAL: postgetstorage [max once]

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

			// GLOBAL: getstorage [foreach]
			// GLOBAL: postgetstorage [foreach]

			if (!pgs_done && (p == (STORAGE_PLUGIN + 1)) && handle->globalPlugins[POSTGETSTORAGE][MAXONCE])
			{
				pgs_done = 1;
				keySetName (parentKey, keyName (initialParent));
				ksRewind (ks);
				handle->globalPlugins[POSTGETSTORAGE][MAXONCE]->kdbGet (handle->globalPlugins[POSTGETSTORAGE][MAXONCE], ks, parentKey);
				keySetName (parentKey, keyName (split->parents[i]));
			}
			else if (!pgc_done && (p == (NR_OF_PLUGINS - 1)) && handle->globalPlugins[POSTGETCLEANUP][MAXONCE])
			{
				pgc_done = 1;
				keySetName (parentKey, keyName (initialParent));
				ksRewind (ks);
				handle->globalPlugins[POSTGETCLEANUP][MAXONCE]->kdbGet (handle->globalPlugins[POSTGETCLEANUP][MAXONCE], ks, parentKey);
				keySetName (parentKey, keyName (split->parents[i]));
			}

			if (backend->getplugins[p] && backend->getplugins[p]->kdbGet)
			{
				if (p <= STORAGE_PLUGIN)
				{
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
				// Ohh, an error occurred,
				// lets stop the process.
				elektraGlobalGet(handle, ks, parentKey, GETSTORAGE, DEINIT);
				// GLOBAL: postgetstorage [deinit]
				return -1;
			}
		}
	}
	elektraGlobalGet(handle, ks, parentKey, GETSTORAGE, DEINIT);
	// GLOBAL: postgetstorage [deinit]
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
	keySetMeta (key, "error/ingroup", 0);
	keySetMeta (key, "error/module", 0);
	keySetMeta (key, "error/file", 0);
	keySetMeta (key, "error/line", 0);
	keySetMeta (key, "error/configfile", 0);
	keySetMeta (key, "error/mountpoint", 0);
}

/**
 * @brief Retrieve keys in an atomic and universal way.
 *
 * @pre The @p handle must be passed as returned from kdbOpen().
 *
 * @pre The @p returned KeySet must be a valid KeySet, e.g. constructed
 *     with ksNew().
 *
 * @pre The @p parentKey Key must be a valid Key, e.g. constructed with
 *     keyNew().
 *
 * If you pass NULL on any parameter kdbGet() will fail immediately without doing anything.
 *
 * The @p returned KeySet may already contain some keys, e.g. from previous
 * kdbGet() calls. The new retrieved keys will be appended using
 * ksAppendKey().
 *
 * If not done earlier kdbGet() will fully retrieve all keys under the @p parentKey
 * folder recursively (See Optimization below when it will not be done).
 *
 * @note kdbGet() might retrieve more keys then requested (that are not
 *     below parentKey). These keys must be passed to calls of kdbSet(),
 *     otherwise they will be lost. This stems from the fact that the
 *     user has the only copy of the whole configuration and backends
 *     only write configuration that was passed to them.
 *     For example, if you kdbGet() "system/mountpoint/interest"
 *     you will not only get all keys below system/mountpoint/interest,
 *     but also all keys below system/mountpoint (if system/mountpoint
 *     is a mountpoint as the name suggests, but
 *     system/mountpoint/interest is not a mountpoint).
 *     Make sure to not touch or remove keys outside the keys of interest,
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
 * In the first run of kdbGet all requested (or more) keys are retrieved. On subsequent
 * calls only the keys are retrieved where something was changed
 * inside the key database. The other keys stay in the
 * KeySet returned as passed.
 *
 * It is your responsibility to save the original keyset if you
 * need it afterwards.
 *
 * If you want to be sure to get a fresh keyset again, you need to open a
 * second handle to the key database using kdbOpen().
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param parentKey is used to add warnings and set an error
 *         information. Additionally, its name is a hint which keys
 *         should be retrieved (it is possible that more are retrieved, see Note above).
 *           - cascading keys (starting with /) will retrieve the same path in all namespaces
 *           - / will retrieve all keys
 * @param ks the (pre-initialized) KeySet returned with all keys found
 * 	will not be changed on error or if no update is required
 * @see ksLookup(), ksLookupByName() for powerful
 * 	lookups after the KeySet was retrieved
 * @see kdbOpen() which needs to be called before
 * @see kdbSet() to save the configuration afterwards and kdbClose() to
 * 	finish affairs with the key database.
 * @retval 1 if the keys were retrieved successfully
 * @retval 0 if there was no update - no changes are made to the keyset then
 * @retval -1 on failure - no changes are made to the keyset then
 * @ingroup kdb
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
		ELEKTRA_SET_ERRORF (104, parentKey, "metakey with name \"%s\" passed to kdbGet", keyName (parentKey));
		return -1;
	}

	if (ns == KEY_NS_EMPTY)
	{
		ELEKTRA_ADD_WARNING (105, parentKey, "invalid key name passed to kdbGet");
	}

	int errnosave = errno;
	Key * initialParent = keyDup (parentKey);

	ELEKTRA_LOG ("now in new kdbGet (%s)", keyName (parentKey));

	Split * split = splitNew ();

	if (!handle || !ks)
	{
		clearError (parentKey);
		ELEKTRA_SET_ERROR (37, parentKey, "handle or ks null pointer");
		goto error;
	}
	// GLOBAL: pregetstorage [init]
	// GLOBAL: pregetstorage [max once]
	// GLOBAL: pregetstorage [foreach]
	// GLOBAL: pregetstorage [deinit]

	elektraGlobalGet(handle, ks, parentKey, PREGETSTORAGE, MAXONCE);

	if (splitBuildup (split, handle, parentKey) == -1)
	{
		clearError (parentKey);
		ELEKTRA_SET_ERROR (38, parentKey, "error in splitBuildup");
		goto error;
	}

	// Check if a update is needed at all
	switch (elektraGetCheckUpdateNeeded (split, parentKey))
	{
	case 0: // We don't need an update so let's do nothing
		keySetName (parentKey, keyName (initialParent));
		if (handle && handle->globalPlugins[POSTGETSTORAGE])
		{
			handle->globalPlugins[POSTGETSTORAGE]->kdbGet (handle->globalPlugins[POSTGETSTORAGE], ks, parentKey);
		}
		splitUpdateFileName (split, handle, parentKey);
		keyDel (initialParent);
		splitDel (split);
		errno = errnosave;
		keyDel (oldError);
		return 0;
	case -1:
		goto error;
		// otherwise falltrough
	}

	// Appoint keys (some in the bypass)
	if (splitAppoint (split, handle, ks) == -1)
	{
		clearError (parentKey);
		ELEKTRA_SET_ERROR (38, parentKey, "error in splitAppoint");
		goto error;
	}

	if (handle->globalPlugins[POSTGETSTORAGE][MAXONCE] || handle->globalPlugins[POSTGETCLEANUP][MAXONCE])
	{
		clearError (parentKey);
		if (elektraGetDoUpdateWithGlobalHooks (NULL, split, NULL, parentKey, initialParent, FIRST) == -1)
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
			ELEKTRA_ADD_WARNING (108, parentKey, keyName (ksCurrent (ks)));
			// continue, because sizes are already updated
		}
		ksClear (ks);
		splitMerge (split, ks);

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
		if (elektraGetDoUpdate (split, parentKey) == -1)
		{
			goto error;
		}
		else
		{
			copyError (parentKey, oldError);
		}
		/* Now postprocess the updated keysets */
		if (splitGet (split, parentKey, handle) == -1)
		{
			ELEKTRA_ADD_WARNING (108, parentKey, keyName (ksCurrent (ks)));
			// continue, because sizes are already updated
		}
		/* We are finished, now just merge everything to returned */
		ksClear (ks);

		splitMerge (split, ks);
	}

	ksRewind (ks);

	keySetName (parentKey, keyName (initialParent));

	splitUpdateFileName (split, handle, parentKey);
	keyDel (initialParent);
	keyDel (oldError);
	splitDel (split);
	errno = errnosave;
	return 1;

error:
	keySetName (parentKey, keyName (initialParent));
	elektraGlobalGet(handle, ks, parentKey, POSTGETSTORAGE, MAXONCE);

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

	// GLOBAL: setresolver [init]
	// GLOBAL: setresolver [max once]

	// GLOBAL: presetstorage [init]
	// GLOBAL: presetstorage [max once]

	// GLOBAL: setstorage [init]
	// GLOBAL: setstorage [max once]

	for (size_t i = 0; i < split->size; i++)
	{
		for (size_t p = 0; p < COMMIT_PLUGIN; ++p)
		{
			int ret = 0; // last return value

			// if (p == RESOLVER_PLUGIN)
			// GLOBAL: setresolver [foreach]

			// if (p == STORAGE_PLUGIN)
			// GLOBAL: setstorage [foreach]

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
				// GLOBAL: presetstorage [foreach]
				if (hooks[PRESETSTORAGE][MAXONCE])
				{
					// the only place global presetstorage hooks can be executed
					ksRewind (split->keysets[i]);
					hooks[PRESETSTORAGE][MAXONCE]->kdbSet (hooks[PRESETSTORAGE][MAXONCE], split->keysets[i], parentKey);
				}
			}
			else if (p == (STORAGE_PLUGIN - 1))
			{
				if (hooks[PRESETCLEANUP][MAXONCE])
				{
					ksRewind (split->keysets[i]);
					hooks[PRESETCLEANUP][MAXONCE]->kdbSet (hooks[PRESETCLEANUP][MAXONCE], split->keysets[i], parentKey);
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
	// GLOBAL: setresolver [deinit]
	// GLOBAL: presetstorage [deinit]
	// GLOBAL: setstorage [deinit]
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
				ret = backend->setplugins[p]->kdbSet (backend->setplugins[p], split->keysets[i], parentKey);
				if (p == COMMIT_PLUGIN)
				{
					// name of non-temp file
					keySetString (split->parents[i], keyString (parentKey));
				}
			}

			if (ret == -1)
			{
				ELEKTRA_ADD_WARNING (80, parentKey, keyName (backend->mountpoint));
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
				ELEKTRA_ADD_WARNING (81, parentKey, keyName (backend->mountpoint));
			}
		}
	}
}


/** @brief Set keys in an atomic and universal way.
 *
 * @pre kdbGet() must be called before kdbSet():
 *    - initially (after kdbOpen())
 *    - after conflict errors in kdbSet().
 *
 * @pre The @p returned KeySet must be a valid KeySet, e.g. constructed
 *     with ksNew().
 *
 * @pre The @p parentKey Key must be a valid Key, e.g. constructed with
 *     keyNew().
 *
 * If you pass NULL on any parameter kdbSet() will fail immediately without doing anything.
 *
 * With @p parentKey you can give an hint which part of the given keyset
 * is of interest for you. Then you promise to only modify or
 * remove keys below this key. All others would be passed back
 * as they were retrieved by kdbGet().
 *
 * @par Errors
 * If some error occurs:
 * - kdbSet() will leave the KeySet's * internal cursor on the key that generated the error.
 * - Error information will be written into the metadata of
 *   the parent key.
 * - None of the keys are actually committed in this situation, i.e. no
 *   configuration file will be modified.
 *
 * In case of errors you should present the error message to the user and let the user decide what
 * to do. Possible solutions are:
 * - remove the problematic key and use kdbSet() again (for validation or type errors)
 * - change the value of the problematic key and use kdbSet() again (for validation errors)
 * - do a kdbGet() (for conflicts, i.e. error 30) and then
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
 * @param parentKey is used to add warnings and set an error
 *         information. Additionally, its name is an hint which keys
 *         should be committed (it is possible that more are changed).
 *           - cascading keys (starting with /) will set the path in all namespaces
 *           - / will commit all keys
 *           - metanames will be rejected (error 104)
 *           - empty/invalid (error 105)
 * @retval 1 on success
 * @retval 0 if nothing had to be done, no changes in KDB
 * @retval -1 on failure, no changes in KDB
 * @see keyNeedSync()
 * @see ksCurrent() contains the error key
 * @see kdbOpen() and kdbGet() that must be called first
 * @see kdbClose() that must be called afterwards
 * @ingroup kdb
 */
int kdbSet (KDB * handle, KeySet * ks, Key * parentKey)
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
		clearError (parentKey); // clear previous error to set new one
		ELEKTRA_SET_ERRORF (104, parentKey, "metakey with name \"%s\" passed to kdbSet", keyName (parentKey));
		keyDel (oldError);
		return -1;
	}

	if (ns == KEY_NS_EMPTY)
	{
		ELEKTRA_ADD_WARNING (105, parentKey, "invalid key name passed to kdbSet");
	}

	if (!handle || !ks)
	{
		clearError (parentKey); // clear previous error to set new one
		ELEKTRA_SET_ERROR (37, parentKey, "handle or ks null pointer");
		keyDel (oldError);
		return -1;
	}

	int errnosave = errno;
	Key * initialParent = keyDup (parentKey);

	ELEKTRA_LOG ("now in new kdbSet (%s)\n", keyName (parentKey));

	Split * split = splitNew ();
	Key * errorKey = 0;

	if (splitBuildup (split, handle, parentKey) == -1)
	{
		clearError (parentKey); // clear previous error to set new one
		ELEKTRA_SET_ERROR (38, parentKey, "error in splitBuildup");
		goto error;
	}

	// 1.) Search for syncbits
	int syncstate = splitDivide (split, handle, ks);
	if (syncstate == -1)
	{
		clearError (parentKey); // clear previous error to set new one
		ELEKTRA_SET_ERROR (8, parentKey, keyName (ksCurrent (ks)));
		goto error;
	}
	ELEKTRA_ASSERT (syncstate == 0 || syncstate == 1, "syncstate not 0 or 1, but %d", syncstate);

	// 2.) Search for changed sizes
	syncstate |= splitSync (split);
	ELEKTRA_ASSERT (syncstate <= 1, "syncstate not equal or below 1, but %d", syncstate);
	if (syncstate != 1)
	{
		/* No update is needed */
		keySetName (parentKey, keyName (initialParent));
		if (syncstate < 0) clearError (parentKey); // clear previous error to set new one
		if (syncstate == -1)
		{
			ELEKTRA_SET_ERROR (107, parentKey, "Assert failed: invalid namespace");
		}
		else if (syncstate < -1)
		{
			ELEKTRA_SET_ERROR (107, parentKey, keyName (split->parents[-syncstate - 2]));
		}
		keyDel (initialParent);
		splitDel (split);
		errno = errnosave;
		keyDel (oldError);
		return syncstate == 0 ? 0 : -1;
	}
	ELEKTRA_ASSERT (syncstate == 1, "syncstate not 1, but %d", syncstate);

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
	// GLOBAL: precommit [init]
	// GLOBAL: precommit [max once]
	// GLOBAL: precommit [foreach]
	// GLOBAL: precommit [deinit]
	elektraGlobalSet(handle, ks, parentKey, PRECOMMIT, MAXONCE);

	elektraSetCommit (split, parentKey);
	// GLOBAL: commit [init]
	// GLOBAL: commit [max once]
	// GLOBAL: commit [foreach]
	// GLOBAL: commit [deinit]

	splitUpdateSize (split);

	keySetName (parentKey, keyName (initialParent));
	// GLOBAL: postcommit [init]
	elektraGlobalSet(handle, ks, parentKey, POSTCOMMIT, INIT);

	// GLOBAL: postcommit [max once]
	// GLOBAL: postcommit [foreach]
	// GLOBAL: postcommit [deinit]
	elektraGlobalSet(handle, ks, parentKey, POSTCOMMIT, MAXONCE);

	for (size_t i = 0; i < ks->size; ++i)
	{
		// remove all flags from all keys
		clear_bit (ks->array[i]->flags, KEY_FLAG_SYNC);
	}

	keySetName (parentKey, keyName (initialParent));
	keyDel (initialParent);
	splitDel (split);

	keyDel (oldError);
	errno = errnosave;
	return 1;

error:
	keySetName (parentKey, keyName (initialParent));
	// GLOBAL: prerollback [init]
	// GLOBAL: prerollback [max once]
	// GLOBAL: prerollback [foreach]
	// GLOBAL: prerollback [deinit]
	elektraGlobalError (handle, ks, parentKey, PREROLLBACK, MAXONCE);

	elektraSetRollback (split, parentKey);
	// position for rollback seems superfluous / the same as postrollback
	// TODO: maybe a different postition was meant in the global-plugins proposal
	// GLOBAL: rollback [init]
	// GLOBAL: rollback [max once]
	// GLOBAL: rollback [foreach]
	// GLOBAL: rollback [deinit]

	if (errorKey)
	{
		Key * found = ksLookup (ks, errorKey, 0);
		if (!found)
		{
			ELEKTRA_ADD_WARNING (82, parentKey, keyName (errorKey));
		}
	}

	keySetName (parentKey, keyName (initialParent));
	// GLOBAL: postrollback [init]
	// GLOBAL: postrollback [max once]
	// GLOBAL: postrollback [foreach]
	// GLOBAL: postrollback [deinit]
	elektraGlobalError (handle, ks, parentKey, POSTROLLBACK, MAXONCE);

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
