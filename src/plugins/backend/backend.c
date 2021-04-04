

#include "backend.h"
#include "backendprivate.h"

#include <kdberrors.h>
#include <kdblogger.h>
#include <kdbprivate.h>

// FIXME: TESTS

int ELEKTRA_PLUGIN_FUNCTION (open) (Plugin * plugin, Key * errorKey ELEKTRA_UNUSED)
{
	BackendHandle * handle = elektraCalloc (sizeof (BackendHandle));
	elektraPluginSetData (plugin, handle);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static bool loadPlugin (Plugin ** pluginPtr, Plugin * thisPlugin, Key * pluginRefKey, Key * parentKey)
{
	if (pluginRefKey == NULL)
	{
		*pluginPtr = NULL;
		return true;
	}

	const char * pluginRef = keyString (pluginRefKey);
	*pluginPtr = elektraPluginFromMountpoint (thisPlugin, pluginRef);
	if (*pluginPtr == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (
			parentKey, "The plugin referenced in '%s%s' (value: '%s') could not be found. (Configuration of mountpoint: '%s')",
			keyName (parentKey), keyName (pluginRefKey), pluginRef, keyBaseName (parentKey));
		return false;
	}
	return true;
}

static bool loadPluginList (PluginList ** pluginListPtr, Plugin * thisPlugin, KeySet * definition, Key * pluginRefRoot, Key * parentKey)
{
	*pluginListPtr = NULL;
	PluginList * listEnd = NULL;

	for (elektraCursor end, i = ksFindHierarchy (definition, pluginRefRoot, &end); i < end; i++)
	{
		Key * cur = ksAtCursor (definition, i);
		if (keyIsDirectlyBelow (pluginRefRoot, cur) != 1)
		{
			continue;
		}

		Plugin * plugin;
		if (!loadPlugin (&plugin, thisPlugin, cur, parentKey))
		{
			return false;
		}

		PluginList * element = elektraMalloc (sizeof (PluginList));
		element->plugin = plugin;
		element->next = NULL;

		if (listEnd == NULL)
		{
			*pluginListPtr = element;
		}
		else
		{
			listEnd->next = element;
		}
		listEnd = element;
	}

	return true;
}

int ELEKTRA_PLUGIN_FUNCTION (init) (Plugin * plugin, KeySet * definition, Key * parentKey)
{
	Key * pathKey = ksLookupByName (definition, "/path", 0);
	const char * path = keyString (pathKey);
	if (pathKey == NULL || strlen (path) == 0)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (parentKey,
						 "You must set '%s/path' to a non-empty value. (Configuration of mountpoint: %s)",
						 keyName (parentKey), keyBaseName (parentKey));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Key * getResolverRefKey = ksLookupByName (definition, "/positions/get/resolver", 0);
	const char * getResolverRef = keyString (getResolverRefKey);
	bool hasGetResolver = getResolverRefKey != NULL && strlen (getResolverRef) > 0;

	Key * setResolverRefKey = ksLookupByName (definition, "/positions/set/resolver", 0);
	const char * setResolverRef = keyString (setResolverRefKey);
	bool hasSetResolver = setResolverRefKey != NULL && strlen (setResolverRef) > 0;

	if (hasGetResolver)
	{
		if (path[0] == '/' && ksLookupByName (definition, "/path/absolute", 0) == NULL)
		{
			ELEKTRA_ADD_INSTALLATION_WARNINGF (
				parentKey,
				"You configured a resolver. That absolute path in '%s/path' might not be used as-is. If the configuration "
				"is intentional, set '%s/path/absolute' to any value to silence this warning. (Configuration of "
				"mountpoint: '%s')",
				keyName (parentKey), keyName (parentKey), keyBaseName (parentKey));
		}
	}
	else
	{
		if (hasSetResolver)
		{
			ELEKTRA_SET_INSTALLATION_ERRORF (
				parentKey,
				"If '%s/positions/set/resolver' is set, '%s/positions/get/resolver' you must also set to a "
				"non-empty value. (Configuration of mountpoint: '%s')",
				keyName (parentKey), keyName (parentKey), keyBaseName (parentKey));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (path[0] != '/')
		{
			ELEKTRA_SET_INSTALLATION_ERRORF (parentKey,
							 "If no resolver is configured, '%s/path' you must set to an absolute path. "
							 "(Configuration of mountpoint: '%s')",
							 keyName (parentKey), keyBaseName (parentKey));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	// TODO (Q): support for read-write to absolute path?
	bool readOnly = !hasSetResolver;

	BackendHandle * handle = elektraPluginGetData (plugin);
	handle->path = elektraStrDup (path);

	handle->getPositions.resolver = hasGetResolver ? elektraPluginFromMountpoint (plugin, getResolverRef) : NULL;
	if (!loadPluginList (&handle->getPositions.prestorage, plugin, definition,
			     ksLookupByName (definition, "/positions/get/prestorage", 0), parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPlugin (&handle->getPositions.storage, plugin, ksLookupByName (definition, "/positions/get/storage", 0), parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPluginList (&handle->getPositions.poststorage, plugin, definition,
			     ksLookupByName (definition, "/positions/get/poststorage", 0), parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	handle->setPositions.resolver = hasSetResolver ? elektraPluginFromMountpoint (plugin, getResolverRef) : NULL;

	if (!loadPluginList (&handle->setPositions.prestorage, plugin, definition,
			     ksLookupByName (definition, "/positions/set/prestorage", 0), parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPlugin (&handle->setPositions.storage, plugin, ksLookupByName (definition, "/positions/set/storage", 0), parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPluginList (&handle->setPositions.poststorage, plugin, definition,
			     ksLookupByName (definition, "/positions/set/poststorage", 0), parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPluginList (&handle->setPositions.precommit, plugin, definition,
			     ksLookupByName (definition, "/positions/set/precommit", 0), parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPlugin (&handle->setPositions.commit, plugin, ksLookupByName (definition, "/positions/set/commit", 0), parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPluginList (&handle->setPositions.postcommit, plugin, definition,
			     ksLookupByName (definition, "/positions/set/postcommit", 0), parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPluginList (&handle->setPositions.prerollback, plugin, definition,
			     ksLookupByName (definition, "/positions/set/prerollback", 0), parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPlugin (&handle->setPositions.rollback, plugin, ksLookupByName (definition, "/positions/set/rollback", 0), parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPluginList (&handle->setPositions.postrollback, plugin, definition,
			     ksLookupByName (definition, "/positions/set/postrollback", 0), parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (handle->getPositions.storage == NULL && ksLookupByName (definition, "/positions/get/storage/omit", 0) != NULL)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (
			parentKey,
			"No storage plugin defined for kdbGet(). You probably forgot to set '%s/positions/get/storage'. If the "
			"configuration is intentional, you can silence this warning by setting '%s/positions/get/storage/omit' to any "
			"value. (Configuration of mountpoint: '%s')",
			keyName (parentKey), keyName (parentKey), keyBaseName (parentKey));
	}

	if (readOnly)
	{
		if (handle->setPositions.prestorage != NULL || handle->setPositions.storage != NULL ||
		    handle->setPositions.poststorage != NULL || handle->setPositions.precommit != NULL ||
		    handle->setPositions.commit != NULL || handle->setPositions.postcommit != NULL ||
		    handle->setPositions.prerollback != NULL || handle->setPositions.rollback != NULL ||
		    handle->setPositions.postrollback != NULL)
		{
			ELEKTRA_ADD_INSTALLATION_WARNINGF (
				parentKey,
				"The mountpoint '%s' is configured as read-only (no set-resolver configured), but there are some plugins "
				"configured for set-positions below '%s/positions/set'. These plugins will be ignored. Remove them from "
				"the configuration to remove this warning. (Configuration of mountpoint: '%s')",
				keyBaseName (parentKey), keyName (parentKey), keyBaseName (parentKey));
		}
	}
	else
	{
		if (handle->setPositions.storage == NULL && ksLookupByName (definition, "/positions/set/storage/omit", 0) != NULL)
		{
			ELEKTRA_ADD_INSTALLATION_WARNINGF (
				parentKey,
				"You defined a resolver, but no storage plugin defined for kdbSet(). You probably forgot to set "
				"'%s/positions/set/storage'. If you wanted to create a read-only mountpoint remove "
				"'%s/positions/set/resolver'. If the configuration is intentional, you can silence this "
				"warning by setting '%s/positions/set/storage/omit' to any value. (Configuration of mountpoint: '%s')",
				keyName (parentKey), keyName (parentKey), keyName (parentKey), keyBaseName (parentKey));
		}
	}

	if (handle->setPositions.resolver != handle->setPositions.commit &&
	    ksLookupByName (definition, "/positions/set/commit/differs", 0) != NULL)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (
			parentKey,
			"The resolver plugin ('%s') differs from the commit plugin ('%s'). This is a non-standard configuration. Normally, "
			"'%s/positions/set/resolver' and '%s/positions/set/commit' should have the same value. If the configuration is "
			"intentional, you can silence this warning by setting '%s/positions/set/commit/differs' to any value. "
			"(Configuration of mountpoint: '%s')",
			handle->setPositions.resolver->name, handle->setPositions.commit->name, keyName (parentKey), keyName (parentKey),
			keyName (parentKey), keyBaseName (parentKey));
	}

	if (handle->setPositions.resolver != handle->setPositions.rollback &&
	    ksLookupByName (definition, "/positions/set/rollback/differs", 0) != NULL)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (
			parentKey,
			"The resolver plugin ('%s') differs from the rollback plugin ('%s'). This is a non-standard configuration. "
			"Normally, '%s/positions/set/resolver' and '%s/positions/set/rollback' should have the same value. If the "
			"configuration is intentional, you can silence this warning by setting '%s/positions/set/rollback/differs' to any "
			"value. (Configuration of mountpoint: '%s')",
			handle->setPositions.resolver->name, handle->setPositions.rollback->name, keyName (parentKey), keyName (parentKey),
			keyName (parentKey), keyBaseName (parentKey));
	}

	return readOnly ? ELEKTRA_PLUGIN_STATUS_NO_UPDATE : ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static inline void addGenericError (Key * key, const char * function, const char * plugin)
{
	ELEKTRA_SET_INTERFACE_ERRORF (key,
				      "The %s() function of the plugin '%s' returned ELEKTRA_PLUGIN_STATUS_ERROR, but did not actually set "
				      "an error. If you are the author of this plugin, please add a proper error to the parentKey.",
				      function, plugin);
}

static bool runPluginGet (Plugin * plugin, KeySet * ks, Key * parentKey)
{
	// TODO: provide way to access kdbGet and name without kdbprivate.h
	if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		if (keyGetMeta (parentKey, "error") == NULL)
		{
			addGenericError (parentKey, "kdbGet", plugin->name);
		}
		return false;
	}
	return true;
}

static bool runPluginListGet (PluginList * plugins, KeySet * ks, Key * parentKey)
{
	for (PluginList * cur = plugins; cur != NULL; cur = cur->next)
	{
		if (!runPluginGet (cur->plugin, ks, parentKey))
		{
			return false;
		}
	}
	return true;
}

int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * plugin, KeySet * ks, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/backend"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system:/elektra/modules/backend", KEY_VALUE, "backend plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/backend/exports", KEY_END),
			keyNew ("system:/elektra/modules/backend/exports/open", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (open), KEY_END),
			keyNew ("system:/elektra/modules/backend/exports/init", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (init), KEY_END),
			keyNew ("system:/elektra/modules/backend/exports/get", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (get), KEY_END),
			keyNew ("system:/elektra/modules/backend/exports/set", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (set), KEY_END),
			keyNew ("system:/elektra/modules/backend/exports/commit", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (commit), KEY_END),
			keyNew ("system:/elektra/modules/backend/exports/error", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (error), KEY_END),
			keyNew ("system:/elektra/modules/backend/exports/close", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (close), KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/backend/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (ks, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	BackendHandle * handle = elektraPluginGetData (plugin);

	if (handle == NULL)
	{
		ELEKTRA_SET_INTERNAL_ERROR (parentKey,
					    "Internal plugin data was NULL. Please report this bug at https://issues.libelektra.org.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	const char * phase = elektraPluginGetPhase (plugin);
	if (strcmp (phase, KDB_GET_PHASE_RESOLVER) == 0)
	{
		keySetString (parentKey, handle->path);

		if (handle->getPositions.resolver == NULL)
		{
			// no resolver configured -> path is absolute
			// TODO (Q): check mtime to determine up date needed?
			return ELEKTRA_PLUGIN_STATUS_SUCCESS;
		}

		return runPluginGet (handle->getPositions.resolver, ks, parentKey);
	}
	else if (strcmp (phase, KDB_GET_PHASE_CACHECHECK) == 0)
	{
		// FIXME: implement
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}
	else if (strcmp (phase, KDB_GET_PHASE_PRE_STORAGE) == 0)
	{
		return runPluginListGet (handle->getPositions.prestorage, ks, parentKey);
	}
	else if (strcmp (phase, KDB_GET_PHASE_STORAGE) == 0)
	{
		return runPluginGet (handle->getPositions.storage, ks, parentKey);
	}
	else if (strcmp (phase, KDB_GET_PHASE_POST_STORAGE) == 0)
	{
		return runPluginListGet (handle->getPositions.poststorage, ks, parentKey);
	}
	else
	{
		ELEKTRA_SET_INTERNAL_ERRORF (
			parentKey, "Unknown phase of kdbGet(): %s\n Please report this bug at https://issues.libelektra.org.", phase);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static bool runPluginSet (Plugin * plugin, KeySet * ks, Key * parentKey)
{
	// TODO: provide way to access kdbSet and name without kdbprivate.h
	if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		if (keyGetMeta (parentKey, "error") == NULL)
		{
			addGenericError (parentKey, "kdbSet", plugin->name);
		}
		return false;
	}
	return true;
}

static bool runPluginListSet (PluginList * plugins, KeySet * ks, Key * parentKey)
{
	for (PluginList * cur = plugins; cur != NULL; cur = cur->next)
	{
		if (!runPluginSet (cur->plugin, ks, parentKey))
		{
			return false;
		}
	}
	return true;
}

int ELEKTRA_PLUGIN_FUNCTION (set) (Plugin * plugin, KeySet * ks, Key * parentKey)
{
	BackendHandle * handle = elektraPluginGetData (plugin);
	if (handle == NULL)
	{
		ELEKTRA_SET_INTERNAL_ERROR (parentKey,
					    "Internal plugin data was NULL. Please report this bug at https://issues.libelektra.org.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	const char * phase = elektraPluginGetPhase (plugin);
	if (strcmp (phase, KDB_SET_PHASE_RESOLVER) == 0)
	{
		keySetString (parentKey, handle->path);

		if (handle->setPositions.resolver == NULL)
		{
			// no resolver configured -> path is absolute
			// TODO (Q): support for read-write?
			ELEKTRA_SET_INTERNAL_ERROR (
				parentKey,
				"No resolver, but initialized as read-write. Please report this bug at https://issues.libelektra.org.");
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		return runPluginSet (handle->setPositions.resolver, ks, parentKey);
	}
	else if (strcmp (phase, KDB_SET_PHASE_PRE_COMMIT) == 0)
	{
		return runPluginListSet (handle->setPositions.precommit, ks, parentKey);
	}
	else if (strcmp (phase, KDB_SET_PHASE_PRE_COMMIT) == 0)
	{
		return runPluginSet (handle->setPositions.commit, ks, parentKey);
	}
	else if (strcmp (phase, KDB_SET_PHASE_PRE_COMMIT) == 0)
	{
		return runPluginListSet (handle->setPositions.postcommit, ks, parentKey);
	}
	else
	{
		ELEKTRA_SET_INTERNAL_ERRORF (
			parentKey, "Unknown phase of kdbSet(): %s\n Please report this bug at https://issues.libelektra.org.", phase);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static bool runPluginCommit (Plugin * plugin, KeySet * ks, Key * parentKey)
{
	// TODO: provide way to access kdbCommit and name without kdbprivate.h
	if (plugin->kdbCommit (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		if (keyGetMeta (parentKey, "error") == NULL)
		{
			addGenericError (parentKey, "kdbCommit", plugin->name);
		}
		return false;
	}
	return true;
}

static bool runPluginListCommit (PluginList * plugins, KeySet * ks, Key * parentKey)
{
	for (PluginList * cur = plugins; cur != NULL; cur = cur->next)
	{
		if (!runPluginCommit (cur->plugin, ks, parentKey))
		{
			return false;
		}
	}
	return true;
}

int ELEKTRA_PLUGIN_FUNCTION (commit) (Plugin * plugin, KeySet * ks, Key * parentKey)
{
	BackendHandle * handle = elektraPluginGetData (plugin);
	if (handle == NULL)
	{
		ELEKTRA_SET_INTERNAL_ERROR (parentKey,
					    "Internal plugin data was NULL. Please report this bug at https://issues.libelektra.org.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	const char * phase = elektraPluginGetPhase (plugin);
	if (strcmp (phase, KDB_SET_PHASE_PRE_COMMIT) == 0)
	{
		return runPluginListCommit (handle->setPositions.precommit, ks, parentKey);
	}
	else if (strcmp (phase, KDB_SET_PHASE_PRE_COMMIT) == 0)
	{
		return runPluginCommit (handle->setPositions.commit, ks, parentKey);
	}
	else if (strcmp (phase, KDB_SET_PHASE_PRE_COMMIT) == 0)
	{
		return runPluginListCommit (handle->setPositions.postcommit, ks, parentKey);
	}
	else
	{
		ELEKTRA_SET_INTERNAL_ERRORF (
			parentKey, "Unknown phase of kdbSet(): %s\n Please report this bug at https://issues.libelektra.org.", phase);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static bool runPluginError (Plugin * plugin, KeySet * ks, Key * parentKey)
{
	// TODO: provide way to access kdbError and name without kdbprivate.h
	if (plugin->kdbError (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		if (keyGetMeta (parentKey, "error") == NULL)
		{
			addGenericError (parentKey, "kdbError", plugin->name);
		}
		return false;
	}
	return true;
}

static bool runPluginListError (PluginList * plugins, KeySet * ks, Key * parentKey)
{
	for (PluginList * cur = plugins; cur != NULL; cur = cur->next)
	{
		if (!runPluginError (cur->plugin, ks, parentKey))
		{
			return false;
		}
	}
	return true;
}

int ELEKTRA_PLUGIN_FUNCTION (error) (Plugin * plugin, KeySet * ks, Key * parentKey)
{
	BackendHandle * handle = elektraPluginGetData (plugin);
	if (handle == NULL)
	{
		ELEKTRA_SET_INTERNAL_ERROR (parentKey,
					    "Internal plugin data was NULL. Please report this bug at https://issues.libelektra.org.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	const char * phase = elektraPluginGetPhase (plugin);
	if (strcmp (phase, KDB_SET_PHASE_PRE_ROLLBACK) == 0)
	{
		return runPluginListError (handle->setPositions.prerollback, ks, parentKey);
	}
	else if (strcmp (phase, KDB_SET_PHASE_PRE_ROLLBACK) == 0)
	{
		return runPluginError (handle->setPositions.rollback, ks, parentKey);
	}
	else if (strcmp (phase, KDB_SET_PHASE_PRE_ROLLBACK) == 0)
	{
		return runPluginListError (handle->setPositions.postrollback, ks, parentKey);
	}
	else
	{
		ELEKTRA_SET_INTERNAL_ERRORF (
			parentKey, "Unknown phase of kdbSet(): %s\n Please report this bug at https://issues.libelektra.org.", phase);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static void freePluginList (PluginList ** pluginsPtr)
{
	PluginList * cur = *pluginsPtr;
	while (cur != NULL)
	{
		PluginList * next = cur->next;
		elektraFree (cur);
		cur = next;
	}
	*pluginsPtr = NULL;
}

int ELEKTRA_PLUGIN_FUNCTION (close) (Plugin * plugin, Key * errorKey ELEKTRA_UNUSED)
{
	BackendHandle * handle = elektraPluginGetData (plugin);
	if (handle == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	if (handle->path != NULL)
	{
		elektraFree (handle->path);
	}

	freePluginList (&handle->getPositions.prestorage);
	freePluginList (&handle->getPositions.poststorage);
	freePluginList (&handle->setPositions.prestorage);
	freePluginList (&handle->setPositions.poststorage);
	freePluginList (&handle->setPositions.precommit);
	freePluginList (&handle->setPositions.postcommit);
	freePluginList (&handle->setPositions.prerollback);
	freePluginList (&handle->setPositions.postrollback);

	elektraFree (handle);
	elektraPluginSetData (plugin, NULL);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("backend",
		ELEKTRA_PLUGIN_OPEN, &ELEKTRA_PLUGIN_FUNCTION (open),
		ELEKTRA_PLUGIN_INIT, &ELEKTRA_PLUGIN_FUNCTION (init),
		ELEKTRA_PLUGIN_GET, &ELEKTRA_PLUGIN_FUNCTION (get),
		ELEKTRA_PLUGIN_SET, &ELEKTRA_PLUGIN_FUNCTION (set),
		ELEKTRA_PLUGIN_COMMIT, &ELEKTRA_PLUGIN_FUNCTION (commit),
		ELEKTRA_PLUGIN_ERROR, &ELEKTRA_PLUGIN_FUNCTION (error),
		ELEKTRA_PLUGIN_CLOSE, &ELEKTRA_PLUGIN_FUNCTION (close),
	ELEKTRA_PLUGIN_END);
	// clang-format on
}
