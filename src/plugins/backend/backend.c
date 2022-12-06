

#include "backend.h"
#include "backendprivate.h"

#include <kdberrors.h>
#include <kdblogger.h>
#include <elektra/kdbprivate.h>

int ELEKTRA_PLUGIN_FUNCTION (open) (Plugin * plugin, Key * errorKey ELEKTRA_UNUSED)
{
	BackendHandle * handle = elektraCalloc (sizeof (BackendHandle));
	elektraPluginSetData (plugin, handle);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

enum PluginType
{
	PLUGIN_TYPE_GET,
	PLUGIN_TYPE_SET,
	PLUGIN_TYPE_COMMIT,
	PLUGIN_TYPE_ERROR,
};

static bool loadPlugin (Plugin ** pluginPtr, Plugin * thisPlugin, Key * pluginRefKey, enum PluginType type, Key * parentKey)
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
		ELEKTRA_SET_INSTALLATION_ERRORF (parentKey,
						 "The plugin referenced in '%s%s' (value: '%s', refering to '%s/plugins/%s') could not be "
						 "found. (Configuration of mountpoint: '%s')",
						 keyName (parentKey), keyName (pluginRefKey), pluginRef, keyName (parentKey), pluginRef,
						 keyBaseName (parentKey));
		return false;
	}

	if (type == PLUGIN_TYPE_GET && (*pluginPtr)->kdbGet == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (parentKey,
						 "The plugin '%s' was referenced in a kdbGet() position ('%s%s'), but does not implement "
						 "kdbGet(). (Configuration of mountpoint: '%s')",
						 (*pluginPtr)->name, keyName (parentKey), keyName (pluginRefKey), keyBaseName (parentKey));
		return false;
	}

	if (type == PLUGIN_TYPE_SET && (*pluginPtr)->kdbSet == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (parentKey,
						 "The plugin '%s' was referenced in a kdbSet() position ('%s%s'), but does not implement "
						 "kdbSet(). (Configuration of mountpoint: '%s')",
						 (*pluginPtr)->name, keyName (parentKey), keyName (pluginRefKey), keyBaseName (parentKey));
		return false;
	}

	if (type == PLUGIN_TYPE_COMMIT && (*pluginPtr)->kdbCommit == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (parentKey,
						 "The plugin '%s' was referenced in a kdbCommit() position ('%s%s'), but does not "
						 "implement kdbCommit(). (Configuration of mountpoint: '%s')",
						 (*pluginPtr)->name, keyName (parentKey), keyName (pluginRefKey), keyBaseName (parentKey));
		return false;
	}

	if (type == PLUGIN_TYPE_ERROR && (*pluginPtr)->kdbError == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (parentKey,
						 "The plugin '%s' was referenced in a kdbError() position ('%s%s'), but does not implement "
						 "kdbError(). (Configuration of mountpoint: '%s')",
						 (*pluginPtr)->name, keyName (parentKey), keyName (pluginRefKey), keyBaseName (parentKey));
		return false;
	}

	return true;
}

static bool loadPluginList (PluginList ** pluginListPtr, Plugin * thisPlugin, KeySet * definition, const char * pluginRefRootName,
			    enum PluginType type, Key * parentKey)
{
	Key * pluginRefRoot = keyNew (pluginRefRootName, KEY_END);

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
		if (!loadPlugin (&plugin, thisPlugin, cur, type, parentKey))
		{
			keyDel (pluginRefRoot);
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

	keyDel (pluginRefRoot);
	return true;
}

int ELEKTRA_PLUGIN_FUNCTION (init) (Plugin * plugin, KeySet * definition, Key * parentKey)
{
	if (keyGetNamespace (parentKey) == KEY_NS_PROC)
	{
		BackendHandle * handle = elektraPluginGetData (plugin);
		handle->path = elektraStrDup ("");

		if (!loadPluginList (&handle->getPositions.poststorage, plugin, definition, "system:/positions/get/poststorage",
				     PLUGIN_TYPE_GET, parentKey))
		{
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}

	Key * pathKey = ksLookupByName (definition, "system:/path", 0);
	const char * path = keyString (pathKey);
	if (pathKey == NULL || strlen (path) == 0)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (
			parentKey, "You must set '%s/definition/path' to a non-empty value. (Configuration of mountpoint: %s)",
			keyName (parentKey), keyBaseName (parentKey));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	BackendHandle * handle = elektraPluginGetData (plugin);
	handle->path = elektraStrDup (path);

	// load get plugins
	if (!loadPlugin (&handle->getPositions.resolver, plugin, ksLookupByName (definition, "system:/positions/get/resolver", 0),
			 PLUGIN_TYPE_GET, parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPluginList (&handle->getPositions.prestorage, plugin, definition, "system:/positions/get/prestorage", PLUGIN_TYPE_GET,
			     parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPlugin (&handle->getPositions.storage, plugin, ksLookupByName (definition, "system:/positions/get/storage", 0),
			 PLUGIN_TYPE_GET, parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPluginList (&handle->getPositions.poststorage, plugin, definition, "system:/positions/get/poststorage", PLUGIN_TYPE_GET,
			     parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// load set plugins
	if (!loadPlugin (&handle->setPositions.resolver, plugin, ksLookupByName (definition, "system:/positions/set/resolver", 0),
			 PLUGIN_TYPE_SET, parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPluginList (&handle->setPositions.prestorage, plugin, definition, "system:/positions/set/prestorage", PLUGIN_TYPE_SET,
			     parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPlugin (&handle->setPositions.storage, plugin, ksLookupByName (definition, "system:/positions/set/storage", 0),
			 PLUGIN_TYPE_SET, parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPluginList (&handle->setPositions.poststorage, plugin, definition, "system:/positions/set/poststorage", PLUGIN_TYPE_SET,
			     parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// load commit plugins
	if (!loadPluginList (&handle->setPositions.precommit, plugin, definition, "system:/positions/set/precommit", PLUGIN_TYPE_COMMIT,
			     parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPlugin (&handle->setPositions.commit, plugin, ksLookupByName (definition, "system:/positions/set/commit", 0),
			 PLUGIN_TYPE_COMMIT, parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPluginList (&handle->setPositions.postcommit, plugin, definition, "system:/positions/set/postcommit", PLUGIN_TYPE_COMMIT,
			     parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// load error plugins
	if (!loadPluginList (&handle->setPositions.prerollback, plugin, definition, "system:/positions/set/prerollback", PLUGIN_TYPE_ERROR,
			     parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPlugin (&handle->setPositions.rollback, plugin, ksLookupByName (definition, "system:/positions/set/rollback", 0),
			 PLUGIN_TYPE_ERROR, parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPluginList (&handle->setPositions.postrollback, plugin, definition, "system:/positions/set/postrollback",
			     PLUGIN_TYPE_ERROR, parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (handle->getPositions.resolver != NULL)
	{
		if (path[0] == '/' && ksLookupByName (definition, "system:/path/absolute", 0) == NULL)
		{
			ELEKTRA_ADD_INSTALLATION_WARNINGF (
				parentKey,
				"You configured a resolver. The absolute path in '%s/definition/path' might not be used as-is. If the "
				"configuration is intentional, set '%s/definition/path/absolute' to any value to silence this warning. "
				"(Configuration of mountpoint: '%s')",
				keyName (parentKey), keyName (parentKey), keyBaseName (parentKey));
		}
	}
	else
	{
		if (handle->setPositions.resolver != NULL)
		{
			ELEKTRA_SET_INSTALLATION_ERRORF (
				parentKey,
				"To set '%s/definition/positions/set/resolver', you must also set '%s/definition/positions/get/resolver' "
				"to a non-empty value. (Configuration of mountpoint: '%s')",
				keyName (parentKey), keyName (parentKey), keyBaseName (parentKey));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (path[0] != '/')
		{
			ELEKTRA_SET_INSTALLATION_ERRORF (
				parentKey,
				"If no resolver is configured, '%s/definition/path' you must set to an absolute path. "
				"(Configuration of mountpoint: '%s')",
				keyName (parentKey), keyBaseName (parentKey));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	bool readOnly = handle->setPositions.resolver == NULL;

	if (handle->getPositions.storage == NULL && ksLookupByName (definition, "system:/positions/get/storage/omit", 0) == NULL)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (
			parentKey,
			"No storage plugin defined for kdbGet(). You probably forgot to set '%s/definition/positions/get/storage'. If the "
			"configuration is intentional, you can silence this warning by setting '%s/definition/positions/get/storage/omit' "
			"to any value. (Configuration of mountpoint: '%s')",
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
				"configured for set-positions below '%s/definition/positions/set'. These plugins will be ignored. Remove "
				"them from the configuration to remove this warning. (Configuration of mountpoint: '%s')",
				keyBaseName (parentKey), keyName (parentKey), keyBaseName (parentKey));
		}
	}
	else
	{
		if (handle->setPositions.storage == NULL)
		{
			ELEKTRA_SET_INSTALLATION_ERRORF (
				parentKey,
				"You defined a set-resolver plugin, but no storage plugin for kdbSet(). You probably forgot to set "
				"'%s/definition/positions/set/storage'. (Configuration of mountpoint: '%s')",
				keyName (parentKey), keyBaseName (parentKey));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (handle->setPositions.commit == NULL)
		{
			ELEKTRA_SET_INSTALLATION_ERRORF (
				parentKey,
				"You defined a set-resolver plugin, but no commit plugin for kdbSet(). In most cases the same plugin is "
				"used as resolver and commit. To enable this configuration, set '%s/definition/positions/set/commit' to "
				"'%s', i.e. to the same value as '%s/definition/positions/set/resolver'. (Configuration of mountpoint: "
				"'%s')",
				keyName (parentKey), handle->setPositions.resolver->name, keyName (parentKey), keyBaseName (parentKey));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (handle->setPositions.rollback == NULL)
		{
			ELEKTRA_SET_INSTALLATION_ERRORF (
				parentKey,
				"You defined a set-resolver and commit plugin, but no rollback plugin for kdbSet(). In most cases the same "
				"plugin is used as commit and rollback. To enable this configuration, set "
				"'%s/definition/positions/set/rollback' to '%s', i.e. to the same value as "
				"'%s/definition/positions/set/commit'. (Configuration of mountpoint: '%s')",
				keyName (parentKey), handle->setPositions.resolver->name, keyName (parentKey), keyBaseName (parentKey));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (handle->getPositions.resolver != handle->setPositions.resolver &&
		    ksLookupByName (definition, "system:/positions/set/resolver/differs", 0) == NULL)
		{
			ELEKTRA_ADD_INSTALLATION_WARNINGF (
				parentKey,
				"The resolver plugin for kdbSet() ('%s') differs from the resolver plugin for kdbGet() ('%s'). This is a "
				"non-standard configuration. Normally, '%s/definition/positions/get/resolver' and "
				"'%s/definition/positions/set/resolver' should have the same value. If the configuration is intentional, "
				"you can "
				"silence this warning by setting '%s/definition/positions/set/resolver/differs' to any value. "
				"(Configuration of "
				"mountpoint: '%s')",
				handle->getPositions.resolver->name, handle->setPositions.resolver->name, keyName (parentKey),
				keyName (parentKey), keyName (parentKey), keyBaseName (parentKey));
		}

		if (handle->setPositions.resolver != handle->setPositions.commit &&
		    ksLookupByName (definition, "system:/positions/set/commit/differs", 0) == NULL)
		{
			ELEKTRA_ADD_INSTALLATION_WARNINGF (
				parentKey,
				"The resolver plugin ('%s') differs from the commit plugin ('%s'). This is a non-standard configuration. "
				"Normally, "
				"'%s/definition/positions/set/resolver' and '%s/definition/positions/set/commit' should have the same "
				"value. If "
				"the configuration is intentional, you can silence this warning by setting "
				"'%s/definition/positions/set/commit/differs' to any value. (Configuration of mountpoint: '%s')",
				handle->setPositions.resolver->name, handle->setPositions.commit->name, keyName (parentKey),
				keyName (parentKey), keyName (parentKey), keyBaseName (parentKey));
		}

		if (handle->setPositions.resolver != handle->setPositions.rollback &&
		    ksLookupByName (definition, "system:/positions/set/rollback/differs", 0) == NULL)
		{
			ELEKTRA_ADD_INSTALLATION_WARNINGF (
				parentKey,
				"The resolver plugin ('%s') differs from the rollback plugin ('%s'). This is a non-standard configuration. "
				"Normally, '%s/definition/positions/set/resolver' and '%s/definition/positions/set/rollback' should have "
				"the same "
				"value. If the configuration is intentional, you can silence this warning by setting "
				"'%s/definition/positions/set/rollback/differs' to any value. (Configuration of mountpoint: '%s')",
				handle->setPositions.resolver->name, handle->setPositions.rollback->name, keyName (parentKey),
				keyName (parentKey), keyName (parentKey), keyBaseName (parentKey));
		}
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

static int runPluginGet (Plugin * plugin, KeySet * ks, Key * parentKey)
{
	// TODO: provide way to access kdbGet and name without kdbprivate.h
	ksRewind (ks);
	int ret = plugin->kdbGet (plugin, ks, parentKey);
	if (ret == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		if (keyGetMeta (parentKey, "error") == NULL)
		{
			addGenericError (parentKey, "kdbGet", plugin->name);
		}
	}
	return ret;
}

static int runPluginListGet (PluginList * plugins, KeySet * ks, Key * parentKey)
{
	for (PluginList * cur = plugins; cur != NULL; cur = cur->next)
	{
		if (runPluginGet (cur->plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
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

	ElektraKdbPhase phase = elektraPluginGetPhase (plugin);
	switch (phase)
	{
	case ELEKTRA_KDB_GET_PHASE_RESOLVER:
		keySetString (parentKey, handle->path);

		if (handle->getPositions.resolver == NULL)
		{
			// no resolver configured -> path is absolute
			// TODO [new_backend]: check mtime to determine up date needed?
			return ELEKTRA_PLUGIN_STATUS_SUCCESS;
		}

		return runPluginGet (handle->getPositions.resolver, ks, parentKey);
	case ELEKTRA_KDB_GET_PHASE_CACHECHECK:
		// TODO [new_backend]: implement cache
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	case ELEKTRA_KDB_GET_PHASE_PRE_STORAGE:
		return runPluginListGet (handle->getPositions.prestorage, ks, parentKey);
	case ELEKTRA_KDB_GET_PHASE_STORAGE:
		return runPluginGet (handle->getPositions.storage, ks, parentKey);
	case ELEKTRA_KDB_GET_PHASE_POST_STORAGE:
		return runPluginListGet (handle->getPositions.poststorage, ks, parentKey);
	default:
		ELEKTRA_SET_INTERNAL_ERRORF (
			parentKey, "Unknown phase of kdbGet(): %02x\n Please report this bug at https://issues.libelektra.org.", phase);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
}

static int runPluginSet (Plugin * plugin, KeySet * ks, Key * parentKey)
{
	// TODO: provide way to access kdbSet and name without kdbprivate.h
	ksRewind (ks);
	int ret = plugin->kdbSet (plugin, ks, parentKey);
	if (ret == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		if (keyGetMeta (parentKey, "error") == NULL)
		{
			addGenericError (parentKey, "kdbSet", plugin->name);
		}
	}
	return ret;
}

static int runPluginListSet (PluginList * plugins, KeySet * ks, Key * parentKey)
{
	for (PluginList * cur = plugins; cur != NULL; cur = cur->next)
	{
		if (runPluginSet (cur->plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
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

	ElektraKdbPhase phase = elektraPluginGetPhase (plugin);
	switch (phase)
	{
	case ELEKTRA_KDB_SET_PHASE_RESOLVER:
		keySetString (parentKey, handle->path);

		if (handle->setPositions.resolver == NULL)
		{
			// no resolver configured -> path is absolute
			ELEKTRA_SET_INTERNAL_ERROR (
				parentKey,
				"No resolver, but initialized as read-write. Please report this bug at https://issues.libelektra.org.");
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		return runPluginSet (handle->setPositions.resolver, ks, parentKey);
	case ELEKTRA_KDB_SET_PHASE_PRE_STORAGE:
		return runPluginListSet (handle->setPositions.prestorage, ks, parentKey);
	case ELEKTRA_KDB_SET_PHASE_STORAGE:
		return runPluginSet (handle->setPositions.storage, ks, parentKey);
	case ELEKTRA_KDB_SET_PHASE_POST_STORAGE:
		return runPluginListSet (handle->setPositions.poststorage, ks, parentKey);
	default:

		ELEKTRA_SET_INTERNAL_ERRORF (
			parentKey, "Unknown phase of kdbSet(): %02x\n Please report this bug at https://issues.libelektra.org.", phase);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
}

static int runPluginCommit (Plugin * plugin, KeySet * ks, Key * parentKey)
{
	// TODO: provide way to access kdbCommit and name without kdbprivate.h
	ksRewind (ks);
	int ret = plugin->kdbCommit (plugin, ks, parentKey);
	if (ret == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		if (keyGetMeta (parentKey, "error") == NULL)
		{
			addGenericError (parentKey, "kdbCommit", plugin->name);
		}
	}
	return ret;
}

static int runPluginListCommit (PluginList * plugins, KeySet * ks, Key * parentKey)
{
	for (PluginList * cur = plugins; cur != NULL; cur = cur->next)
	{
		if (runPluginCommit (cur->plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
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

	ElektraKdbPhase phase = elektraPluginGetPhase (plugin);
	switch (phase)
	{
	case ELEKTRA_KDB_SET_PHASE_PRE_COMMIT:
		return runPluginListCommit (handle->setPositions.precommit, ks, parentKey);
	case ELEKTRA_KDB_SET_PHASE_COMMIT:
		return runPluginCommit (handle->setPositions.commit, ks, parentKey);
	case ELEKTRA_KDB_SET_PHASE_POST_COMMIT:
		return runPluginListCommit (handle->setPositions.postcommit, ks, parentKey);
	default:
		ELEKTRA_SET_INTERNAL_ERRORF (
			parentKey, "Unknown phase of kdbSet(): %02x\n Please report this bug at https://issues.libelektra.org.", phase);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
}

static int runPluginError (Plugin * plugin, KeySet * ks, Key * parentKey)
{
	// TODO: provide way to access kdbError and name without kdbprivate.h
	ksRewind (ks);
	int ret = plugin->kdbError (plugin, ks, parentKey);
	if (ret == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		if (keyGetMeta (parentKey, "error") == NULL)
		{
			addGenericError (parentKey, "kdbError", plugin->name);
		}
	}
	return ret;
}

static int runPluginListError (PluginList * plugins, KeySet * ks, Key * parentKey)
{
	for (PluginList * cur = plugins; cur != NULL; cur = cur->next)
	{
		if (runPluginError (cur->plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
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

	ElektraKdbPhase phase = elektraPluginGetPhase (plugin);
	switch (phase)
	{
	case ELEKTRA_KDB_SET_PHASE_PRE_ROLLBACK:
		return runPluginListError (handle->setPositions.prerollback, ks, parentKey);
	case ELEKTRA_KDB_SET_PHASE_ROLLBACK:
		return runPluginError (handle->setPositions.rollback, ks, parentKey);
	case ELEKTRA_KDB_SET_PHASE_POST_ROLLBACK:
		return runPluginListError (handle->setPositions.postrollback, ks, parentKey);
	default:
		ELEKTRA_SET_INTERNAL_ERRORF (
			parentKey, "Unknown phase of kdbSet(): %02x\n Please report this bug at https://issues.libelektra.org.", phase);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
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
