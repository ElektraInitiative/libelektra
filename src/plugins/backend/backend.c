

#include "backend.h"
#include "backendprivate.h"

#include <kdberrors.h>
#include <kdblogger.h>
#include <kdbprivate.h>

// FIXME: TESTS

int ELEKTRA_PLUGIN_FUNCTION (open) (Plugin * plugin, ElektraKey * errorKey ELEKTRA_UNUSED)
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

static bool loadPlugin (Plugin ** pluginPtr, Plugin * thisPlugin, ElektraKey * pluginRefKey, enum PluginType type, ElektraKey * parentKey)
{
	if (pluginRefKey == NULL)
	{
		*pluginPtr = NULL;
		return true;
	}

	const char * pluginRef = elektraKeyString (pluginRefKey);
	*pluginPtr = elektraPluginFromMountpoint (thisPlugin, pluginRef);
	if (*pluginPtr == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (parentKey,
						 "The plugin referenced in '%s%s' (value: '%s', refering to '%s/plugins/%s') could not be "
						 "found. (Configuration of mountpoint: '%s')",
						 elektraKeyName (parentKey), elektraKeyName (pluginRefKey), pluginRef, elektraKeyName (parentKey), pluginRef,
						 elektraKeyBaseName (parentKey));
		return false;
	}

	if (type == PLUGIN_TYPE_GET && (*pluginPtr)->kdbGet == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (parentKey,
						 "The plugin '%s' was referenced in a kdbGet() position ('%s%s'), but does not implement "
						 "kdbGet(). (Configuration of mountpoint: '%s')",
						 (*pluginPtr)->name, elektraKeyName (parentKey), elektraKeyName (pluginRefKey), elektraKeyBaseName (parentKey));
		return false;
	}

	if (type == PLUGIN_TYPE_SET && (*pluginPtr)->kdbSet == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (parentKey,
						 "The plugin '%s' was referenced in a kdbSet() position ('%s%s'), but does not implement "
						 "kdbSet(). (Configuration of mountpoint: '%s')",
						 (*pluginPtr)->name, elektraKeyName (parentKey), elektraKeyName (pluginRefKey), elektraKeyBaseName (parentKey));
		return false;
	}

	if (type == PLUGIN_TYPE_COMMIT && (*pluginPtr)->kdbCommit == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (parentKey,
						 "The plugin '%s' was referenced in a kdbCommit() position ('%s%s'), but does not "
						 "implement kdbCommit(). (Configuration of mountpoint: '%s')",
						 (*pluginPtr)->name, elektraKeyName (parentKey), elektraKeyName (pluginRefKey), elektraKeyBaseName (parentKey));
		return false;
	}

	if (type == PLUGIN_TYPE_ERROR && (*pluginPtr)->kdbError == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (parentKey,
						 "The plugin '%s' was referenced in a kdbError() position ('%s%s'), but does not implement "
						 "kdbError(). (Configuration of mountpoint: '%s')",
						 (*pluginPtr)->name, elektraKeyName (parentKey), elektraKeyName (pluginRefKey), elektraKeyBaseName (parentKey));
		return false;
	}

	return true;
}

static bool loadPluginList (PluginList ** pluginListPtr, Plugin * thisPlugin, ElektraKeyset * definition, const char * pluginRefRootName,
			    enum PluginType type, ElektraKey * parentKey)
{
	ElektraKey * pluginRefRoot = elektraKeyNew (pluginRefRootName, ELEKTRA_KEY_END);

	*pluginListPtr = NULL;
	PluginList * listEnd = NULL;

	for (elektraCursor end, i = elektraKeysetFindHierarchy (definition, pluginRefRoot, &end); i < end; i++)
	{
		ElektraKey * cur = elektraKeysetAtCursor (definition, i);
		if (elektraKeyIsDirectlyBelow (pluginRefRoot, cur) != 1)
		{
			continue;
		}

		Plugin * plugin;
		if (!loadPlugin (&plugin, thisPlugin, cur, type, parentKey))
		{
			elektraKeyDel (pluginRefRoot);
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

	elektraKeyDel (pluginRefRoot);
	return true;
}

int ELEKTRA_PLUGIN_FUNCTION (init) (Plugin * plugin, ElektraKeyset * definition, ElektraKey * parentKey)
{
	if (elektraKeyGetNamespace (parentKey) == ELEKTRA_NS_PROC)
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

	ElektraKey * pathKey = elektraKeysetLookupByName (definition, "system:/path", 0);
	const char * path = elektraKeyString (pathKey);
	if (pathKey == NULL || strlen (path) == 0)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (
			parentKey, "You must set '%s/definition/path' to a non-empty value. (Configuration of mountpoint: %s)",
			elektraKeyName (parentKey), elektraKeyBaseName (parentKey));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	BackendHandle * handle = elektraPluginGetData (plugin);
	handle->path = elektraStrDup (path);

	// load get plugins
	if (!loadPlugin (&handle->getPositions.resolver, plugin, elektraKeysetLookupByName (definition, "system:/positions/get/resolver", 0),
			 PLUGIN_TYPE_GET, parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPluginList (&handle->getPositions.prestorage, plugin, definition, "system:/positions/get/prestorage", PLUGIN_TYPE_GET,
			     parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPlugin (&handle->getPositions.storage, plugin, elektraKeysetLookupByName (definition, "system:/positions/get/storage", 0),
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
	if (!loadPlugin (&handle->setPositions.resolver, plugin, elektraKeysetLookupByName (definition, "system:/positions/set/resolver", 0),
			 PLUGIN_TYPE_SET, parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPluginList (&handle->setPositions.prestorage, plugin, definition, "system:/positions/set/prestorage", PLUGIN_TYPE_SET,
			     parentKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	if (!loadPlugin (&handle->setPositions.storage, plugin, elektraKeysetLookupByName (definition, "system:/positions/set/storage", 0),
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
	if (!loadPlugin (&handle->setPositions.commit, plugin, elektraKeysetLookupByName (definition, "system:/positions/set/commit", 0),
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
	if (!loadPlugin (&handle->setPositions.rollback, plugin, elektraKeysetLookupByName (definition, "system:/positions/set/rollback", 0),
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
		if (path[0] == '/' && elektraKeysetLookupByName (definition, "system:/path/absolute", 0) == NULL)
		{
			ELEKTRA_ADD_INSTALLATION_WARNINGF (
				parentKey,
				"You configured a resolver. The absolute path in '%s/definition/path' might not be used as-is. If the "
				"configuration is intentional, set '%s/definition/path/absolute' to any value to silence this warning. "
				"(Configuration of mountpoint: '%s')",
				elektraKeyName (parentKey), elektraKeyName (parentKey), elektraKeyBaseName (parentKey));
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
				elektraKeyName (parentKey), elektraKeyName (parentKey), elektraKeyBaseName (parentKey));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (path[0] != '/')
		{
			ELEKTRA_SET_INSTALLATION_ERRORF (
				parentKey,
				"If no resolver is configured, '%s/definition/path' you must set to an absolute path. "
				"(Configuration of mountpoint: '%s')",
				elektraKeyName (parentKey), elektraKeyBaseName (parentKey));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	// TODO (Q): support for read-write to absolute path?
	bool readOnly = handle->setPositions.resolver == NULL;

	if (handle->getPositions.storage == NULL && elektraKeysetLookupByName (definition, "system:/positions/get/storage/omit", 0) == NULL)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (
			parentKey,
			"No storage plugin defined for kdbGet(). You probably forgot to set '%s/definition/positions/get/storage'. If the "
			"configuration is intentional, you can silence this warning by setting '%s/definition/positions/get/storage/omit' "
			"to any value. (Configuration of mountpoint: '%s')",
			elektraKeyName (parentKey), elektraKeyName (parentKey), elektraKeyBaseName (parentKey));
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
				elektraKeyBaseName (parentKey), elektraKeyName (parentKey), elektraKeyBaseName (parentKey));
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
				elektraKeyName (parentKey), elektraKeyBaseName (parentKey));
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
				elektraKeyName (parentKey), handle->setPositions.resolver->name, elektraKeyName (parentKey), elektraKeyBaseName (parentKey));
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
				elektraKeyName (parentKey), handle->setPositions.resolver->name, elektraKeyName (parentKey), elektraKeyBaseName (parentKey));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	if (handle->getPositions.resolver != handle->setPositions.resolver &&
	    elektraKeysetLookupByName (definition, "system:/positions/set/resolver/differs", 0) == NULL)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (
			parentKey,
			"The resolver plugin for kdbSet() ('%s') differs from the resolver plugin for kdbGet() ('%s'). This is a "
			"non-standard configuration. Normally, '%s/definition/positions/get/resolver' and "
			"'%s/definition/positions/set/resolver' should have the same value. If the configuration is intentional, you can "
			"silence this warning by setting '%s/definition/positions/set/resolver/differs' to any value. (Configuration of "
			"mountpoint: '%s')",
			handle->getPositions.resolver->name, handle->setPositions.resolver->name, elektraKeyName (parentKey), elektraKeyName (parentKey),
			elektraKeyName (parentKey), elektraKeyBaseName (parentKey));
	}

	if (handle->setPositions.resolver != handle->setPositions.commit &&
	    elektraKeysetLookupByName (definition, "system:/positions/set/commit/differs", 0) == NULL)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (
			parentKey,
			"The resolver plugin ('%s') differs from the commit plugin ('%s'). This is a non-standard configuration. Normally, "
			"'%s/definition/positions/set/resolver' and '%s/definition/positions/set/commit' should have the same value. If "
			"the configuration is intentional, you can silence this warning by setting "
			"'%s/definition/positions/set/commit/differs' to any value. (Configuration of mountpoint: '%s')",
			handle->setPositions.resolver->name, handle->setPositions.commit->name, elektraKeyName (parentKey), elektraKeyName (parentKey),
			elektraKeyName (parentKey), elektraKeyBaseName (parentKey));
	}

	if (handle->setPositions.resolver != handle->setPositions.rollback &&
	    elektraKeysetLookupByName (definition, "system:/positions/set/rollback/differs", 0) == NULL)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (
			parentKey,
			"The resolver plugin ('%s') differs from the rollback plugin ('%s'). This is a non-standard configuration. "
			"Normally, '%s/definition/positions/set/resolver' and '%s/definition/positions/set/rollback' should have the same "
			"value. If the configuration is intentional, you can silence this warning by setting "
			"'%s/definition/positions/set/rollback/differs' to any value. (Configuration of mountpoint: '%s')",
			handle->setPositions.resolver->name, handle->setPositions.rollback->name, elektraKeyName (parentKey), elektraKeyName (parentKey),
			elektraKeyName (parentKey), elektraKeyBaseName (parentKey));
	}

	return readOnly ? ELEKTRA_PLUGIN_STATUS_NO_UPDATE : ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static inline void addGenericError (ElektraKey * key, const char * function, const char * plugin)
{
	ELEKTRA_SET_INTERFACE_ERRORF (key,
				      "The %s() function of the plugin '%s' returned ELEKTRA_PLUGIN_STATUS_ERROR, but did not actually set "
				      "an error. If you are the author of this plugin, please add a proper error to the parentKey.",
				      function, plugin);
}

static int runPluginGet (Plugin * plugin, ElektraKeyset * ks, ElektraKey * parentKey)
{
	// TODO: provide way to access kdbGet and name without kdbprivate.h
	elektraKeysetRewind (ks);
	int ret = plugin->kdbGet (plugin, ks, parentKey);
	if (ret == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		if (elektraKeyGetMeta (parentKey, "error") == NULL)
		{
			addGenericError (parentKey, "kdbGet", plugin->name);
		}
	}
	return ret;
}

static int runPluginListGet (PluginList * plugins, ElektraKeyset * ks, ElektraKey * parentKey)
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

int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * plugin, ElektraKeyset * ks, ElektraKey * parentKey)
{
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/backend"))
	{
		ElektraKeyset * contract = elektraKeysetNew (
			30, elektraKeyNew ("system:/elektra/modules/backend", ELEKTRA_KEY_VALUE, "backend plugin waits for your orders", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/backend/exports", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/backend/exports/open", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (open), ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/backend/exports/init", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (init), ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/backend/exports/get", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (get), ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/backend/exports/set", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (set), ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/backend/exports/commit", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (commit), ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/backend/exports/error", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (error), ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/backend/exports/close", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (close), ELEKTRA_KEY_END),
#include ELEKTRA_README
			elektraKeyNew ("system:/elektra/modules/backend/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (ks, contract);
		elektraKeysetDel (contract);

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
	if (strcmp (phase, ELEKTRA_KDB_GET_PHASE_RESOLVER) == 0)
	{
		elektraKeySetString (parentKey, handle->path);

		if (handle->getPositions.resolver == NULL)
		{
			// no resolver configured -> path is absolute
			// TODO (Q): check mtime to determine up date needed?
			return ELEKTRA_PLUGIN_STATUS_SUCCESS;
		}

		return runPluginGet (handle->getPositions.resolver, ks, parentKey);
	}
	else if (strcmp (phase, ELEKTRA_KDB_GET_PHASE_CACHECHECK) == 0)
	{
		// FIXME (kodebach): implement cache
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}
	else if (strcmp (phase, ELEKTRA_KDB_GET_PHASE_PRE_STORAGE) == 0)
	{
		return runPluginListGet (handle->getPositions.prestorage, ks, parentKey);
	}
	else if (strcmp (phase, ELEKTRA_KDB_GET_PHASE_STORAGE) == 0)
	{
		return runPluginGet (handle->getPositions.storage, ks, parentKey);
	}
	else if (strcmp (phase, ELEKTRA_KDB_GET_PHASE_POST_STORAGE) == 0)
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

static int runPluginSet (Plugin * plugin, ElektraKeyset * ks, ElektraKey * parentKey)
{
	// TODO: provide way to access kdbSet and name without kdbprivate.h
	elektraKeysetRewind (ks);
	int ret = plugin->kdbSet (plugin, ks, parentKey);
	if (ret == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		if (elektraKeyGetMeta (parentKey, "error") == NULL)
		{
			addGenericError (parentKey, "kdbSet", plugin->name);
		}
	}
	return ret;
}

static int runPluginListSet (PluginList * plugins, ElektraKeyset * ks, ElektraKey * parentKey)
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

int ELEKTRA_PLUGIN_FUNCTION (set) (Plugin * plugin, ElektraKeyset * ks, ElektraKey * parentKey)
{
	BackendHandle * handle = elektraPluginGetData (plugin);
	if (handle == NULL)
	{
		ELEKTRA_SET_INTERNAL_ERROR (parentKey,
					    "Internal plugin data was NULL. Please report this bug at https://issues.libelektra.org.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	const char * phase = elektraPluginGetPhase (plugin);
	if (strcmp (phase, ELEKTRA_KDB_SET_PHASE_RESOLVER) == 0)
	{
		elektraKeySetString (parentKey, handle->path);

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
	else if (strcmp (phase, ELEKTRA_KDB_SET_PHASE_PRE_STORAGE) == 0)
	{
		return runPluginListSet (handle->setPositions.prestorage, ks, parentKey);
	}
	else if (strcmp (phase, ELEKTRA_KDB_SET_PHASE_STORAGE) == 0)
	{
		return runPluginSet (handle->setPositions.storage, ks, parentKey);
	}
	else if (strcmp (phase, ELEKTRA_KDB_SET_PHASE_POST_STORAGE) == 0)
	{
		return runPluginListSet (handle->setPositions.poststorage, ks, parentKey);
	}
	else
	{
		ELEKTRA_SET_INTERNAL_ERRORF (
			parentKey, "Unknown phase of kdbSet(): %s\n Please report this bug at https://issues.libelektra.org.", phase);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static int runPluginCommit (Plugin * plugin, ElektraKeyset * ks, ElektraKey * parentKey)
{
	// TODO: provide way to access kdbCommit and name without kdbprivate.h
	elektraKeysetRewind (ks);
	int ret = plugin->kdbCommit (plugin, ks, parentKey);
	if (ret == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		if (elektraKeyGetMeta (parentKey, "error") == NULL)
		{
			addGenericError (parentKey, "kdbCommit", plugin->name);
		}
	}
	return ret;
}

static int runPluginListCommit (PluginList * plugins, ElektraKeyset * ks, ElektraKey * parentKey)
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

int ELEKTRA_PLUGIN_FUNCTION (commit) (Plugin * plugin, ElektraKeyset * ks, ElektraKey * parentKey)
{
	BackendHandle * handle = elektraPluginGetData (plugin);
	if (handle == NULL)
	{
		ELEKTRA_SET_INTERNAL_ERROR (parentKey,
					    "Internal plugin data was NULL. Please report this bug at https://issues.libelektra.org.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	const char * phase = elektraPluginGetPhase (plugin);
	if (strcmp (phase, ELEKTRA_KDB_SET_PHASE_PRE_COMMIT) == 0)
	{
		return runPluginListCommit (handle->setPositions.precommit, ks, parentKey);
	}
	else if (strcmp (phase, ELEKTRA_KDB_SET_PHASE_COMMIT) == 0)
	{
		return runPluginCommit (handle->setPositions.commit, ks, parentKey);
	}
	else if (strcmp (phase, ELEKTRA_KDB_SET_PHASE_POST_COMMIT) == 0)
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

static int runPluginError (Plugin * plugin, ElektraKeyset * ks, ElektraKey * parentKey)
{
	// TODO: provide way to access kdbError and name without kdbprivate.h
	elektraKeysetRewind (ks);
	int ret = plugin->kdbError (plugin, ks, parentKey);
	if (ret == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		if (elektraKeyGetMeta (parentKey, "error") == NULL)
		{
			addGenericError (parentKey, "kdbError", plugin->name);
		}
	}
	return ret;
}

static int runPluginListError (PluginList * plugins, ElektraKeyset * ks, ElektraKey * parentKey)
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

int ELEKTRA_PLUGIN_FUNCTION (error) (Plugin * plugin, ElektraKeyset * ks, ElektraKey * parentKey)
{
	BackendHandle * handle = elektraPluginGetData (plugin);
	if (handle == NULL)
	{
		ELEKTRA_SET_INTERNAL_ERROR (parentKey,
					    "Internal plugin data was NULL. Please report this bug at https://issues.libelektra.org.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	const char * phase = elektraPluginGetPhase (plugin);
	if (strcmp (phase, ELEKTRA_KDB_SET_PHASE_PRE_ROLLBACK) == 0)
	{
		return runPluginListError (handle->setPositions.prerollback, ks, parentKey);
	}
	else if (strcmp (phase, ELEKTRA_KDB_SET_PHASE_ROLLBACK) == 0)
	{
		return runPluginError (handle->setPositions.rollback, ks, parentKey);
	}
	else if (strcmp (phase, ELEKTRA_KDB_SET_PHASE_POST_ROLLBACK) == 0)
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

int ELEKTRA_PLUGIN_FUNCTION (close) (Plugin * plugin, ElektraKey * errorKey ELEKTRA_UNUSED)
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
