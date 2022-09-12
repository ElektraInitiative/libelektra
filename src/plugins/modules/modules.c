/**
 * @file
 *
 * @brief Source for modules plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "modules.h"

#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbprivate.h> // for struct _Plugin internals

/**
 * The modules backend is automatically configured by libelektra-kdb.
 * The init functions receives a mountpoint definition with a single Key '/plugin'.
 * This binary Key contains a Plugin* of which the modules plugin takes ownership.
 * During the storage phase of kdbGet() we then call the kdbGet() function of that plugin.
 * The plugin only works like this, if it is called with a parentKey of the form 'system:/elektra/modules/<plugin>' (where '<plugin>' !=
 * 'modules'). If the plugin is called with 'system:/elektra/modules/modules' it returns its own module information. If the plugin is called
 * with 'system:/elektra/modules' it does nothing. For all other parentKeys the plugin reports an error.
 */

int ELEKTRA_PLUGIN_FUNCTION (open) (Plugin * handle, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	elektraPluginSetData (handle, NULL);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int ELEKTRA_PLUGIN_FUNCTION (init) (Plugin * handle, ElektraKeyset * definition, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	ElektraKey * pluginKey = ksLookupByName (definition, "system:/plugin", 0);
	if (pluginKey != NULL)
	{
		elektraPluginSetData (handle, *(Plugin **) keyValue (pluginKey));
	}
	// init as read-only
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/modules"))
	{
		ElektraKeyset * contract =
			ksNew (30, keyNew ("system:/elektra/modules/modules", ELEKTRA_KEY_VALUE, "modules plugin waits for your orders", ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/modules/exports", ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/modules/exports/open", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (open), ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/modules/exports/init", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (init), ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/modules/exports/get", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (get), ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/modules/exports/close", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (close), ELEKTRA_KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/modules/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	ElektraKey * modulesRoot = keyNew ("system:/elektra/modules", ELEKTRA_KEY_END);
	if (keyCmp (modulesRoot, parentKey) == 0)
	{
		keyDel (modulesRoot);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	if (keyIsDirectlyBelow (modulesRoot, parentKey) != 1)
	{
		ELEKTRA_SET_INSTALLATION_ERROR (parentKey, "The 'modules' plugin is intended for internal use by 'libelektra-kdb' only.");
		keyDel (modulesRoot);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	const char * phase = elektraPluginGetPhase (handle);
	if (strcmp (phase, ELEKTRA_KDB_GET_PHASE_RESOLVER) == 0)
	{
		keyDel (modulesRoot);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	else if (strcmp (phase, ELEKTRA_KDB_GET_PHASE_STORAGE) == 0)
	{
		Plugin * plugin = elektraPluginGetData (handle);

		// create separate parentKey so that symlinked/aliased plugins still work
		// TODO (kodebach): use separate function for contract to avoid all of this
		keyAddBaseName (modulesRoot, plugin->name);
		int ret = plugin->kdbGet (plugin, returned, modulesRoot);
		keyDel (modulesRoot);

		if (ret == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	else
	{
		keyDel (modulesRoot);
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}
}

int ELEKTRA_PLUGIN_FUNCTION (close) (Plugin * handle, ElektraKey * errorKey)
{
	Plugin * plugin = elektraPluginGetData (handle);
	if (!elektraPluginClose (plugin, errorKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	elektraPluginSetData (handle, NULL);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("modules",
		ELEKTRA_PLUGIN_OPEN,	&ELEKTRA_PLUGIN_FUNCTION(open),
		ELEKTRA_PLUGIN_INIT,	&ELEKTRA_PLUGIN_FUNCTION(init),
		ELEKTRA_PLUGIN_GET,	&ELEKTRA_PLUGIN_FUNCTION(get),
		ELEKTRA_PLUGIN_CLOSE,	&ELEKTRA_PLUGIN_FUNCTION(close),
		ELEKTRA_PLUGIN_END);
}
