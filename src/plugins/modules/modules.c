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

/**
 * The modules backend is configured by libelektra-kdb with the modules KeySet
 * of the KDB handle as its mountpoint definition. The plugin simply stores this
 * KeySet as its plugin data during the init phase of kdbGet() and then returns it
 * in the storage phase.
 */

int ELEKTRA_PLUGIN_FUNCTION (open) (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	elektraPluginSetData (handle, NULL);
	// init as read-only
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int ELEKTRA_PLUGIN_FUNCTION (init) (Plugin * handle, KeySet * definition, Key * parentKey ELEKTRA_UNUSED)
{
	elektraPluginSetData (handle, ksDup (definition));
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/modules"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/modules", KEY_VALUE, "modules plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/modules/exports", KEY_END),
			       keyNew ("system:/elektra/modules/modules/exports/open", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (open), KEY_END),
			       keyNew ("system:/elektra/modules/modules/exports/init", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (init), KEY_END),
			       keyNew ("system:/elektra/modules/modules/exports/get", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (get), KEY_END),
			       keyNew ("system:/elektra/modules/modules/exports/close", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (close), KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/modules/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	if (strcmp (keyName (parentKey), "system:/elektra/modules") != 0)
	{
		ELEKTRA_SET_INSTALLATION_ERROR (parentKey, "The 'modules' plugin is intended for internal use by 'libelektra-kdb' only.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	const char * phase = elektraPluginGetPhase (handle);
	if (strcmp (phase, KDB_GET_PHASE_RESOLVER) == 0)
	{
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	else if (strcmp (phase, KDB_GET_PHASE_STORAGE) == 0)
	{
		ksAppend (returned, elektraPluginGetData (handle));
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	else
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}
}

int ELEKTRA_PLUGIN_FUNCTION (close) (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	ksDel (elektraPluginGetData (handle));
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
