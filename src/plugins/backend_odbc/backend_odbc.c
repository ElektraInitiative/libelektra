/**
 * @file
 *
 * @brief Source file for the ODBC backend plugin
 *
 * This file contains the functions that are called by the Elektra core when it uses plugins
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./backend_odbc.h"
#include "./backend_odbc_get.h"

#include <kdbassert.h>
#include <kdberrors.h>
#include <kdblogger.h>


int ELEKTRA_PLUGIN_FUNCTION (open) (Plugin * plugin ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int ELEKTRA_PLUGIN_FUNCTION (init) (Plugin * plugin, KeySet * ksDefinition, Key * parentKey)
{
	if (!ksDefinition)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (parentKey, "Got NULL for the KeySet 'ksDefinition' for the mountpoint at %s\n",
						 keyName (parentKey));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	else if (!plugin)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (parentKey, "Got NULL for the 'plugin' argument for the mountpoint at %s\n",
						 keyName (parentKey));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	struct dataSourceConfig * dsConfig = fillDsStructFromDefinitionKs (ksDefinition);

	if (!dsConfig)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (parentKey, "Could not get all necessary data from the mountpoint definition at %s\n",
						 keyName (parentKey));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	elektraPluginSetData (plugin, dsConfig);


	// init as read-only
	/* TODO: Implement set function and then change to STATUS_SUCCESS */
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}


int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * plugin, KeySet * ksReturned, Key * parentKey)
{
	if (elektraStrCmp (keyName (parentKey), "system:/elektra/modules/backend_odbc") == 0)
	{
		KeySet * contract = ksNew (
			30,
			keyNew ("system:/elektra/modules/backend_odbc", KEY_VALUE, "backend_odbc plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/backend_odbc/exports", KEY_END),
			keyNew ("system:/elektra/modules/backend_odbc/exports/open", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (open), KEY_END),
			keyNew ("system:/elektra/modules/backend_odbc/exports/init", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (init), KEY_END),
			keyNew ("system:/elektra/modules/backend_odbc/exports/get", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (get), KEY_END),
			keyNew ("system:/elektra/modules/backend_odbc/exports/set", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (set), KEY_END),
			keyNew ("system:/elektra/modules/backend_odbc/exports/commit", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (commit), KEY_END),
			keyNew ("system:/elektra/modules/backend_odbc/exports/error", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (error), KEY_END),
			keyNew ("system:/elektra/modules/backend_odbc/exports/close", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (close), KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/backend_odbc/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (ksReturned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	/* Gets filled by the init-function of this plugin (see above) and is used in later phases (esp. storage-phase) */
	struct dataSourceConfig * dsConfig = elektraPluginGetData (plugin);

	if (!dsConfig)
	{
		ELEKTRA_SET_INTERNAL_ERROR (
			parentKey,
			"Internal plugin data for the ODBC backend was NULL. Please report this bug at https://issues.libelektra.org.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	ElektraKdbPhase phase = elektraPluginGetPhase (plugin);
	switch (phase)
	{
	case ELEKTRA_KDB_GET_PHASE_RESOLVER: {
		ssize_t ret = keySetString (parentKey, dsConfigToString (dsConfig));
		ELEKTRA_ASSERT (ret != 0,
				"keySetString returned 0. This looks like a programming error!\\nPlease report the issue at "
				"https://issues.libelektra.org");
		if (ret == 1 || ret == -1)
		{
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
		else
		{
			return ELEKTRA_PLUGIN_STATUS_SUCCESS;
		}
	}
	case ELEKTRA_KDB_GET_PHASE_CACHECHECK:
		/* TODO: implement cache */
		// return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	case ELEKTRA_KDB_GET_PHASE_PRE_STORAGE:
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	case ELEKTRA_KDB_GET_PHASE_STORAGE:
		if (ksAppend (ksReturned, getKeysFromDataSource (dsConfig, parentKey)) == -1)
		{
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
		else
		{
			return ELEKTRA_PLUGIN_STATUS_SUCCESS;
		}
	case ELEKTRA_KDB_GET_PHASE_POST_STORAGE:
	default:
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}
}


int ELEKTRA_PLUGIN_FUNCTION (set) (Plugin * plugin ELEKTRA_UNUSED, KeySet * ks ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	/* TODO: Implement set function */
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}


int ELEKTRA_PLUGIN_FUNCTION (commit) (Plugin * plugin ELEKTRA_UNUSED, KeySet * ks ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	/* TODO: Implement commit for set-operations */
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}


int ELEKTRA_PLUGIN_FUNCTION (error) (Plugin * plugin ELEKTRA_UNUSED, KeySet * ks ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	/* TODO: Implement error function */
	return ELEKTRA_PLUGIN_STATUS_ERROR;
}


int ELEKTRA_PLUGIN_FUNCTION (close) (Plugin * plugin, Key * errorKey ELEKTRA_UNUSED)
{
	struct dataSourceConfig * dsConfig = elektraPluginGetData (plugin);
	elektraFree (dsConfig);
	elektraPluginSetData (plugin, NULL);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("backend_odbc",
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
