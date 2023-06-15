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
#include "./backend_odbc_general.h"
#include "./backend_odbc_get.h"
#include "./backend_odbc_set.h"

#include <kdbassert.h>
#include <kdbdiff.h>
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
		ELEKTRA_SET_INSTALLATION_ERRORF (parentKey, "Got NULL for the 'ksDefinition' argument for the mountpoint at %s",
						 keyName (parentKey));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	else if (!plugin)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (parentKey, "Got NULL for the 'plugin' argument for the mountpoint at %s",
						 keyName (parentKey));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	struct odbcSharedData * sharedData = elektraCalloc (sizeof (struct odbcSharedData));
	if (!sharedData)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	/* Check if the mountpoint definition CAN be valid */
	struct dataSourceConfig * dsConfig = fillDsStructFromDefinitionKs (ksDefinition, parentKey);

	if (!dsConfig)
	{
		/* The fillDsStructFromDefinitionKs () function should've set the error on the parentKey */
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	sharedData->dsConfig = dsConfig;

	// void * pluginData = elektraPluginGetData (plugin);

	/* TODO: Check if freeing this memory is safe in all cases! */
	// if (pluginData)
	//{
	//	elektraFree (pluginData);
	// }

	elektraPluginSetData (plugin, sharedData);

	/* init as read-write backend */
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}


static int setOdbcStorageUnitId (const struct dataSourceConfig * dsConfig, Key * parentKey)
{
	ssize_t ret = keySetString (parentKey, dsConfigToString (dsConfig));

	ELEKTRA_ASSERT (ret != 0,
			"keySetString returned 0. This looks like a bug! Please report the issue at https://issues.libelektra.org");
	if (ret == 1 || ret == -1)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	else
	{
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
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
	struct odbcSharedData * sharedData = elektraPluginGetData (plugin);

	if (!sharedData || !(sharedData->dsConfig))
	{
		ELEKTRA_SET_INTERNAL_ERROR (
			parentKey,
			"Internal plugin data for the ODBC backend was NULL. Please report this bug at https://issues.libelektra.org.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	ElektraKdbPhase phase = elektraPluginGetPhase (plugin);
	switch (phase)
	{
	case ELEKTRA_KDB_GET_PHASE_RESOLVER:
		/* There is no standardized way in ODBC to get the time of the last modification --> always query the data
		   However, feel free to implement checks for specific DBMSs. */
		return setOdbcStorageUnitId (sharedData->dsConfig, parentKey);

	case ELEKTRA_KDB_GET_PHASE_CACHECHECK:
		/* TODO: implement cache */
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;

	case ELEKTRA_KDB_GET_PHASE_PRE_STORAGE:
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;

	case ELEKTRA_KDB_GET_PHASE_STORAGE:
		if (ksAppend (ksReturned, getKeysFromDataSource (sharedData, false, parentKey)) == -1)
		{
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
		else
		{
			return ELEKTRA_PLUGIN_STATUS_SUCCESS;
		}

	case ELEKTRA_KDB_GET_PHASE_POST_STORAGE:
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;

	default:
		ELEKTRA_SET_INTERNAL_ERRORF (parentKey,
					     "An unknown get phase (not resolver, cachecheck, prestorage, storage or "
					     "poststorage) was encountered.\nThe encountered phase has an integer representation of '%d'\n"
					     "This looks like a bug! Please report this issues at https://issues.libelektra.org",
					     phase);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
}


int ELEKTRA_PLUGIN_FUNCTION (set) (Plugin * plugin, KeySet * ks, Key * parentKey)
{
	/* Gets filled by the init-function of this plugin (see above) and is used in later phases (esp. storage-phase) */
	struct odbcSharedData * sharedData = elektraPluginGetData (plugin);

	if (!sharedData || !(sharedData->dsConfig))
	{
		ELEKTRA_SET_INTERNAL_ERROR (
			parentKey,
			"Internal plugin data for the ODBC backend was NULL. Please report this bug at https://issues.libelektra.org.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	ElektraKdbPhase phase = elektraPluginGetPhase (plugin);

	switch (phase)
	{
	case ELEKTRA_KDB_SET_PHASE_RESOLVER: {
		/* Check whether the data was changed since the last get operation */
		int ret = setOdbcStorageUnitId (sharedData->dsConfig, parentKey);
		if (ret == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			ksAppendKey (elektraPluginGetGlobalKeySet (plugin),
				     keyNew ("system:/elektra/kdb/backend/failedphase", KEY_BINARY, KEY_SIZE, sizeof (ElektraKdbPhase),
					     KEY_VALUE, &phase, KEY_END));
		}
		return ret;
	}

	case ELEKTRA_KDB_SET_PHASE_PRE_STORAGE:
		/* This phase is currently not used (can be used for validation) */
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;

	case ELEKTRA_KDB_SET_PHASE_STORAGE: {
		/* Write the actual data to the ODBC data source */
		long ret = storeKeysInDataSource (sharedData, ks, parentKey);
		if (ret < 0)
		{
			ksAppendKey (elektraPluginGetGlobalKeySet (plugin),
				     keyNew ("system:/elektra/kdb/backend/failedphase", KEY_BINARY, KEY_SIZE, sizeof (ElektraKdbPhase),
					     KEY_VALUE, &phase, KEY_END));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
		else if (ret == 0)
		{
			return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
		}
		else
		{
			ELEKTRA_LOG ("The storage phase affected %ld rows.\n", ret);
			return ELEKTRA_PLUGIN_STATUS_SUCCESS;
		}
	}

	case ELEKTRA_KDB_SET_PHASE_POST_STORAGE:
		/* Not used here (mainly intended for logging and symmetry to the get-phases) */
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;

	default:
		ELEKTRA_SET_INTERNAL_ERRORF (parentKey,
					     "An unknown set phase (not resolver, prestorage, storage or poststorage) was "
					     "encountered.\nThe encountered phase has an integer representation of '%d'\n"
					     "This looks like a bug! Please report this issues at https://issues.libelektra.org",
					     phase);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
}


int ELEKTRA_PLUGIN_FUNCTION (commit) (Plugin * plugin, KeySet * ks ELEKTRA_UNUSED, Key * parentKey)
{
	ElektraKdbPhase phase = elektraPluginGetPhase (plugin);
	switch (phase)
	{
	case ELEKTRA_KDB_SET_PHASE_PRE_COMMIT:
		/* Not used by the ODBC backend plugin */
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;

	case ELEKTRA_KDB_SET_PHASE_COMMIT: {
		ELEKTRA_LOG_DEBUG ("in commit phase\n");

		struct odbcSharedData * sharedData = elektraPluginGetData (plugin);
		if (!sharedData || !(sharedData->connection) || !(sharedData->environment))
		{
			ELEKTRA_SET_INTERNAL_ERROR (parentKey,
						    "Could not commit transaction! Internal plugin data for the ODBC backend was NULL. "
						    "Please report this bug at https://issues.libelektra.org.");
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (endTransaction (sharedData->connection, true, parentKey))
		{
			ELEKTRA_LOG_DEBUG ("Commit succeeded\n");
			return ELEKTRA_PLUGIN_STATUS_SUCCESS;
		}
		else
		{
			ELEKTRA_LOG_DEBUG ("Commit failed!\n");
			ksAppendKey (elektraPluginGetGlobalKeySet (plugin),
				     keyNew ("system:/elektra/kdb/backend/failedphase", KEY_BINARY, KEY_SIZE, sizeof (ElektraKdbPhase),
					     KEY_VALUE, &phase, KEY_END));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	case ELEKTRA_KDB_SET_PHASE_POST_COMMIT:
		/* Not used by the ODBC backend plugin (this phase is mostly useful for logging) */
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;

	default:
		ELEKTRA_SET_INTERNAL_ERRORF (parentKey,
					     "An unknown commit phase (not precommit, commit or postcommit) was encountered.\n"
					     "The encountered phase has an integer representation of '%d'\n"
					     "This looks like a bug! Please report this issues at https://issues.libelektra.org",
					     phase);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
}


int ELEKTRA_PLUGIN_FUNCTION (error) (Plugin * plugin, KeySet * ks ELEKTRA_UNUSED, Key * parentKey)
{
	ElektraKdbPhase phase = elektraPluginGetPhase (plugin);

	switch (phase)
	{
	case ELEKTRA_KDB_SET_PHASE_PRE_ROLLBACK:
		/* Phase is used by the ODBC backend plugin */
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;

	case ELEKTRA_KDB_SET_PHASE_ROLLBACK: {
		struct odbcSharedData * sharedData = elektraPluginGetData (plugin);
		if (!sharedData || !(sharedData->connection) || !(sharedData->environment))
		{
			ELEKTRA_SET_INTERNAL_ERROR (parentKey,
						    "Could not rollback transaction! Internal plugin data for the ODBC backend was NULL. "
						    "Please report this bug at https://issues.libelektra.org.");
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		KeySet * globalKs = elektraPluginGetGlobalKeySet (plugin);
		Key * keyFailedPhase = ksLookupByName (globalKs, "system:/elektra/kdb/backend/failedphase", KDB_O_POP);

		if (!keyFailedPhase)
		{
			ELEKTRA_ADD_INTERNAL_WARNING (parentKey,
						      "The key 'system:/elektra/kdb/backend/failedphase' which should store the name of "
						      "the failed phase, was not found in global KeySet.");
		}
#ifdef HAVE_LOGGER
		else
		{
			ELEKTRA_LOG_NOTICE (
				"The phase '%s' failed and lead to the execution of the rollback-procedure during kdbSet().\n"
				"The changes will not be stored on in the data source!",
				elektraPluginPhaseName (*((ElektraKdbPhase *) keyValue (keyFailedPhase))));
			keyDel (keyFailedPhase);
		}
#endif

		bool ret = endTransaction (sharedData->connection, false, parentKey);

		/* Close the connection and free handles for connection and environment */
		if (!clearOdbcSharedData (sharedData, false, false))
		{
			ELEKTRA_ADD_RESOURCE_WARNING (parentKey,
						      "Could not successfully close the connection and free the SQL handles for "
						      " the connection and environment. Please check the state of your data source.");
		}

		elektraPluginSetData (plugin, sharedData);

		if (ret)
		{
			ELEKTRA_LOG_DEBUG ("ROLLBACK succeeded!\n");
			return ELEKTRA_PLUGIN_STATUS_SUCCESS;
		}
		else
		{
			ELEKTRA_LOG_DEBUG ("ROLLBACK failed!\n");
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	case ELEKTRA_KDB_SET_PHASE_POST_ROLLBACK:
		/* Phase is not used by the ODBC backend plugin */
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	default:
		ELEKTRA_SET_INTERNAL_ERRORF (parentKey,
					     "An unknown rollback phase (not prerollback, rollback or postrollback) was"
					     "encountered.\nThe encountered phase has an integer representation of '%d'\n"
					     "This looks like a bug! Please report this issues at https://issues.libelektra.org",
					     phase);

		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
}


int ELEKTRA_PLUGIN_FUNCTION (close) (Plugin * plugin ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	struct odbcSharedData * sharedData = elektraPluginGetData (plugin);
	clearOdbcSharedData (sharedData, true, true);
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
