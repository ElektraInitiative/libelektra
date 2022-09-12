/**
 * @file
 *
 * @brief Source for list plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include "list.h"
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbinternal.h>
#include <kdbinvoke.h>
#include <kdbmodule.h>
#include <kdbos.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


typedef enum
{
	preGetStorage = 0,
	procGetStorage,
	postGetStorage,
	postGetCleanup,
	getEnd
} GetPlacements;

static const char * getStrings[] = { "pregetstorage", "procgetstorage", "postgetstorage", "postgetcleanup" };

typedef enum
{
	preSetStorage = 0,
	preSetCleanup,
	preCommit,
	postCommit,
	setEnd
} SetPlacements;

static const char * setStrings[] = { "presetstorage", "presetcleanup", "precommit", "postcommit" };

typedef enum
{
	preRollback = 0,
	postRollback,
	errEnd
} ErrPlacements;

static const char * errStrings[] = { "prerollback", "postrollback" };

typedef enum
{
	GET,
	SET,
	ERR
} OP;

typedef struct
{
	// keep track of placements
	ErrPlacements errCurrent;
	SetPlacements setCurrent;
	GetPlacements getCurrent;

	ErrPlacements errPlacements[2]; // prerollback and postrollback
	SetPlacements setPlacements[4]; // presetstorage, presetcleanup, precommit and postcommit
	GetPlacements getPlacements[4]; // pregetstorage, procgetstorage, postgetstorage, postgetclenaup

	// each keyset contains the list of plugin names for a given placement
	ElektraKeyset * setKS[4];
	ElektraKeyset * errKS[2];
	ElektraKeyset * getKS[4];
	ElektraKeyset * plugins;
	ElektraKeyset * modules;

	ElektraDeferredCallList * deferredCalls;

} Placements;

static char lastIndex[ELEKTRA_MAX_ARRAY_SIZE];

static int listParseConfiguration (Placements * placements, ElektraKeyset * config)
{
	ElektraKey * cur;
	ElektraKey * key = elektraKeysetLookupByName (config, "/plugins", 0);
	ElektraKeyset * cutKS = elektraKeysetCut (config, key);
	elektraKeysetRewind (cutKS);
	if (elektraKeysetGetSize (cutKS) < 2)
	{
		elektraKeysetDel (cutKS);
		return 0;
	}
	int rc = 0;
	while ((cur = elektraKeysetNext (cutKS)) != NULL)
	{
		if (elektraKeyIsDirectlyBelow (key, cur) != 1)
		{
			continue;
		}
		if (elektraKeyBaseName (cur)[0] == '#')
		{
			if (strcmp (lastIndex, elektraKeyBaseName (cur)) < 0)
			{
				snprintf (lastIndex, ELEKTRA_MAX_ARRAY_SIZE, "%s", elektraKeyBaseName (cur));
			}
		}
		ElektraKey * sub;
		ElektraKey * lookup = elektraKeyDup (cur, ELEKTRA_KEY_CP_ALL);
		elektraKeyAddBaseName (lookup, "placements");
		elektraKeyAddBaseName (lookup, "set");
		sub = elektraKeysetLookup (cutKS, lookup, 0);
		if (sub)
		{
			const char * setString = elektraKeyString (sub);
			SetPlacements setPlacement = preSetStorage;
			while (setPlacement != setEnd)
			{
				if (strstr (setString, setStrings[setPlacement]))
				{
					rc = 1;
					elektraKeysetAppendKey (placements->setKS[setPlacement], elektraKeyDup (cur, ELEKTRA_KEY_CP_ALL));
				}
				++setPlacement;
			}
		}
		elektraKeySetBaseName (lookup, "get");
		sub = elektraKeysetLookup (cutKS, lookup, 0);
		if (sub)
		{
			const char * getString = elektraKeyString (sub);
			GetPlacements getPlacement = preGetStorage;
			while (getPlacement != getEnd)
			{
				if (strstr (getString, getStrings[getPlacement]))
				{
					rc = 1;
					elektraKeysetAppendKey (placements->getKS[getPlacement], elektraKeyDup (cur, ELEKTRA_KEY_CP_ALL));
				}
				++getPlacement;
			}
		}
		elektraKeySetBaseName (lookup, "error");
		sub = elektraKeysetLookup (cutKS, lookup, 0);
		if (sub)
		{
			const char * errString = elektraKeyString (sub);
			ErrPlacements errPlacement = preRollback;
			while (errPlacement != errEnd)
			{
				if (strstr (errString, errStrings[errPlacement]))
				{
					rc = 1;
					elektraKeysetAppendKey (placements->errKS[errPlacement], elektraKeyDup (cur, ELEKTRA_KEY_CP_ALL));
				}
				++errPlacement;
			}
		}
		elektraKeyDel (lookup);
	}
	elektraKeysetDel (cutKS);
	return rc;
}

void elektraListDeferredCall (Plugin * plugin, const char * name, ElektraKeyset * parameters)
{
	Placements * placements = elektraPluginGetData (plugin);
	ELEKTRA_NOT_NULL (placements);
	elektraDeferredCallAdd (placements->deferredCalls, name, parameters);

	// Execute call immediately on already loaded plugins
	elektraKeysetRewind (placements->plugins);
	ElektraKey * current;
	while ((current = elektraKeysetNext (placements->plugins)) != NULL)
	{
		Plugin * slave;
		slave = *(Plugin **) elektraKeyValue (current);
		elektraDeferredCallsExecute (slave, placements->deferredCalls);
	}
}

int elektraListOpen (Plugin * handle, ElektraKey * errorKey ELEKTRA_UNUSED)
{

	Placements * placements = (Placements *) elektraPluginGetData (handle);
	if (!placements)
	{
		placements = (Placements *) elektraMalloc (sizeof (Placements));
		memset (placements, 0, sizeof (Placements));
		placements->errCurrent = preRollback;
		placements->setCurrent = preSetStorage;
		placements->getCurrent = preGetStorage;
		placements->getKS[0] = elektraKeysetNew (0, ELEKTRA_KS_END);
		placements->getKS[1] = elektraKeysetNew (0, ELEKTRA_KS_END);
		placements->getKS[2] = elektraKeysetNew (0, ELEKTRA_KS_END);
		placements->setKS[0] = elektraKeysetNew (0, ELEKTRA_KS_END);
		placements->setKS[1] = elektraKeysetNew (0, ELEKTRA_KS_END);
		placements->setKS[2] = elektraKeysetNew (0, ELEKTRA_KS_END);
		placements->setKS[3] = elektraKeysetNew (0, ELEKTRA_KS_END);
		placements->errKS[0] = elektraKeysetNew (0, ELEKTRA_KS_END);
		placements->errKS[1] = elektraKeysetNew (0, ELEKTRA_KS_END);
		placements->plugins = elektraKeysetNew (0, ELEKTRA_KS_END);
		placements->modules = elektraKeysetNew (0, ELEKTRA_KS_END);
		placements->deferredCalls = elektraDeferredCallCreateList ();
	}
	elektraPluginSetData (handle, placements);

	elektraModulesInit (placements->modules, NULL);
	ElektraKeyset * config = elektraKeysetDup (elektraPluginGetConfig (handle));
	elektraKeysetRewind (config);
	ElektraKey * key = elektraKeysetLookupByName (config, "/placements/set", 0);
	if (key)
	{
		const char * setString = elektraKeyString (key);
		SetPlacements setPlacement = preSetStorage;
		while (setPlacement != setEnd)
		{
			if (strstr (setString, setStrings[setPlacement]))
			{
				placements->setPlacements[setPlacement] = 1;
			}
			++setPlacement;
		}
	}
	key = elektraKeysetLookupByName (config, "/placements/get", 0);
	if (key)
	{
		const char * getString = elektraKeyString (key);
		GetPlacements getPlacement = preGetStorage;
		while (getPlacement != getEnd)
		{
			if (strstr (getString, getStrings[getPlacement]))
			{
				placements->getPlacements[getPlacement] = 1;
			}
			++getPlacement;
		}
	}
	key = elektraKeysetLookupByName (config, "/placements/error", 0);
	if (key)
	{
		const char * errString = elektraKeyString (key);
		ErrPlacements errPlacement = preRollback;
		while (errPlacement != errEnd)
		{
			if (strstr (errString, errStrings[errPlacement]))
			{
				placements->errPlacements[errPlacement] = 1;
			}
			++errPlacement;
		}
	}
	listParseConfiguration (placements, config);
	elektraKeysetDel (config);
	return 1; /* success */
}

int elektraListClose (Plugin * handle, ElektraKey * errorKey)
{
	/* free all plugin resources and shut it down */
	Placements * placements = elektraPluginGetData (handle);
	elektraKeysetDel (placements->getKS[0]);
	elektraKeysetDel (placements->getKS[1]);
	elektraKeysetDel (placements->getKS[2]);
	elektraKeysetDel (placements->setKS[0]);
	elektraKeysetDel (placements->setKS[1]);
	elektraKeysetDel (placements->setKS[2]);
	elektraKeysetDel (placements->setKS[3]);
	elektraKeysetDel (placements->errKS[0]);
	elektraKeysetDel (placements->errKS[1]);
	ElektraKey * cur;
	elektraKeysetRewind (placements->plugins);
	while ((cur = elektraKeysetNext (placements->plugins)) != NULL)
	{
		Plugin * slave;
		slave = *(Plugin **) elektraKeyValue (cur);
		elektraPluginClose (slave, errorKey);
	}
	elektraKeysetDel (placements->plugins);
	elektraModulesClose (placements->modules, NULL);
	elektraKeysetDel (placements->modules);
	elektraDeferredCallDeleteList (placements->deferredCalls);
	elektraFree (placements);
	elektraPluginSetData (handle, 0);
	return 1; /* success */
}

static int runPlugins (ElektraKeyset * pluginKS, ElektraKeyset * modules, ElektraKeyset * plugins, ElektraKeyset * configOrig, ElektraKeyset * returned, ElektraKeyset * global,
		       ElektraKey * parentKey, OP op, ElektraKey * (*traversalFunction) (ElektraKeyset *), ElektraDeferredCallList * deferredCalls)
{
	ElektraKey * current;

	Plugin * slave = NULL;

	// for every plugin in our list: load it, run the expected function (set/get/error) and close it again
	ElektraKeyset * realPluginConfig = NULL;
	while ((current = traversalFunction (pluginKS)) != NULL)
	{
		const char * name = elektraKeyString (current);

		ElektraKey * handleKey = elektraKeyDup (current, ELEKTRA_KEY_CP_ALL);
		elektraKeyAddName (handleKey, "handle");
		ElektraKey * handleLookup = elektraKeysetLookup (configOrig, handleKey, 0);
		elektraKeyDel (handleKey);
		if (handleLookup)
		{
			slave = *(Plugin **) elektraKeyValue (handleLookup);
		}
		else
		{
			ElektraKey * searchKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
			elektraKeyAddBaseName (searchKey, name);
			ElektraKey * lookup = elektraKeysetLookup (plugins, searchKey, 0);
			elektraKeyDel (searchKey);
			if (lookup)
			{
				slave = *(Plugin **) elektraKeyValue (lookup);
			}
			else
			{
				ElektraKey * userCutPoint = elektraKeyNew ("user:/", ELEKTRA_KEY_END);
				ElektraKey * globalConfCutPoint = elektraKeyNew ("/config", ELEKTRA_KEY_END);
				ElektraKeyset * config = elektraKeysetDup (configOrig);
				ElektraKeyset * globalConfigAll = elektraKeysetCut (config, globalConfCutPoint);
				ElektraKeyset * userConfigAll = elektraKeysetCut (config, userCutPoint);
				ElektraKeyset * pluginConfig = elektraKeysetCut (userConfigAll, current);
				// replace "user:/plugins/#X" with "user:/"
				ElektraKeyset * pluginConfigWithConfigPrefix = elektraKeysetRenameKeys (pluginConfig, "user:/");
				elektraKeysetDel (pluginConfig);
				// append config below "/config" to all plugins
				ElektraKeyset * globalPluginConfig = elektraKeysetRenameKeys (globalConfigAll, "user:/config");
				elektraKeysetAppend (pluginConfigWithConfigPrefix, globalPluginConfig);
				elektraKeysetDel (globalPluginConfig);
				// remove "placements" from plugin config
				ElektraKey * toRemove = elektraKeyNew ("user:/placements", ELEKTRA_KEY_END);
				elektraKeysetDel (elektraKeysetCut (pluginConfigWithConfigPrefix, toRemove));
				elektraKeysetRewind (pluginConfigWithConfigPrefix);
				elektraKeysetDel (globalConfigAll);
				elektraKeysetDel (userConfigAll);
				elektraKeysetDel (config);
				elektraKeyDel (userCutPoint);
				elektraKeyDel (globalConfCutPoint);
				elektraKeyDel (toRemove);
				// replace "user:/config/" with "user:/"
				realPluginConfig = elektraKeysetRenameKeys (pluginConfigWithConfigPrefix, "user:/");
				elektraKeysetDel (pluginConfigWithConfigPrefix);
				slave = elektraPluginOpen (name, modules, elektraKeysetDup (realPluginConfig), parentKey);
				elektraKeysetDel (realPluginConfig);
				if (!slave)
				{
					elektraKeysetDel (configOrig);
					return -1;
				}
				ElektraKey * slaveKey = elektraKeyNew ("/", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, sizeof (Plugin *), ELEKTRA_KEY_VALUE, &slave, ELEKTRA_KEY_END);
				elektraKeyAddBaseName (slaveKey, name);
				elektraKeysetAppendKey (plugins, elektraKeyDup (slaveKey, ELEKTRA_KEY_CP_ALL));
				elektraKeyDel (slaveKey);
			}
		}
		slave->global = global;
		elektraDeferredCallsExecute (slave, deferredCalls);

		if ((op == GET && slave->kdbGet && (slave->kdbGet (slave, returned, parentKey)) == -1) ||
		    (op == SET && slave->kdbSet && (slave->kdbSet (slave, returned, parentKey)) == -1) ||
		    (op == ERR && slave->kdbError && (slave->kdbError (slave, returned, parentKey)) == -1))
		{
			elektraKeysetDel (configOrig);
			return -1;
		}
	}
	elektraKeysetDel (configOrig);
	return 1;
}

int elektraListGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/list"))
	{
		ElektraKeyset * contract =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/list", ELEKTRA_KEY_VALUE, "list plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/list/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/list/exports/open", ELEKTRA_KEY_FUNC, elektraListOpen, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/list/exports/close", ELEKTRA_KEY_FUNC, elektraListClose, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/list/exports/get", ELEKTRA_KEY_FUNC, elektraListGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/list/exports/set", ELEKTRA_KEY_FUNC, elektraListSet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/list/exports/error", ELEKTRA_KEY_FUNC, elektraListError, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/list/exports/addPlugin", ELEKTRA_KEY_FUNC, elektraListAddPlugin, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/list/exports/editPlugin", ELEKTRA_KEY_FUNC, elektraListEditPlugin, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/list/exports/deferredCall", ELEKTRA_KEY_FUNC, elektraListDeferredCall, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/list/exports/mountplugin", ELEKTRA_KEY_FUNC, elektraListMountPlugin, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/list/exports/unmountplugin", ELEKTRA_KEY_FUNC, elektraListUnmountPlugin, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/list/exports/findplugin", ELEKTRA_KEY_FUNC, elektraListFindPlugin, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       elektraKeyNew ("system:/elektra/modules/list/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return 1;
	}
	Placements * placements = elektraPluginGetData (handle);
	ElektraKeyset * config = elektraPluginGetConfig (handle);
	GetPlacements currentPlacement = placements->getCurrent;
	ElektraKeyset * pluginKS = elektraKeysetDup ((placements)->getKS[currentPlacement]);
	elektraKeysetRewind (pluginKS);
	int ret = runPlugins (pluginKS, placements->modules, placements->plugins, elektraKeysetDup (config), returned,
			      elektraPluginGetGlobalKeySet (handle), parentKey, GET, elektraKeysetNext, placements->deferredCalls);
	placements->getCurrent = ((++currentPlacement) % getEnd);
	while (currentPlacement < getEnd && !placements->getPlacements[currentPlacement])
	{
		placements->getCurrent = ((++currentPlacement) % getEnd);
	}
	elektraKeysetDel (pluginKS);
	return ret;
}

int elektraListSet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	Placements * placements = elektraPluginGetData (handle);
	ElektraKeyset * config = elektraPluginGetConfig (handle);
	SetPlacements currentPlacement = placements->setCurrent;
	ElektraKeyset * pluginKS = elektraKeysetDup ((placements)->setKS[currentPlacement]);
	elektraKeysetRewind (pluginKS);
	int ret = 0;
	ret = runPlugins (pluginKS, placements->modules, placements->plugins, elektraKeysetDup (config), returned,
			  elektraPluginGetGlobalKeySet (handle), parentKey, SET, elektraKeysetPop, placements->deferredCalls);
	placements->setCurrent = ((++currentPlacement) % setEnd);
	while (currentPlacement < setEnd && !placements->setPlacements[currentPlacement])
	{
		placements->setCurrent = ((++currentPlacement) % setEnd);
	}
	elektraPluginSetData (handle, placements);
	elektraKeysetDel (pluginKS);

	return ret;
}

int elektraListError (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	Placements * placements = elektraPluginGetData (handle);
	ElektraKeyset * config = elektraPluginGetConfig (handle);
	ErrPlacements currentPlacement = placements->errCurrent;
	ElektraKeyset * pluginKS = elektraKeysetDup ((placements)->errKS[currentPlacement]);
	elektraKeysetRewind (pluginKS);
	int ret = runPlugins (pluginKS, placements->modules, placements->plugins, elektraKeysetDup (config), returned,
			      elektraPluginGetGlobalKeySet (handle), parentKey, ERR, elektraKeysetPop, placements->deferredCalls);
	placements->errCurrent = ((++currentPlacement) % errEnd);
	while (currentPlacement < errEnd && !placements->errPlacements[currentPlacement])
	{
		placements->errCurrent = ((++currentPlacement) % errEnd);
	}
	elektraKeysetDel (pluginKS);
	return ret;
}

int elektraListAddPlugin (Plugin * handle, ElektraKeyset * pluginConfig)
{
	if (!pluginConfig)
	{
		return 0;
	}
	elektraKeysetRewind (pluginConfig);
	elektraKeysetNext (pluginConfig);
	ElektraKey * lookup = elektraKeysetNext (pluginConfig);
	if (elektraKeyBaseName (lookup)[0] != '#')
	{
		return -1;
	}
	else
	{
		if (strcmp (lastIndex, elektraKeyBaseName (lookup)) >= 0)
		{
			return -1;
		}
	}
	Placements * placements = elektraPluginGetData (handle);
	ElektraKeyset * conf = elektraKeysetDup (pluginConfig);
	elektraKeysetRewind (conf);
	int rc = listParseConfiguration (placements, conf);
	elektraKeysetDel (conf);
	return rc;
}

static char * getPluginPlacementList (Plugin * plugin)
{
	ELEKTRA_NOT_NULL (plugin);

	// Get placements from plugin
	ElektraKey * pluginInfo = elektraKeyNew ("system:/elektra/modules/", ELEKTRA_KEY_END);
	elektraKeyAddBaseName (pluginInfo, plugin->name);
	ElektraKeyset * ksResult = elektraKeysetNew (0, ELEKTRA_KS_END);
	plugin->kdbGet (plugin, ksResult, pluginInfo);

	ElektraKey * placementsKey = elektraKeyDup (pluginInfo, ELEKTRA_KEY_CP_ALL);
	elektraKeyAddBaseName (placementsKey, "infos");
	elektraKeyAddBaseName (placementsKey, "placements");
	ElektraKey * placements = elektraKeysetLookup (ksResult, placementsKey, 0);
	if (placements == NULL)
	{
		ELEKTRA_LOG_WARNING ("could not read placements from plugin");
		return 0;
	}
	char * placementList = elektraStrDup (elektraKeyString (placements));

	elektraKeyDel (pluginInfo);
	elektraKeyDel (placementsKey);
	elektraKeysetDel (ksResult);

	return placementList;
}

static char * extractGetPlacements (const char * placementList)
{
	char * result = elektraMalloc (strlen (placementList) + 1);
	result[0] = '\0';
	char * resultPos = result;
	const char * last = placementList;
	const char * placement = strchr (last, ' ');
	while (placement != NULL)
	{
		size_t len = placement - last;
		if (strncasecmp (last, GlobalpluginPositionsStr[GETRESOLVER], len) == 0 ||
		    strncasecmp (last, GlobalpluginPositionsStr[PREGETSTORAGE], len) == 0 ||
		    strncasecmp (last, GlobalpluginPositionsStr[GETSTORAGE], len) == 0 ||
		    strncasecmp (last, GlobalpluginPositionsStr[PROCGETSTORAGE], len) == 0 ||
		    strncasecmp (last, GlobalpluginPositionsStr[POSTGETSTORAGE], len) == 0 ||
		    strncasecmp (last, GlobalpluginPositionsStr[POSTGETCLEANUP], len) == 0)
		{
			strncpy (resultPos, last, len);
			resultPos[len] = ' ';
			resultPos += len + 1;
		}

		last = placement + 1;
		placement = strchr (last, ' ');
	}

	if (strcasecmp (last, GlobalpluginPositionsStr[GETRESOLVER]) == 0 ||
	    strcasecmp (last, GlobalpluginPositionsStr[PREGETSTORAGE]) == 0 ||
	    strcasecmp (last, GlobalpluginPositionsStr[GETSTORAGE]) == 0 ||
	    strcasecmp (last, GlobalpluginPositionsStr[PROCGETSTORAGE]) == 0 ||
	    strcasecmp (last, GlobalpluginPositionsStr[POSTGETSTORAGE]) == 0 ||
	    strcasecmp (last, GlobalpluginPositionsStr[POSTGETCLEANUP]) == 0)
	{
		strcpy (resultPos, last);
		resultPos += strlen (last);
	}

	*resultPos = '\0';
	return result;
}

static char * extractSetPlacements (const char * placementList)
{
	char * result = elektraMalloc (strlen (placementList) + 1);
	result[0] = '\0';
	char * resultPos = result;
	const char * last = placementList;
	const char * placement = strchr (last, ' ');
	while (placement != NULL)
	{
		size_t len = placement - last;
		if (strncasecmp (last, GlobalpluginPositionsStr[SETRESOLVER], len) == 0 ||
		    strncasecmp (last, GlobalpluginPositionsStr[PRESETSTORAGE], len) == 0 ||
		    strncasecmp (last, GlobalpluginPositionsStr[SETSTORAGE], len) == 0 ||
		    strncasecmp (last, GlobalpluginPositionsStr[PRESETCLEANUP], len) == 0 ||
		    strncasecmp (last, GlobalpluginPositionsStr[PRECOMMIT], len) == 0 ||
		    strncasecmp (last, GlobalpluginPositionsStr[COMMIT], len) == 0 ||
		    strncasecmp (last, GlobalpluginPositionsStr[POSTCOMMIT], len) == 0)
		{
			strncpy (resultPos, last, len);
			resultPos[len] = ' ';
			resultPos += len + 1;
		}

		last = placement + 1;
		placement = strchr (last, ' ');
	}

	if (strcasecmp (last, GlobalpluginPositionsStr[SETRESOLVER]) == 0 ||
	    strcasecmp (last, GlobalpluginPositionsStr[PRESETSTORAGE]) == 0 ||
	    strcasecmp (last, GlobalpluginPositionsStr[SETSTORAGE]) == 0 ||
	    strcasecmp (last, GlobalpluginPositionsStr[PRESETCLEANUP]) == 0 ||
	    strcasecmp (last, GlobalpluginPositionsStr[PRECOMMIT]) == 0 || strcasecmp (last, GlobalpluginPositionsStr[COMMIT]) == 0 ||
	    strcasecmp (last, GlobalpluginPositionsStr[POSTCOMMIT]) == 0)
	{
		strcpy (resultPos, last);
		resultPos += strlen (last);
	}

	*resultPos = '\0';
	return result;
}

static char * extractErrorPlacements (const char * placementList)
{
	char * result = elektraMalloc (strlen (placementList) + 1);
	result[0] = '\0';
	char * resultPos = result;
	const char * last = placementList;
	const char * placement = strchr (last, ' ');
	while (placement != NULL)
	{
		size_t len = placement - last;
		if (strncasecmp (last, GlobalpluginPositionsStr[PREROLLBACK], len) == 0 ||
		    strncasecmp (last, GlobalpluginPositionsStr[ROLLBACK], len) == 0 ||
		    strncasecmp (last, GlobalpluginPositionsStr[POSTROLLBACK], len) == 0)
		{
			strncpy (resultPos, last, len);
			resultPos[len] = ' ';
			resultPos += len + 1;
		}

		last = placement + 1;
		placement = strchr (last, ' ');
	}

	if (strcasecmp (last, GlobalpluginPositionsStr[PREROLLBACK]) == 0 || strcasecmp (last, GlobalpluginPositionsStr[ROLLBACK]) == 0 ||
	    strcasecmp (last, GlobalpluginPositionsStr[POSTROLLBACK]) == 0)
	{
		strcpy (resultPos, last);
		resultPos += strlen (last);
	}

	*resultPos = '\0';
	return result;
}

static ElektraKey * findPluginInConfig (ElektraKeyset * config, const char * pluginName)
{
	ElektraKey * configBase = elektraKeyNew ("user:/plugins", ELEKTRA_KEY_END);
	ElektraKeyset * array = elektraArrayGet (configBase, config);

	elektraKeysetRewind (array);
	ElektraKey * cur = NULL;
	while ((cur = elektraKeysetNext (array)) != NULL)
	{
		if (strcmp (elektraKeyString (cur), pluginName) == 0)
		{
			// found plugin
			ElektraKey * result = elektraKeyDup (cur, ELEKTRA_KEY_CP_ALL);
			elektraKeyDel (configBase);
			elektraKeysetDel (array);
			return result;
		}
	}

	elektraKeyDel (configBase);
	elektraKeysetDel (array);
	return NULL;
}

static void resetPlugins (Plugin * handle, ElektraKey * errorKey)
{
	Placements * placements = elektraPluginGetData (handle);
	elektraKeysetClear (placements->getKS[0]);
	elektraKeysetClear (placements->getKS[1]);
	elektraKeysetClear (placements->getKS[2]);
	elektraKeysetClear (placements->setKS[0]);
	elektraKeysetClear (placements->setKS[1]);
	elektraKeysetClear (placements->setKS[2]);
	elektraKeysetClear (placements->setKS[3]);
	elektraKeysetClear (placements->errKS[0]);
	elektraKeysetClear (placements->errKS[1]);
	ElektraKey * cur;
	elektraKeysetRewind (placements->plugins);
	while ((cur = elektraKeysetNext (placements->plugins)) != NULL)
	{
		Plugin * slave;
		slave = *(Plugin **) elektraKeyValue (cur);
		elektraPluginClose (slave, errorKey);
	}
	elektraKeysetClear (placements->plugins);
}

/**
 * Adds a plugin in all the intended positions (given in its infos/placements key).
 * If the plugin is already added, effectively equivalent to calling ksDel() on pluginConfig.
 *
 * @param handle       A handle of the list plugin
 * @param pluginName   The plugin to add
 * @param pluginConfig The config for the plugin, if it has to be mounted; the KeySet is consumed,
 *                     don't call ksDel() on it afterwards.
 * @param errorKey     Used for error reporting
 *
 * @retval #ELEKTRA_PLUGIN_STATUS_SUCCESS   if the plugin was added
 * @retval #ELEKTRA_PLUGIN_STATUS_NO_UPDATE if the plugin was added already
 * @retval #ELEKTRA_PLUGIN_STATUS_ERROR     on NULL pointers and other errors
 */
int elektraListMountPlugin (Plugin * handle, const char * pluginName, ElektraKeyset * pluginConfig, ElektraKey * errorKey)
{
	if (handle == NULL || pluginName == NULL || pluginConfig == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Placements * placements = elektraPluginGetData (handle);
	ElektraKeyset * config = elektraPluginGetConfig (handle);

	// check if plugin already added
	ElektraKey * pluginKey = findPluginInConfig (config, pluginName);
	if (pluginKey != NULL)
	{
		elektraKeyDel (pluginKey);
		elektraKeysetDel (pluginConfig); // consume config
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}

	// Find name for next item in plugins array
	ElektraKey * configBase = elektraKeyNew ("user:/plugins", ELEKTRA_KEY_END);
	ElektraKeyset * array = elektraArrayGet (configBase, config);
	ElektraKey * pluginItem = elektraArrayGetNextKey (array);

	if (pluginItem == NULL)
	{
		pluginItem = elektraKeyNew ("user:/plugins/#0", ELEKTRA_KEY_END);
	}

	elektraKeySetString (pluginItem, pluginName);

	elektraKeyDel (configBase);
	elektraKeysetDel (array);

	Plugin * plugin = elektraPluginOpen (pluginName, placements->modules, pluginConfig, errorKey);

	if (plugin == NULL)
	{
		elektraKeyDel (pluginItem);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// Store key with plugin handle
	ElektraKey * searchKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeyAddBaseName (searchKey, pluginName);
	elektraKeySetBinary (searchKey, &plugin, sizeof (plugin));

	// Find plugin placements
	char * placementList = getPluginPlacementList (plugin);

	ElektraKey * pluginPlacements = elektraKeyDup (pluginItem, ELEKTRA_KEY_CP_ALL);
	elektraKeyAddBaseName (pluginPlacements, "placements");

	// Append keys to list plugin configuration
	elektraKeysetAppendKey (config, pluginItem);
	elektraKeysetAppendKey (config, pluginPlacements);

	// Add get placements
	char * getPlacementsString = extractGetPlacements (placementList);
	if (getPlacementsString != NULL)
	{
		ElektraKey * getPlacements = elektraKeyDup (pluginPlacements, ELEKTRA_KEY_CP_ALL);
		elektraKeyAddBaseName (getPlacements, "get");
		elektraKeySetString (getPlacements, getPlacementsString);
		elektraKeysetAppendKey (config, getPlacements);
	}
	elektraFree (getPlacementsString);


	// Add set placements
	char * setPlacementsString = extractSetPlacements (placementList);
	if (setPlacementsString != NULL)
	{
		ElektraKey * setPlacements = elektraKeyDup (pluginPlacements, ELEKTRA_KEY_CP_ALL);
		elektraKeyAddBaseName (setPlacements, "set");
		elektraKeySetString (setPlacements, setPlacementsString);
		elektraKeysetAppendKey (config, setPlacements);
	}
	elektraFree (setPlacementsString);

	// Add error placements
	char * errorPlacementsString = extractErrorPlacements (placementList);
	if (errorPlacementsString != NULL)
	{
		ElektraKey * errorPlacements = elektraKeyDup (pluginPlacements, ELEKTRA_KEY_CP_ALL);
		elektraKeyAddBaseName (errorPlacements, "error");
		elektraKeySetString (errorPlacements, errorPlacementsString);
		elektraKeysetAppendKey (config, errorPlacements);
	}
	elektraFree (errorPlacementsString);
	elektraFree (placementList);

	// reload configuration
	resetPlugins (handle, errorKey);

	// store new handle
	elektraKeysetAppendKey (placements->plugins, searchKey);
	return elektraListOpen (handle, errorKey);
}

/**
 * Removes a plugin from all the intended positions (given in its infos/placements key).
 * If the plugin isn't present, nothing happens.
 *
 * @param handle       A handle of the list plugin
 * @param pluginName   The plugin to remove
 * @param errorKey     Used for error reporting
 *
 * @retval #ELEKTRA_PLUGIN_STATUS_SUCCESS   if the plugin was added
 * @retval #ELEKTRA_PLUGIN_STATUS_NO_UPDATE if the plugin was added already
 * @retval #ELEKTRA_PLUGIN_STATUS_ERROR     on NULL pointers and other errors
 */
int elektraListUnmountPlugin (Plugin * handle, const char * pluginName, ElektraKey * errorKey)
{
	if (handle == NULL || pluginName == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Placements * placements = elektraPluginGetData (handle);
	ElektraKeyset * config = elektraPluginGetConfig (handle);

	// Find plugin
	ElektraKey * pluginItem = findPluginInConfig (config, pluginName);
	if (pluginItem == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}

	// Look for plugin via handle
	ElektraKey * pluginHandle = elektraKeyDup (pluginItem, ELEKTRA_KEY_CP_ALL);
	elektraKeyAddName (pluginHandle, "handle");
	pluginHandle = elektraKeysetLookup (config, pluginHandle, ELEKTRA_KDB_O_DEL);

	// unload plugin if loaded via handle
	if (pluginHandle != NULL)
	{
		elektraPluginClose (*((Plugin **) elektraKeyValue (pluginHandle)), errorKey);
		elektraKeyDel (pluginHandle);
	}

	// Look for plugin via plugins
	ElektraKey * searchKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeyAddBaseName (searchKey, pluginName);

	// pop if found
	searchKey = elektraKeysetLookup (placements->plugins, searchKey, ELEKTRA_KDB_O_DEL | ELEKTRA_KDB_O_POP);

	// unload plugin if loaded via plugins
	if (searchKey != NULL)
	{
		elektraPluginClose (*((Plugin **) elektraKeyValue (searchKey)), errorKey);
		elektraKeyDel (searchKey);
	}

	// Remove plugin data from config
	elektraKeysetDel (elektraKeysetCut (config, pluginItem));
	elektraKeyDel (pluginItem);

	// reload configuration
	resetPlugins (handle, errorKey);
	return elektraListOpen (handle, errorKey);
}

/**
 * Find the handle of plugin.
 *
 * If elektraListGet(), elektraListSet() and elektraListError()
 * haven't been called yet, only plugins added via elektraListMountPlugin()
 * will be found. Other plugins aren't opened (and therefore don't have a handle)
 * before get/set/error is called.
 *
 * @param handle     A handle of the list plugin
 * @param pluginName The name of the plugin to look for
 *
 * @return the handle for the given plugin, or NULL if not found
 * NULL is also returned if @p handle or @p pluginName are NULL
 */
Plugin * elektraListFindPlugin (Plugin * handle, const char * pluginName)
{
	if (handle == NULL || pluginName == NULL)
	{
		return NULL;
	}

	Placements * placements = elektraPluginGetData (handle);
	ElektraKeyset * config = elektraPluginGetConfig (handle);

	ElektraKey * searchKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeyAddBaseName (searchKey, pluginName);
	ElektraKey * lookup = elektraKeysetLookup (placements->plugins, searchKey, ELEKTRA_KDB_O_DEL);
	if (lookup)
	{
		return *(Plugin **) elektraKeyValue (lookup);
	}


	ElektraKey * current;
	for (int i = 0; i < getEnd; ++i)
	{
		while ((current = elektraKeysetNext (placements->getKS[i])) != NULL)
		{
			ElektraKey * handleKey = elektraKeyDup (current, ELEKTRA_KEY_CP_ALL);
			elektraKeyAddName (handleKey, "handle");
			ElektraKey * handleLookup = elektraKeysetLookup (config, handleKey, ELEKTRA_KDB_O_DEL);
			if (handleLookup)
			{
				return *(Plugin **) elektraKeyValue (handleLookup);
			}
		}
	}

	for (int i = 0; i < setEnd; ++i)
	{
		while ((current = elektraKeysetNext (placements->setKS[i])) != NULL)
		{
			ElektraKey * handleKey = elektraKeyDup (current, ELEKTRA_KEY_CP_ALL);
			elektraKeyAddName (handleKey, "handle");
			ElektraKey * handleLookup = elektraKeysetLookup (config, handleKey, ELEKTRA_KDB_O_DEL);
			if (handleLookup)
			{
				return *(Plugin **) elektraKeyValue (handleLookup);
			}
		}
	}

	for (int i = 0; i < errEnd; ++i)
	{
		while ((current = elektraKeysetNext (placements->errKS[i])) != NULL)
		{
			ElektraKey * handleKey = elektraKeyDup (current, ELEKTRA_KEY_CP_ALL);
			elektraKeyAddName (handleKey, "handle");
			ElektraKey * handleLookup = elektraKeysetLookup (config, handleKey, ELEKTRA_KDB_O_DEL);
			if (handleLookup)
			{
				return *(Plugin **) elektraKeyValue (handleLookup);
			}
		}
	}

	return NULL;
}

int elektraListEditPlugin (Plugin * handle, ElektraKeyset * pluginConfig)
{
	if (!pluginConfig)
	{
		return 0;
	}
	elektraKeysetRewind (pluginConfig);
	elektraKeysetNext (pluginConfig);
	ElektraKey * lookup = elektraKeysetNext (pluginConfig);
	if (elektraKeyBaseName (lookup)[0] != '#')
	{
		return -1;
	}
	else
	{
		if (strcmp (lastIndex, elektraKeyBaseName (lookup)) < 0)
		{
			return -1;
		}
	}
	Placements * placements = elektraPluginGetData (handle);
	ElektraKeyset * conf = elektraKeysetDup (pluginConfig);
	elektraKeysetRewind (conf);
	int rc = listParseConfiguration (placements, conf);
	elektraKeysetDel (conf);
	return rc;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("list",
			ELEKTRA_PLUGIN_OPEN,	&elektraListOpen,
			ELEKTRA_PLUGIN_CLOSE,	&elektraListClose,
			ELEKTRA_PLUGIN_GET,	&elektraListGet,
			ELEKTRA_PLUGIN_SET,	&elektraListSet,
			ELEKTRA_PLUGIN_ERROR,	&elektraListError,
			ELEKTRA_PLUGIN_END);
}
