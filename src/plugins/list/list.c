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
	KeySet * setKS[4];
	KeySet * errKS[2];
	KeySet * getKS[4];
	KeySet * plugins;
	KeySet * modules;

	ElektraDeferredCallList * deferredCalls;

} Placements;

static char lastIndex[ELEKTRA_MAX_ARRAY_SIZE];

static int listParseConfiguration (Placements * placements, KeySet * config)
{
	Key * cur;
	Key * key = ksLookupByName (config, "/plugins", 0);
	KeySet * cutKS = ksCut (config, key);
	ksRewind (cutKS);
	if (ksGetSize (cutKS) < 2)
	{
		ksDel (cutKS);
		return 0;
	}
	int rc = 0;
	while ((cur = ksNext (cutKS)) != NULL)
	{
		if (keyIsDirectlyBelow (key, cur) != 1)
		{
			continue;
		}
		if (keyBaseName (cur)[0] == '#')
		{
			if (strcmp (lastIndex, keyBaseName (cur)) < 0)
			{
				snprintf (lastIndex, ELEKTRA_MAX_ARRAY_SIZE, "%s", keyBaseName (cur));
			}
		}
		Key * sub;
		Key * lookup = keyDup (cur);
		keyAddBaseName (lookup, "placements");
		keyAddBaseName (lookup, "set");
		sub = ksLookup (cutKS, lookup, 0);
		if (sub)
		{
			const char * setString = keyString (sub);
			SetPlacements setPlacement = preSetStorage;
			while (setPlacement != setEnd)
			{
				if (strstr (setString, setStrings[setPlacement]))
				{
					rc = 1;
					ksAppendKey (placements->setKS[setPlacement], keyDup (cur));
				}
				++setPlacement;
			}
		}
		keySetBaseName (lookup, "get");
		sub = ksLookup (cutKS, lookup, 0);
		if (sub)
		{
			const char * getString = keyString (sub);
			GetPlacements getPlacement = preGetStorage;
			while (getPlacement != getEnd)
			{
				if (strstr (getString, getStrings[getPlacement]))
				{
					rc = 1;
					ksAppendKey (placements->getKS[getPlacement], keyDup (cur));
				}
				++getPlacement;
			}
		}
		keySetBaseName (lookup, "error");
		sub = ksLookup (cutKS, lookup, 0);
		if (sub)
		{
			const char * errString = keyString (sub);
			ErrPlacements errPlacement = preRollback;
			while (errPlacement != errEnd)
			{
				if (strstr (errString, errStrings[errPlacement]))
				{
					rc = 1;
					ksAppendKey (placements->errKS[errPlacement], keyDup (cur));
				}
				++errPlacement;
			}
		}
		keyDel (lookup);
	}
	ksDel (cutKS);
	return rc;
}

void elektraListDeferredCall (Plugin * plugin, const char * name, KeySet * parameters)
{
	Placements * placements = elektraPluginGetData (plugin);
	ELEKTRA_NOT_NULL (placements);
	elektraDeferredCallAdd (placements->deferredCalls, name, parameters);

	// Execute call immediately on already loaded plugins
	ksRewind (placements->plugins);
	Key * current;
	while ((current = ksNext (placements->plugins)) != NULL)
	{
		Plugin * slave;
		slave = *(Plugin **) keyValue (current);
		elektraDeferredCallsExecute (slave, placements->deferredCalls);
	}
}

int elektraListOpen (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{

	Placements * placements = (Placements *) elektraPluginGetData (handle);
	if (!placements)
	{
		placements = (Placements *) elektraMalloc (sizeof (Placements));
		memset (placements, 0, sizeof (Placements));
		placements->errCurrent = preRollback;
		placements->setCurrent = preSetStorage;
		placements->getCurrent = preGetStorage;
		placements->getKS[0] = ksNew (0, KS_END);
		placements->getKS[1] = ksNew (0, KS_END);
		placements->getKS[2] = ksNew (0, KS_END);
		placements->setKS[0] = ksNew (0, KS_END);
		placements->setKS[1] = ksNew (0, KS_END);
		placements->setKS[2] = ksNew (0, KS_END);
		placements->setKS[3] = ksNew (0, KS_END);
		placements->errKS[0] = ksNew (0, KS_END);
		placements->errKS[1] = ksNew (0, KS_END);
		placements->plugins = ksNew (0, KS_END);
		placements->modules = ksNew (0, KS_END);
		placements->deferredCalls = elektraDeferredCallCreateList ();
	}
	elektraPluginSetData (handle, placements);

	elektraModulesInit (placements->modules, NULL);
	KeySet * config = ksDup (elektraPluginGetConfig (handle));
	ksRewind (config);
	Key * key = ksLookupByName (config, "/placements/set", 0);
	if (key)
	{
		const char * setString = keyString (key);
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
	key = ksLookupByName (config, "/placements/get", 0);
	if (key)
	{
		const char * getString = keyString (key);
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
	key = ksLookupByName (config, "/placements/error", 0);
	if (key)
	{
		const char * errString = keyString (key);
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
	ksDel (config);
	return 1; /* success */
}

int elektraListClose (Plugin * handle, Key * errorKey)
{
	/* free all plugin resources and shut it down */
	Placements * placements = elektraPluginGetData (handle);
	ksDel (placements->getKS[0]);
	ksDel (placements->getKS[1]);
	ksDel (placements->getKS[2]);
	ksDel (placements->setKS[0]);
	ksDel (placements->setKS[1]);
	ksDel (placements->setKS[2]);
	ksDel (placements->setKS[3]);
	ksDel (placements->errKS[0]);
	ksDel (placements->errKS[1]);
	Key * cur;
	ksRewind (placements->plugins);
	while ((cur = ksNext (placements->plugins)) != NULL)
	{
		Plugin * slave;
		slave = *(Plugin **) keyValue (cur);
		elektraPluginClose (slave, errorKey);
	}
	ksDel (placements->plugins);
	elektraModulesClose (placements->modules, NULL);
	ksDel (placements->modules);
	elektraDeferredCallDeleteList (placements->deferredCalls);
	elektraFree (placements);
	elektraPluginSetData (handle, 0);
	return 1; /* success */
}

static int runPlugins (KeySet * pluginKS, KeySet * modules, KeySet * plugins, KeySet * configOrig, KeySet * returned, KeySet * global,
		       Key * parentKey, OP op, Key * (*traversalFunction) (KeySet *), ElektraDeferredCallList * deferredCalls)
{
	Key * current;

	Plugin * slave = NULL;

	// for every plugin in our list: load it, run the expected function (set/get/error) and close it again
	KeySet * realPluginConfig = NULL;
	while ((current = traversalFunction (pluginKS)) != NULL)
	{
		const char * name = keyString (current);

		Key * handleKey = keyDup (current);
		keyAddName (handleKey, "handle");
		Key * handleLookup = ksLookup (configOrig, handleKey, 0);
		keyDel (handleKey);
		if (handleLookup)
		{
			slave = *(Plugin **) keyValue (handleLookup);
		}
		else
		{
			Key * searchKey = keyNew ("/", KEY_END);
			keyAddBaseName (searchKey, name);
			Key * lookup = ksLookup (plugins, searchKey, 0);
			keyDel (searchKey);
			if (lookup)
			{
				slave = *(Plugin **) keyValue (lookup);
			}
			else
			{
				Key * userCutPoint = keyNew ("user", 0);
				Key * globalConfCutPoint = keyNew ("/config", 0);
				KeySet * config = ksDup (configOrig);
				KeySet * globalConfigAll = ksCut (config, globalConfCutPoint);
				KeySet * userConfigAll = ksCut (config, userCutPoint);
				KeySet * pluginConfig = ksCut (userConfigAll, current);
				// replace "user/plugins/#X" with "user/"
				KeySet * pluginConfigWithConfigPrefix = elektraRenameKeys (pluginConfig, "user");
				ksDel (pluginConfig);
				// append config below "/config" to all plugins
				KeySet * globalPluginConfig = elektraRenameKeys (globalConfigAll, "user/config");
				ksAppend (pluginConfigWithConfigPrefix, globalPluginConfig);
				ksDel (globalPluginConfig);
				// remove "placements" from plugin config
				Key * toRemove = keyNew ("user/placements", 0);
				ksDel (ksCut (pluginConfigWithConfigPrefix, toRemove));
				ksRewind (pluginConfigWithConfigPrefix);
				ksDel (globalConfigAll);
				ksDel (userConfigAll);
				ksDel (config);
				keyDel (userCutPoint);
				keyDel (globalConfCutPoint);
				keyDel (toRemove);
				// replace "user/config/" with "user/"
				realPluginConfig = elektraRenameKeys (pluginConfigWithConfigPrefix, "user");
				ksDel (pluginConfigWithConfigPrefix);
				slave = elektraPluginOpen (name, modules, ksDup (realPluginConfig), parentKey);
				ksDel (realPluginConfig);
				if (!slave)
				{
					ksDel (configOrig);
					return -1;
				}
				slave->global = global;
				Key * slaveKey = keyNew (name, KEY_BINARY, KEY_SIZE, sizeof (Plugin *), KEY_VALUE, &slave, KEY_END);
				keySetName (slaveKey, "/");
				keyAddBaseName (slaveKey, name);
				ksAppendKey (plugins, keyDup (slaveKey));
				keyDel (slaveKey);
			}
		}
		elektraDeferredCallsExecute (slave, deferredCalls);

		if ((op == GET && slave->kdbGet && (slave->kdbGet (slave, returned, parentKey)) == -1) ||
		    (op == SET && slave->kdbSet && (slave->kdbSet (slave, returned, parentKey)) == -1) ||
		    (op == ERR && slave->kdbError && (slave->kdbError (slave, returned, parentKey)) == -1))
		{
			ksDel (configOrig);
			return -1;
		}
	}
	ksDel (configOrig);
	return 1;
}

int elektraListGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!strcmp (keyName (parentKey), "system/elektra/modules/list"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/list", KEY_VALUE, "list plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/list/exports", KEY_END),
			       keyNew ("system/elektra/modules/list/exports/open", KEY_FUNC, elektraListOpen, KEY_END),
			       keyNew ("system/elektra/modules/list/exports/close", KEY_FUNC, elektraListClose, KEY_END),
			       keyNew ("system/elektra/modules/list/exports/get", KEY_FUNC, elektraListGet, KEY_END),
			       keyNew ("system/elektra/modules/list/exports/set", KEY_FUNC, elektraListSet, KEY_END),
			       keyNew ("system/elektra/modules/list/exports/error", KEY_FUNC, elektraListError, KEY_END),
			       keyNew ("system/elektra/modules/list/exports/addPlugin", KEY_FUNC, elektraListAddPlugin, KEY_END),
			       keyNew ("system/elektra/modules/list/exports/editPlugin", KEY_FUNC, elektraListEditPlugin, KEY_END),
			       keyNew ("system/elektra/modules/list/exports/deferredCall", KEY_FUNC, elektraListDeferredCall, KEY_END),
			       keyNew ("system/elektra/modules/list/exports/mountplugin", KEY_FUNC, elektraListMountPlugin, KEY_END),
			       keyNew ("system/elektra/modules/list/exports/unmountplugin", KEY_FUNC, elektraListUnmountPlugin, KEY_END),
			       keyNew ("system/elektra/modules/list/exports/findplugin", KEY_FUNC, elektraListFindPlugin, KEY_END),
#include ELEKTRA_README
			       keyNew ("system/elektra/modules/list/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1;
	}
	Placements * placements = elektraPluginGetData (handle);
	KeySet * config = elektraPluginGetConfig (handle);
	GetPlacements currentPlacement = placements->getCurrent;
	KeySet * pluginKS = ksDup ((placements)->getKS[currentPlacement]);
	ksRewind (pluginKS);
	int ret = runPlugins (pluginKS, placements->modules, placements->plugins, ksDup (config), returned,
			      elektraPluginGetGlobalKeySet (handle), parentKey, GET, ksNext, placements->deferredCalls);
	placements->getCurrent = ((++currentPlacement) % getEnd);
	while (currentPlacement < getEnd && !placements->getPlacements[currentPlacement])
	{
		placements->getCurrent = ((++currentPlacement) % getEnd);
	}
	ksDel (pluginKS);
	return ret;
}

int elektraListSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	Placements * placements = elektraPluginGetData (handle);
	KeySet * config = elektraPluginGetConfig (handle);
	SetPlacements currentPlacement = placements->setCurrent;
	KeySet * pluginKS = ksDup ((placements)->setKS[currentPlacement]);
	ksRewind (pluginKS);
	int ret = 0;
	ret = runPlugins (pluginKS, placements->modules, placements->plugins, ksDup (config), returned,
			  elektraPluginGetGlobalKeySet (handle), parentKey, SET, ksPop, placements->deferredCalls);
	placements->setCurrent = ((++currentPlacement) % setEnd);
	while (currentPlacement < setEnd && !placements->setPlacements[currentPlacement])
	{
		placements->setCurrent = ((++currentPlacement) % setEnd);
	}
	elektraPluginSetData (handle, placements);
	ksDel (pluginKS);

	return ret;
}

int elektraListError (Plugin * handle, KeySet * returned, Key * parentKey)
{
	Placements * placements = elektraPluginGetData (handle);
	KeySet * config = elektraPluginGetConfig (handle);
	ErrPlacements currentPlacement = placements->errCurrent;
	KeySet * pluginKS = ksDup ((placements)->errKS[currentPlacement]);
	ksRewind (pluginKS);
	int ret = runPlugins (pluginKS, placements->modules, placements->plugins, ksDup (config), returned,
			      elektraPluginGetGlobalKeySet (handle), parentKey, ERR, ksPop, placements->deferredCalls);
	placements->errCurrent = ((++currentPlacement) % errEnd);
	while (currentPlacement < errEnd && !placements->errPlacements[currentPlacement])
	{
		placements->errCurrent = ((++currentPlacement) % errEnd);
	}
	ksDel (pluginKS);
	return ret;
}

int elektraListAddPlugin (Plugin * handle, KeySet * pluginConfig)
{
	if (!pluginConfig)
	{
		return 0;
	}
	ksRewind (pluginConfig);
	ksNext (pluginConfig);
	Key * lookup = ksNext (pluginConfig);
	if (keyBaseName (lookup)[0] != '#')
	{
		return -1;
	}
	else
	{
		if (strcmp (lastIndex, keyBaseName (lookup)) >= 0)
		{
			return -1;
		}
	}
	Placements * placements = elektraPluginGetData (handle);
	KeySet * conf = ksDup (pluginConfig);
	ksRewind (conf);
	int rc = listParseConfiguration (placements, conf);
	ksDel (conf);
	return rc;
}

static char * getPluginPlacementList (Plugin * plugin)
{
	ELEKTRA_NOT_NULL (plugin);

	// Get placements from plugin
	Key * pluginInfo = keyNew ("system/elektra/modules/", KEY_END);
	keyAddBaseName (pluginInfo, plugin->name);
	KeySet * ksResult = ksNew (0, KS_END);
	plugin->kdbGet (plugin, ksResult, pluginInfo);

	Key * placementsKey = keyDup (pluginInfo);
	keyAddBaseName (placementsKey, "infos");
	keyAddBaseName (placementsKey, "placements");
	Key * placements = ksLookup (ksResult, placementsKey, 0);
	if (placements == NULL)
	{
		ELEKTRA_LOG_WARNING ("could not read placements from plugin");
		return 0;
	}
	char * placementList = elektraStrDup (keyString (placements));

	keyDel (pluginInfo);
	keyDel (placementsKey);
	ksDel (ksResult);

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

static Key * findPluginInConfig (KeySet * config, const char * pluginName)
{
	Key * configBase = keyNew ("user/plugins", KEY_END);
	KeySet * array = elektraArrayGet (configBase, config);

	ksRewind (array);
	Key * cur = NULL;
	while ((cur = ksNext (array)) != NULL)
	{
		if (strcmp (keyString (cur), pluginName) == 0)
		{
			// found plugin
			Key * result = keyDup (cur);
			keyDel (configBase);
			ksDel (array);
			return result;
		}
	}

	keyDel (configBase);
	ksDel (array);
	return NULL;
}

static void resetPlugins (Plugin * handle, Key * errorKey)
{
	Placements * placements = elektraPluginGetData (handle);
	ksClear (placements->getKS[0]);
	ksClear (placements->getKS[1]);
	ksClear (placements->getKS[2]);
	ksClear (placements->setKS[0]);
	ksClear (placements->setKS[1]);
	ksClear (placements->setKS[2]);
	ksClear (placements->setKS[3]);
	ksClear (placements->errKS[0]);
	ksClear (placements->errKS[1]);
	Key * cur;
	ksRewind (placements->plugins);
	while ((cur = ksNext (placements->plugins)) != NULL)
	{
		Plugin * slave;
		slave = *(Plugin **) keyValue (cur);
		elektraPluginClose (slave, errorKey);
	}
	ksClear (placements->plugins);
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
int elektraListMountPlugin (Plugin * handle, const char * pluginName, KeySet * pluginConfig, Key * errorKey)
{
	if (handle == NULL || pluginName == NULL || pluginConfig == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Placements * placements = elektraPluginGetData (handle);
	KeySet * config = elektraPluginGetConfig (handle);

	// check if plugin already added
	Key * pluginKey = findPluginInConfig (config, pluginName);
	if (pluginKey != NULL)
	{
		keyDel (pluginKey);
		ksDel (pluginConfig); // consume config
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}

	// Find name for next item in plugins array
	Key * configBase = keyNew ("user/plugins", KEY_END);
	KeySet * array = elektraArrayGet (configBase, config);
	Key * pluginItem = elektraArrayGetNextKey (array);

	if (pluginItem == NULL)
	{
		pluginItem = keyNew ("user/plugins/#0", KEY_END);
	}

	keySetString (pluginItem, pluginName);

	keyDel (configBase);
	ksDel (array);

	Plugin * plugin = elektraPluginOpen (pluginName, placements->modules, pluginConfig, errorKey);

	if (plugin == NULL)
	{
		keyDel (pluginItem);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// Store key with plugin handle
	Key * searchKey = keyNew ("/", KEY_END);
	keyAddBaseName (searchKey, pluginName);
	keySetBinary (searchKey, &plugin, sizeof (plugin));

	// Find plugin placements
	char * placementList = getPluginPlacementList (plugin);

	Key * pluginPlacements = keyDup (pluginItem);
	keyAddBaseName (pluginPlacements, "placements");

	// Append keys to list plugin configuration
	ksAppendKey (config, pluginItem);
	ksAppendKey (config, pluginPlacements);

	// Add get placements
	char * getPlacementsString = extractGetPlacements (placementList);
	if (getPlacementsString != NULL)
	{
		Key * getPlacements = keyDup (pluginPlacements);
		keyAddBaseName (getPlacements, "get");
		keySetString (getPlacements, getPlacementsString);
		ksAppendKey (config, getPlacements);
	}
	elektraFree (getPlacementsString);


	// Add set placements
	char * setPlacementsString = extractSetPlacements (placementList);
	if (setPlacementsString != NULL)
	{
		Key * setPlacements = keyDup (pluginPlacements);
		keyAddBaseName (setPlacements, "set");
		keySetString (setPlacements, setPlacementsString);
		ksAppendKey (config, setPlacements);
	}
	elektraFree (setPlacementsString);

	// Add error placements
	char * errorPlacementsString = extractErrorPlacements (placementList);
	if (errorPlacementsString != NULL)
	{
		Key * errorPlacements = keyDup (pluginPlacements);
		keyAddBaseName (errorPlacements, "error");
		keySetString (errorPlacements, errorPlacementsString);
		ksAppendKey (config, errorPlacements);
	}
	elektraFree (errorPlacementsString);
	elektraFree (placementList);

	// reload configuration
	resetPlugins (handle, errorKey);

	// store new handle
	ksAppendKey (placements->plugins, searchKey);
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
int elektraListUnmountPlugin (Plugin * handle, const char * pluginName, Key * errorKey)
{
	if (handle == NULL || pluginName == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Placements * placements = elektraPluginGetData (handle);
	KeySet * config = elektraPluginGetConfig (handle);

	// Find plugin
	Key * pluginItem = findPluginInConfig (config, pluginName);
	if (pluginItem == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}

	// Look for plugin via handle
	Key * pluginHandle = keyDup (pluginItem);
	keyAddName (pluginHandle, "handle");
	pluginHandle = ksLookup (config, pluginHandle, KDB_O_DEL);

	// unload plugin if loaded via handle
	if (pluginHandle != NULL)
	{
		elektraPluginClose (*((Plugin **) keyValue (pluginHandle)), errorKey);
		keyDel (pluginHandle);
	}

	// Look for plugin via plugins
	Key * searchKey = keyNew ("/", KEY_END);
	keyAddBaseName (searchKey, pluginName);

	// pop if found
	searchKey = ksLookup (placements->plugins, searchKey, KDB_O_DEL | KDB_O_POP);

	// unload plugin if loaded via plugins
	if (searchKey != NULL)
	{
		elektraPluginClose (*((Plugin **) keyValue (searchKey)), errorKey);
		keyDel (searchKey);
	}

	// Remove plugin data from config
	ksDel (ksCut (config, pluginItem));
	keyDel (pluginItem);

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
	KeySet * config = elektraPluginGetConfig (handle);

	Key * searchKey = keyNew ("/", KEY_END);
	keyAddBaseName (searchKey, pluginName);
	Key * lookup = ksLookup (placements->plugins, searchKey, KDB_O_DEL);
	if (lookup)
	{
		return *(Plugin **) keyValue (lookup);
	}


	Key * current;
	for (int i = 0; i < getEnd; ++i)
	{
		while ((current = ksNext (placements->getKS[i])) != NULL)
		{
			Key * handleKey = keyDup (current);
			keyAddName (handleKey, "handle");
			Key * handleLookup = ksLookup (config, handleKey, KDB_O_DEL);
			if (handleLookup)
			{
				return *(Plugin **) keyValue (handleLookup);
			}
		}
	}

	for (int i = 0; i < setEnd; ++i)
	{
		while ((current = ksNext (placements->setKS[i])) != NULL)
		{
			Key * handleKey = keyDup (current);
			keyAddName (handleKey, "handle");
			Key * handleLookup = ksLookup (config, handleKey, KDB_O_DEL);
			if (handleLookup)
			{
				return *(Plugin **) keyValue (handleLookup);
			}
		}
	}

	for (int i = 0; i < errEnd; ++i)
	{
		while ((current = ksNext (placements->errKS[i])) != NULL)
		{
			Key * handleKey = keyDup (current);
			keyAddName (handleKey, "handle");
			Key * handleLookup = ksLookup (config, handleKey, KDB_O_DEL);
			if (handleLookup)
			{
				return *(Plugin **) keyValue (handleLookup);
			}
		}
	}

	return NULL;
}

int elektraListEditPlugin (Plugin * handle, KeySet * pluginConfig)
{
	if (!pluginConfig)
	{
		return 0;
	}
	ksRewind (pluginConfig);
	ksNext (pluginConfig);
	Key * lookup = ksNext (pluginConfig);
	if (keyBaseName (lookup)[0] != '#')
	{
		return -1;
	}
	else
	{
		if (strcmp (lastIndex, keyBaseName (lookup)) < 0)
		{
			return -1;
		}
	}
	Placements * placements = elektraPluginGetData (handle);
	KeySet * conf = ksDup (pluginConfig);
	ksRewind (conf);
	int rc = listParseConfiguration (placements, conf);
	ksDel (conf);
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
