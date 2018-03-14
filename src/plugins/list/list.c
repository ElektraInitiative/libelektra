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
#include <kdberrors.h>
#include <kdbinternal.h>
#include <kdbmodule.h>
#include <kdbos.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/**
 * Structure for registered key variable pairs
 * @internal
 */
typedef struct _ElektraDeferredCall
{
	char * name;
	KeySet * parameters;
	struct _ElektraDeferredCall * next;
} ElektraDeferredCall;

/**
 * Structure for internal plugin state
 * @internal
 */
typedef struct
{
	ElektraDeferredCall * head;
	ElektraDeferredCall * last;
} ElektraDeferredCallList;

/**
 * Declaration for functions that can be called with elektraDeferredCallsExecute().
 *
 * @param  parameters function parameters
 */
typedef void (*ElektraDeferredCallable) (Plugin * plugin, KeySet * parameters);

typedef enum { preGetStorage = 0, postGetStorage, postGetCleanup, getEnd } GetPlacements;

typedef enum { preSetStorage = 0, preSetCleanup, preCommit, postCommit, setEnd } SetPlacements;

typedef enum { preRollback = 0, postRollback, errEnd } ErrPlacements;

typedef enum { GET, SET, ERR } OP;

typedef struct
{
	// keep track of placements
	ErrPlacements errCurrent;
	SetPlacements setCurrent;
	GetPlacements getCurrent;

	ErrPlacements errPlacements[2]; // prerollback and postrollback
	SetPlacements setPlacements[4]; // presetstorage, presetcleanup, precommit and postcommit
	GetPlacements getPlacements[3]; // pregetstorage, postgetstorage, postgetclenaup

	// each keyset contains the list of plugin names for a given placement
	KeySet * setKS[4];
	KeySet * errKS[2];
	KeySet * getKS[3];
	KeySet * plugins;
	KeySet * modules;

	ElektraDeferredCallList * deferredCalls;

} Placements;

static char lastIndex[ELEKTRA_MAX_ARRAY_SIZE];

/**
 * Add a new deferred call to the deferred call list.
 *
 * @param  list       deferred call list
 * @param  name       function name
 * @param  parameters function parameters
 * @retval 1 on success
 * @retval 0 when malloc failed
 */
int elektraDeferredCallAdd (ElektraDeferredCallList * list, char * name, KeySet * parameters)
{
	ElektraDeferredCall * item = elektraMalloc (sizeof *item);
	if (item == NULL)
	{
		return 0;
	}
	item->name = elektraStrDup (name);
	item->parameters = ksDup (parameters);
	item->next = NULL;

	if (list->head == NULL)
	{
		// Initialize list
		list->head = list->last = item;
	}
	else
	{
		// Make new item end of list
		list->last->next = item;
		list->last = item;
	}

	return 1;
}

/**
 * Create new deferred call list.
 *
 * The list needs to be deleted with elektraDeferredCallDeleteList().
 *
 * @return  new list
 */
ElektraDeferredCallList * elektraDeferredCallCreateList (void)
{
	ElektraDeferredCallList * list = elektraMalloc (sizeof *list);
	if (list == NULL)
	{
		return NULL;
	}
	list->head = NULL;
	list->last = NULL;
	return list;
}

void elektraDeferredCallDeleteList (ElektraDeferredCallList * list)
{
	ElektraDeferredCall * item = list->head;
	while (item != NULL)
	{
		elektraFree (item->name);
		ksDel (item->parameters);

		ElektraDeferredCall * next = item->next;
		elektraFree (item);

		item = next;
	}

	elektraFree (list);
}

void elektraDeferredCallsExecute (Plugin * plugin, ElektraDeferredCallList * list)
{
	ElektraDeferredCall * item = list->head;
	while (item != NULL)
	{
		size_t func = elektraPluginGetFunction (plugin, item->name);
		if (!func)
		{
			item = item->next;
			continue;
		}
		ElektraDeferredCallable callable = (ElektraDeferredCallable) func;
		callable (plugin, item->parameters);

		item = item->next;
	}
}

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
		if (keyRel (key, cur) != 1)
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
			const char * setStrings[] = { "presetstorage", "presetcleanup", "precommit", "postcommit" };
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
			const char * getStrings[] = { "pregetstorage", "postgetstorage", "postgetcleanup" };
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
			const char * errStrings[] = { "prerollback", "postrollback" };
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

void elektraListDeferFunctionCall (Plugin * plugin, char * name, KeySet * parameters)
{
	Placements * placements = elektraPluginGetData (plugin);
	ELEKTRA_NOT_NULL (placements);
	printf ("elektraListDeferFunctionCall called\n");
	elektraDeferredCallAdd (placements->deferredCalls, name, parameters);
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
		const char * setStrings[] = { "presetstorage", "presetcleanup", "precommit", "postcommit" };
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
		const char * getStrings[] = { "pregetstorage", "postgetstorage", "postgetcleanup" };
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
		const char * errStrings[] = { "prerollback", "postrollback" };
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

static int runPlugins (KeySet * pluginKS, KeySet * modules, KeySet * plugins, KeySet * configOrig, KeySet * returned, Key * parentKey,
		       OP op, Key * (*traversalFunction) (KeySet *), ElektraDeferredCallList * deferredCalls)
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
				Key * sysConfCutPoint = keyNew ("system", 0);
				KeySet * config = ksDup (configOrig);
				KeySet * sysConfigAll = ksCut (config, sysConfCutPoint);
				KeySet * userConfigAll = ksCut (config, userCutPoint);
				KeySet * pluginConfig = ksCut (userConfigAll, current);
				realPluginConfig = elektraRenameKeys (pluginConfig, "user");
				ksDel (pluginConfig);
				Key * toRemove = keyNew ("user/plugins", 0);
				ksDel (ksCut (sysConfigAll, toRemove));
				ksAppend (realPluginConfig, sysConfigAll);
				keyDel (toRemove);
				toRemove = keyNew ("user/placements", 0);
				ksDel (ksCut (realPluginConfig, toRemove));
				ksRewind (realPluginConfig);
				ksDel (sysConfigAll);
				ksDel (userConfigAll);
				ksDel (config);
				keyDel (userCutPoint);
				keyDel (sysConfCutPoint);
				keyDel (toRemove);
				slave = elektraPluginOpen (name, modules, ksDup (realPluginConfig), parentKey);
				ksDel (realPluginConfig);
				if (!slave)
				{
					ksDel (configOrig);
					return -1;
				}
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
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/list", KEY_VALUE, "list plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/list/exports", KEY_END),
			keyNew ("system/elektra/modules/list/exports/open", KEY_FUNC, elektraListOpen, KEY_END),
			keyNew ("system/elektra/modules/list/exports/close", KEY_FUNC, elektraListClose, KEY_END),
			keyNew ("system/elektra/modules/list/exports/get", KEY_FUNC, elektraListGet, KEY_END),
			keyNew ("system/elektra/modules/list/exports/set", KEY_FUNC, elektraListSet, KEY_END),
			keyNew ("system/elektra/modules/list/exports/error", KEY_FUNC, elektraListError, KEY_END),
			keyNew ("system/elektra/modules/list/exports/addPlugin", KEY_FUNC, elektraListAddPlugin, KEY_END),
			keyNew ("system/elektra/modules/list/exports/editPlugin", KEY_FUNC, elektraListEditPlugin, KEY_END),
			keyNew ("system/elektra/modules/list/exports/deferFunctionCall", KEY_FUNC, elektraListDeferFunctionCall, KEY_END),
#include ELEKTRA_README (list)
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
	int ret = runPlugins (pluginKS, placements->modules, placements->plugins, ksDup (config), returned, parentKey, GET, ksNext,
			      placements->deferredCalls);
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
	ret = runPlugins (pluginKS, placements->modules, placements->plugins, ksDup (config), returned, parentKey, SET, ksPop,
			  placements->deferredCalls);
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
	int ret = runPlugins (pluginKS, placements->modules, placements->plugins, ksDup (config), returned, parentKey, ERR, ksPop,
			      placements->deferredCalls);
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

Plugin * ELEKTRA_PLUGIN_EXPORT (list)
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
