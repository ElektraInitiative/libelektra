/**
 * @file
 *
 * @brief Source for list plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <string.h>
#include <kdberrors.h>
#include <stdio.h>
#include <stdlib.h>
#include <kdbmodule.h>
#include <kdbinternal.h>
#include "list.h"

typedef enum{preGetStorage = 0, postGetStorage, getEnd}GetPlacements;

typedef enum{preSetStorage = 0, preCommit, postCommit, setEnd}SetPlacements;

typedef enum{preRollback = 0, postRollback, errEnd}ErrPlacements;

typedef enum{GET, SET, ERR}OP;

typedef struct{
	//keep track of placements
	ErrPlacements errCurrent; 
	SetPlacements setCurrent;
	GetPlacements getCurrent;

	ErrPlacements errPlacements[2]; //prerollback and postrollback
	SetPlacements setPlacements[3]; //presetstorage, precommit and postcommit
	GetPlacements getPlacements[2]; //pregetstorage and postgetstorage

	//each keyset contains the list of plugin names for a given placement
	KeySet *setKS[3];
	KeySet *errKS[2];
	KeySet *getKS[2];
	KeySet *plugins;
	KeySet *modules;
}Placements;

int elektraListOpen(Plugin *handle , Key *errorKey ELEKTRA_UNUSED)
{
	Placements *placements = (Placements *)elektraPluginGetData(handle);
	if (!placements)
	{
		placements = (Placements *)elektraMalloc(sizeof(Placements));
		memset(placements, 0, sizeof(Placements));
		placements->errCurrent = preRollback;
		placements->setCurrent = preSetStorage;
		placements->getCurrent = preGetStorage;
		placements->getKS[0] = ksNew(0, KS_END);
		placements->getKS[1] = ksNew(0, KS_END);
		placements->setKS[0] = ksNew(0, KS_END);
		placements->setKS[1] = ksNew(0, KS_END);
		placements->setKS[2] = ksNew(0, KS_END);
		placements->errKS[0] = ksNew(0, KS_END);
		placements->errKS[1] = ksNew(0, KS_END);
		placements->plugins = ksNew(0, KS_END);
		placements->modules = ksNew(0, KS_END);
	}
	elektraPluginSetData(handle, placements);

	elektraModulesInit(placements->modules, NULL);
	KeySet *config = ksDup(elektraPluginGetConfig(handle));
	ksRewind(config);
	Key *key = ksLookupByName(config, "/placements/set", 0);
	if (key)
	{
		const char *setString = keyString(key);
		const char *setStrings[] = {"presetstorage", "precommit", "postcommit"};
		SetPlacements setPlacement = preSetStorage;
		while (setPlacement != setEnd)
		{
			if (strstr(setString, setStrings[setPlacement]))
			{
				placements->setPlacements[setPlacement] = 1;
			}
			++setPlacement;
		}
	}
	key = ksLookupByName(config, "/placements/get", 0);
	if (key)
	{
		const char *getString = keyString(key);
		const char *getStrings[] = {"pregetstorage", "postgetstorage"};
		GetPlacements getPlacement = preGetStorage;
		while (getPlacement != getEnd)
		{	
			if (strstr(getString, getStrings[getPlacement]))
			{
				placements->getPlacements[getPlacement] = 1;
			}
			++getPlacement;
		}
	}
	key = ksLookupByName(config, "/placements/error", 0);
	if (key)
	{
		const char *errString = keyString(key);
		const char *errStrings[] = {"prerollback", "postrollback"};
		ErrPlacements errPlacement = preRollback;
		while (errPlacement != errEnd)
		{	
			if (strstr(errString, errStrings[errPlacement]))
			{
				placements->errPlacements[errPlacement] = 1;
			}
			++errPlacement;
		}
	}
	key = ksLookupByName(config, "/plugins", 0);
	Key *cur; 
	KeySet *cutKS = ksCut(config, key);
	ksRewind(cutKS);
	while ((cur = ksNext(cutKS)) != NULL)
	{
		if (keyRel(key, cur) != 1)
		{
			continue;
		}

		Key *sub;
		Key *lookup = keyDup(cur);
		keyAddBaseName(lookup, "placements");
		keyAddBaseName(lookup, "set");
		sub = ksLookup(cutKS, lookup, 0);
		if (sub)
		{
			const char *setString = keyString(sub);
			const char *setStrings[] = {"presetstorage", "precommit", "postcommit"};
			SetPlacements setPlacement = preSetStorage;
			while (setPlacement != setEnd)
			{
				if (strstr(setString, setStrings[setPlacement]))
				{
					ksAppendKey(placements->setKS[setPlacement], keyDup(cur));
				}
				++setPlacement;
			}
		}
		keySetBaseName(lookup, "get");
		sub = ksLookup(cutKS, lookup, 0);
		if (sub)
		{
			const char *getString = keyString(sub);
			const char *getStrings[] = {"pregetstorage", "postgetstorage"};
			GetPlacements getPlacement = preGetStorage;
			while (getPlacement != getEnd)
			{
				if (strstr(getString, getStrings[getPlacement]))
				{
					ksAppendKey(placements->getKS[getPlacement], keyDup(cur));
				}
				++getPlacement;
			}
		}
		keySetBaseName(lookup, "error");
		sub = ksLookup(cutKS, lookup, 0);
		if (sub)
		{
			const char *errString = keyString(sub);
			const char *errStrings[] = {"prerollback", "postrollback"};
			ErrPlacements errPlacement = preRollback;
			while (errPlacement != errEnd)
			{	
				if (strstr(errString, errStrings[errPlacement]))
				{
					ksAppendKey(placements->errKS[errPlacement], keyDup(cur));
				}
				++errPlacement;
			}
		}
		keyDel(lookup);
	}
	ksDel(cutKS);
	ksDel(config);
	return 1; /* success */
}

int elektraListClose(Plugin *handle , Key *errorKey)
{
	/* free all plugin resources and shut it down */
	Placements *placements = elektraPluginGetData(handle);
	ksDel(placements->getKS[0]);
	ksDel(placements->getKS[1]);
	ksDel(placements->setKS[0]);
	ksDel(placements->setKS[1]);
	ksDel(placements->setKS[2]);
	ksDel(placements->errKS[0]);
	ksDel(placements->errKS[1]);
	Key *cur;
	ksRewind(placements->plugins);
	while ((cur = ksNext(placements->plugins)) != NULL)
	{
		Plugin *slave;
		slave = *(Plugin**)keyValue(cur);
		elektraPluginClose(slave, errorKey);
	}
	ksDel(placements->plugins);
	elektraModulesClose(placements->modules, NULL);
	ksDel(placements->modules);
	elektraFree(placements);
	elektraPluginSetData(handle, 0);
	return 1; /* success */
}

static int runPlugins(KeySet *pluginKS, KeySet *modules, KeySet *plugins, KeySet *configOrig, KeySet *returned, Key *parentKey, OP op, Key *(*traversalFunction)(KeySet *))
{
	Key *current;

	Plugin *slave = NULL;

	//for every plugin in our list: load it, run the expected function (set/get/error) and close it again
	KeySet *realPluginConfig = NULL;
	while ((current = traversalFunction(pluginKS)) != NULL)
	{
		const char *name = keyString(current);
		Key *searchKey = keyNew("/", KEY_END);
		keyAddBaseName(searchKey, name);
		Key *lookup = ksLookup(plugins, searchKey, 0);
		keyDel(searchKey);
		if (lookup)
		{
			slave = *(Plugin**)keyValue(lookup);
		}
		else
		{
			Key *userCutPoint = keyNew("user", 0);
			Key *sysConfCutPoint = keyNew("system", 0);
			KeySet *config = ksDup(configOrig);
			KeySet *sysConfigAll = ksCut(config, sysConfCutPoint);
			KeySet *userConfigAll = ksCut(config, userCutPoint);
			KeySet *pluginConfig = ksCut(userConfigAll, current);
			realPluginConfig = elektraRenameKeys(pluginConfig, "user");
			ksDel(pluginConfig);
			Key *toRemove = keyNew("user/plugins", 0);
			ksDel(ksCut(sysConfigAll, toRemove));
			ksAppend(realPluginConfig, sysConfigAll);
			keyDel(toRemove);
			toRemove = keyNew("user/placements",0);
			ksDel(ksCut(realPluginConfig, toRemove));
			ksRewind(realPluginConfig);
			ksDel(sysConfigAll);
			ksDel(userConfigAll);
			ksDel(config);
			keyDel(userCutPoint);
			keyDel(sysConfCutPoint);
			keyDel(toRemove);
			slave = elektraPluginOpen(name, modules, ksDup(realPluginConfig), parentKey);
			ksDel(realPluginConfig);
			if (!slave)
			{
				goto error;
			}
			Key *slaveKey = keyNew(name, KEY_BINARY, KEY_SIZE, sizeof(Plugin *), KEY_VALUE, &slave, KEY_END);
			keySetName(slaveKey, "/");
			keyAddBaseName(slaveKey, name);
			ksAppendKey(plugins, keyDup(slaveKey));
			keyDel(slaveKey);
		}

		if (op == GET)
		{
			if ((slave->kdbGet(slave, returned, parentKey)) == -1)
			{
				goto error;
			}
		}
		else if (op == SET)
		{
			if ((slave->kdbSet(slave, returned, parentKey)) == -1)
			{
				goto error;
			}
		}
		else if (op == ERR)
		{
			if ((slave->kdbError(slave, returned, parentKey)) == -1)
			{
				goto error;
			}
		}
	}
	ksDel(configOrig);
	return 1;

error:
	ksDel(configOrig);
	if (slave)
	{
		elektraPluginClose(slave, parentKey);
	}
	elektraModulesClose(modules, NULL);
	return -1;
}

int elektraListGet(Plugin *handle , KeySet *returned , Key *parentKey )
{
	if (!strcmp(keyName(parentKey), "system/elektra/modules/list"))
	{
		KeySet *contract = ksNew (30,
				keyNew ("system/elektra/modules/list",
					KEY_VALUE, "list plugin waits for your orders", KEY_END),
				keyNew ("system/elektra/modules/list/exports", KEY_END),
				keyNew ("system/elektra/modules/list/exports/open",
					KEY_FUNC, elektraListOpen, KEY_END),
				keyNew ("system/elektra/modules/list/exports/close",
					KEY_FUNC, elektraListClose, KEY_END),
				keyNew ("system/elektra/modules/list/exports/get",
					KEY_FUNC, elektraListGet, KEY_END),
				keyNew ("system/elektra/modules/list/exports/set",
					KEY_FUNC, elektraListSet, KEY_END),
				keyNew ("system/elektra/modules/list/exports/error",
					KEY_FUNC, elektraListError, KEY_END),
#include ELEKTRA_README(list)
				keyNew ("system/elektra/modules/list/infos/version",
					KEY_VALUE, PLUGINVERSION, KEY_END),
				KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1;
	}

	Placements *placements = elektraPluginGetData(handle);
	KeySet *config = elektraPluginGetConfig(handle);
	GetPlacements currentPlacement = placements->getCurrent;
	KeySet *pluginKS = ksDup((placements)->getKS[currentPlacement]);
	ksRewind(pluginKS);
	int ret = runPlugins(pluginKS, placements->modules, placements->plugins, ksDup(config), returned, parentKey, GET, ksNext);
	placements->getCurrent = ((++currentPlacement)%getEnd);
	while (!placements->getCurrent)
	{
		placements->getCurrent = ((++currentPlacement)%getEnd);
	}
	ksDel(pluginKS);
	return ret;
}

int elektraListSet(Plugin *handle , KeySet *returned , Key *parentKey )
{
	Placements *placements = elektraPluginGetData(handle);
	KeySet *config = elektraPluginGetConfig(handle);
	SetPlacements currentPlacement = placements->setCurrent;
	KeySet *pluginKS = ksDup((placements)->setKS[currentPlacement]);
	ksRewind(pluginKS);
	int ret = runPlugins(pluginKS, placements->modules, placements->plugins, ksDup(config), returned, parentKey, SET, ksPop);
	placements->setCurrent = ((++currentPlacement)%setEnd);
	while (!placements->setCurrent)
	{
		placements->setCurrent = ((++currentPlacement)%setEnd);
	}
	ksDel(pluginKS);

	return ret;
}

int elektraListError(Plugin *handle , KeySet *returned , Key *parentKey )
{
	Placements *placements = elektraPluginGetData(handle);
	KeySet *config = elektraPluginGetConfig(handle);
	ErrPlacements currentPlacement = placements->errCurrent;
	KeySet *pluginKS = ksDup((placements)->errKS[currentPlacement]);
	ksRewind(pluginKS);
	int ret = runPlugins(pluginKS, placements->modules, placements->plugins, ksDup(config), returned, parentKey, ERR, ksPop);
	placements->errCurrent = ((++currentPlacement)%errEnd);
	while (!placements->errCurrent)
	{
		placements->errCurrent = ((++currentPlacement)%errEnd);
	}
	ksDel(pluginKS);
	return ret;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(list)
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

