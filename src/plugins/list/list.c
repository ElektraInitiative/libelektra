/**
 * \file
 *
 * \brief Source for list plugin
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
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

typedef enum{preSetStorage = 0, preCommit,	postCommit, setEnd}SetPlacements;

typedef enum{preRollback = 0, postRollback, errEnd}ErrPlacements;

typedef enum{GET, SET, ERR}OP;

typedef struct{
	uint8_t initialized;
	ErrPlacements errCurrent;
	SetPlacements setCurrent;
	GetPlacements getCurrent;

	ErrPlacements errPlacements[2];
	KeySet *errKS[2];
	SetPlacements setPlacements[3];
	KeySet *setKS[3];
	GetPlacements getPlacements[2];
	KeySet *getKS[2];
}Placements;

int elektraListOpen(Plugin *handle , Key *errorKey ELEKTRA_UNUSED)
{
	Placements *placements = (Placements *)elektraPluginGetData(handle);
	if(!placements)
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
	}
	elektraPluginSetData(handle, placements);
	if(placements->initialized)
		return 1;
	KeySet *config = ksDup(elektraPluginGetConfig(handle));
	ksRewind(config);
	Key *key = ksLookupByName(config, "/placements/set", 0);
	if(key)
	{
		const char *setString = keyString(key);
		const char *setStrings[] = {"presetstorage", "precommit", "postcommit"};
		SetPlacements setPlacement = preSetStorage;
		while(setPlacement != setEnd)
		{	
			if(strstr(setString, setStrings[setPlacement]))
			{
				placements->setPlacements[setPlacement] = 1;
			}
			++setPlacement;
		}
	}	
	key = ksLookupByName(config, "/placements/get", 0);
	if(key)
	{
		const char *getString = keyString(key);
		const char *getStrings[] = {"pregetstorage", "postgetstorage"};
		GetPlacements getPlacement = preGetStorage;
		while(getPlacement != getEnd)
		{	
			if(strstr(getString, getStrings[getPlacement]))
			{
				placements->getPlacements[getPlacement] = 1;
			}
			++getPlacement;
		}
	}
	key = ksLookupByName(config, "/placements/err", 0);
	if(key)
	{
		const char *errString = keyString(key);
		const char *errStrings[] = {"prerollback", "postrollback"};
		ErrPlacements errPlacement = preRollback;
		while(errPlacement != errEnd)
		{	
			if(strstr(errString, errStrings[errPlacement]))
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
	while((cur = ksNext(cutKS)) != NULL)
	{
		if(keyRel(key, cur) != 1)
			continue;
		Key *sub;
		Key *lookup = keyDup(cur);
		keyAddBaseName(lookup, "placements");
		keyAddBaseName(lookup, "set");
		sub = ksLookup(cutKS, lookup, 0);
		if(sub)
		{
			const char *setString = keyString(sub);
			const char *setStrings[] = {"presetstorage", "precommit", "postcommit"};
			SetPlacements setPlacement = preSetStorage;
			while(setPlacement != setEnd)
			{	
				if(strstr(setString, setStrings[setPlacement]))
				{
					ksAppendKey(placements->setKS[setPlacement], keyDup(cur));
				}
				++setPlacement;
			}
		}	
		keySetBaseName(lookup, "get");
		sub = ksLookup(cutKS, lookup, 0);
		if(sub)
		{
			const char *getString = keyString(sub);
			const char *getStrings[] = {"pregetstorage", "postgetstorage"};
			GetPlacements getPlacement = preGetStorage;
			while(getPlacement != getEnd)
			{	
				if(strstr(getString, getStrings[getPlacement]))
				{
					ksAppendKey(placements->getKS[getPlacement], keyDup(cur));
				}
				++getPlacement;
			}
		}	
		keySetBaseName(lookup, "err");
		sub = ksLookup(cutKS, lookup, 0);	
		if(sub)
		{

			const char *errString = keyString(sub);
			const char *errStrings[] = {"preRollback", "postRollback"};
			ErrPlacements errPlacement = preRollback;
			while(errPlacement != errEnd)
			{	
				if(strstr(errString, errStrings[errPlacement]))
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
	placements->initialized = 1;
	return 1; /* success */
}

int elektraListClose(Plugin *handle , Key *errorKey ELEKTRA_UNUSED)
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
	elektraFree(placements);
	return 1; /* success */
}

Key *(*traversalFuntion)(const KeySet *);

static int runPlugins(KeySet *pluginKS, KeySet *configOrig, KeySet *returned, Key *parentKey, OP op)
{
	Key *current;
	Key *errorKey = parentKey;
	KeySet *modules = ksNew(0, KS_END);
	elektraModulesInit(modules, NULL);
	while((current = ksNext(pluginKS)) != NULL)
	{
		const char *name = keyString(current);
		elektraPluginFactory pluginFactory = 0;
		pluginFactory = elektraModulesLoad(modules, name, errorKey);
	        if(pluginFactory == NULL)
		{
			goto Error;
		}		
		Plugin *slave;
		slave = pluginFactory();
		if(slave == NULL)
			goto Error;
		Key *userCutPoint = keyNew("user", 0);
		Key *sysConfCutPoint = keyNew("system", 0);
		KeySet *config = ksDup(configOrig);
		KeySet *sysConfigAll = ksCut(config, sysConfCutPoint);
		KeySet *userConfigAll = ksCut(config, userCutPoint);
		KeySet *pluginConfig = ksCut(userConfigAll, current);
		KeySet *realPluginConfig = elektraRenameKeys(pluginConfig, "user");
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
		slave->refcounter = 1;
		slave->config = ksDup(realPluginConfig);
		if(slave->kdbOpen)
		{
			if((slave->kdbOpen(slave, errorKey)) == -1)
			{
				goto Error;
			}
		}
		if(op == GET)
		{
			if((slave->kdbGet(slave, returned, parentKey)) == -1)
			{
				goto Error;
			}
		}
		else if(op == SET)
		{
			if((slave->kdbSet(slave, returned, parentKey)) == -1)
			{
				goto Error;
			}
		}
		else if(op == ERR)
		{
			if((slave->kdbError(slave, returned, parentKey)) == -1)
			{
				goto Error;
			}
		}

		if(slave->kdbClose)
		{
			if((slave->kdbClose(slave, errorKey)) == -1)
			{
				goto Error;
			}
		}
		ksDel(slave->config);
		free(slave);
		ksDel(realPluginConfig);
	}
	elektraModulesClose(modules, NULL);
	ksDel(modules);
	ksDel(configOrig);
	return 1;
Error:
	elektraModulesClose(modules, NULL);
	ksDel(modules);
	ksDel(configOrig);
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
	runPlugins(pluginKS, ksDup(config), returned, parentKey, GET);
	placements->getCurrent = ((++currentPlacement)%getEnd);
	ksDel(pluginKS);
	return 1; /* success */
}

int elektraListSet(Plugin *handle , KeySet *returned , Key *parentKey )
{
	Placements *placements = elektraPluginGetData(handle);
	KeySet *config = elektraPluginGetConfig(handle);
	SetPlacements currentPlacement = placements->setCurrent;
	KeySet *pluginKS = ksDup((placements)->setKS[currentPlacement]);
	ksRewind(pluginKS);
	runPlugins(pluginKS, ksDup(config), returned, parentKey, SET);
	placements->setCurrent = ((++currentPlacement)%setEnd);
	ksDel(pluginKS);

	return 1; /* success */
}

int elektraListError(Plugin *handle , KeySet *returned , Key *parentKey )
{
	Placements *placements = elektraPluginGetData(handle);
	KeySet *config = elektraPluginGetConfig(handle);
	ErrPlacements currentPlacement = placements->errCurrent;
	KeySet *pluginKS = ksDup((placements)->errKS[currentPlacement]);
	ksRewind(pluginKS);
	runPlugins(pluginKS, ksDup(config), returned, parentKey, ERR);
	placements->errCurrent = ((++currentPlacement)%errEnd);
	ksDel(pluginKS);
	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(list)
{
	return elektraPluginExport("list",
			ELEKTRA_PLUGIN_OPEN,	&elektraListOpen,
			ELEKTRA_PLUGIN_CLOSE,	&elektraListClose,
			ELEKTRA_PLUGIN_GET,	&elektraListGet,
			ELEKTRA_PLUGIN_SET,	&elektraListSet,
			ELEKTRA_PLUGIN_ERROR,	&elektraListError,
			ELEKTRA_PLUGIN_END);
}

