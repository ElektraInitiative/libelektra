/**
 * \file
 *
 * \brief Source for globalglob plugin
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <kdberrors.h>
#include <fnmatch.h>
#include "globalglob.h"

static Key *cutNSName(Key *toCut)
{
	Key *ret = keyDup(toCut);
	const char *name = keyName(toCut);
	char *ptr = strchr(name, '/');
	keySetName(ret, ptr);
	return ret;
}

static void log(Key *key)
{

	const Key *meta=keyGetMeta(key, "log/validation/failed");
	if(meta)
	{
		const char *lastIndex = keyString(meta);
		unsigned short index = atoi(lastIndex+1)+1;
		char *newName = elektraMalloc(elektraStrLen(keyName(meta))+elektraStrLen(lastIndex)+5); // 6 digits should be more than enough ?
		if(newName)
		{
			sprintf(newName, "%s/#%u", keyName(meta), index);
			keySetMeta(key, newName, "globalglob struct check failed");
			sprintf(newName, "#%u", index);
			keySetMeta(key, "log/validation/failed", newName);
			elektraFree(newName);
		}
	}
	else
	{
		keySetMeta(key, "log/validation/failed", "#0");
		keySetMeta(key, "log/validation/failed/#0", "globalglob struct check failed");
	}

}
int elektraGlobalglobGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	if (!strcmp(keyName(parentKey), "system/elektra/modules/globalglob"))
	{
		KeySet *contract = ksNew (30,
				keyNew ("system/elektra/modules/globalglob",
					KEY_VALUE, "globalglob plugin waits for your orders", KEY_END),
				keyNew ("system/elektra/modules/globalglob/exports", KEY_END),
				keyNew ("system/elektra/modules/globalglob/exports/get",
					KEY_FUNC, elektraGlobalglobGet, KEY_END),
				keyNew ("system/elektra/modules/globalglob/exports/set",
					KEY_FUNC, elektraGlobalglobSet, KEY_END),
#include ELEKTRA_README(globalglob)
				keyNew ("system/elektra/modules/globalglob/infos/version",
					KEY_VALUE, PLUGINVERSION, KEY_END),
				KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}
	/* get all keys */
	Key *specCutKey = keyNew("spec", KEY_END);
	KeySet *spec = ksCut(returned, specCutKey);
	keyDel(specCutKey);
	Key *specKey;
	ksRewind(spec);
	while((specKey = ksNext(spec)) != NULL)
	{
		Key *curKey = cutNSName(specKey);
		ksRewind(returned);
		Key *cur;
		int found = 0;
		while((cur = ksNext(returned)) != NULL)
		{
			Key *tmp = cutNSName(cur);
			if(fnmatch(keyName(curKey), keyName(tmp), 0) == 0)
			{
				found = 1;
				keyCopyAllMeta(cur, specKey);
			}
			keyDel(tmp);
		}
		if(!found)
		{
			ELEKTRA_SET_ERRORF(136, parentKey, "key: %s\n", keyName(curKey));
			log(specKey);
		}
		keyDel(curKey);		
	}
	ksAppend(returned, spec);
	ksDel(spec);
	return 1; /* success */
}

int elektraGlobalglobSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	/* set all keys */
	Key *specCutKey = keyNew("spec", KEY_END);
	KeySet *spec = ksCut(returned, specCutKey);
	keyDel(specCutKey);
	Key *specKey;
	ksRewind(spec);
	int retval = 1;
	while((specKey = ksNext(spec)) != NULL)
	{
		Key *curKey = cutNSName(specKey);
		ksRewind(returned);
		Key *cur;
		int found = 0;
		while((cur = ksNext(returned)) != NULL)
		{
			Key *tmp = cutNSName(cur);
			if(fnmatch(keyName(curKey), keyName(tmp), 0) == 0)
			{
				found = 1;
				keyCopyAllMeta(cur, specKey);
			}
			keyDel(tmp);
		}
		if(!found)
		{
			ELEKTRA_SET_ERRORF(136, parentKey, "key: %s\n", keyName(curKey));
			log(specKey);
			retval = -1;
		}
		keyDel(curKey);		
	}
	ksAppend(returned, spec);
	ksDel(spec);

	return retval; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(globalglob)
{
	return elektraPluginExport("globalglob",
			ELEKTRA_PLUGIN_GET,	&elektraGlobalglobGet,
			ELEKTRA_PLUGIN_SET,	&elektraGlobalglobSet,
			ELEKTRA_PLUGIN_END);
}

