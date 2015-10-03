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

#include "globalglob.h"

int elektraGlobalglobOpen(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	/* plugin initialization logic */

	return 1; /* success */
}

int elektraGlobalglobClose(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	/* free all plugin resources and shut it down */

	return 1; /* success */
}
static Key *cutNSName(Key *toCut)
{
	Key *ret = keyDup(toCut);
	const char *name = keyName(toCut);
	char *ptr = strchr(name, '/');
	keySetName(ret, ptr);
	return ret;
}
int elektraGlobalglobGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	if (!strcmp(keyName(parentKey), "system/elektra/modules/globalglob"))
	{
		KeySet *contract = ksNew (30,
		keyNew ("system/elektra/modules/globalglob",
			KEY_VALUE, "globalglob plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/globalglob/exports", KEY_END),
		keyNew ("system/elektra/modules/globalglob/exports/open",
			KEY_FUNC, elektraGlobalglobOpen, KEY_END),
		keyNew ("system/elektra/modules/globalglob/exports/close",
			KEY_FUNC, elektraGlobalglobClose, KEY_END),
		keyNew ("system/elektra/modules/globalglob/exports/get",
			KEY_FUNC, elektraGlobalglobGet, KEY_END),
		keyNew ("system/elektra/modules/globalglob/exports/set",
			KEY_FUNC, elektraGlobalglobSet, KEY_END),
		keyNew ("system/elektra/modules/globalglob/exports/error",
			KEY_FUNC, elektraGlobalglobError, KEY_END),
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
		Key *lookup = ksLookup(returned, curKey, 0);
		if(lookup)
		{
			keyCopyAllMeta(lookup, curKey);	
		}
		else
		{
			ELEKTRA_SET_ERRORF(125, parentKey, "key: %s\n", keyName(curKey));
		}
		keyDel(curKey);
	}
	ksAppend(returned, spec);
	ksDel(spec);
	return 1; /* success */
}

int elektraGlobalglobSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	/* set all keys */
	Key *specCutKey = keyNew("spec", KEY_END);
	KeySet *spec = ksCut(returned, specCutKey);
	keyDel(specCutKey);
	Key *specKey;
	ksRewind(spec);
	while((specKey = ksNext(spec)) != NULL)
	{
		Key *curKey = cutNSName(specKey);
		Key *lookup = ksLookup(returned, curKey, 0);
		if(lookup)
		{
			keyCopyAllMeta(lookup, curKey);	
		}
		else
		{
			ELEKTRA_SET_ERRORF(125, parentKey, "key: %s\n", keyName(curKey));
			ksAppend(returned, spec);
			keyDel(spec);
			keyDel(curKey);
			return -1;
		}
		keyDel(curKey);
	}
	ksAppend(returned, spec);
	ksDel(spec);

	return 1; /* success */
}

int elektraGlobalglobError(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(globalglob)
{
	return elektraPluginExport("globalglob",
		ELEKTRA_PLUGIN_OPEN,	&elektraGlobalglobOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraGlobalglobClose,
		ELEKTRA_PLUGIN_GET,	&elektraGlobalglobGet,
		ELEKTRA_PLUGIN_SET,	&elektraGlobalglobSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraGlobalglobError,
		ELEKTRA_PLUGIN_END);
}

