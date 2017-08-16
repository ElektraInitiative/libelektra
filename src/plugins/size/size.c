/**
 * @file
 *
 * @brief Source for size plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "size.h"

#include <kdbhelper.h>
#include <stdio.h>
#include <string.h>


#define DEFAULT_SEPARATOR "x"

static int transform (KeySet * appendKS, const Key * key, const char * sep)
{
	char * value = elektraStrDup (keyString (key));
	if (!value) return 0;
	char * dptr = strstr (value, sep);
	if (!dptr) return 0;
	*dptr = '\0';
	Key * widthKey = keyNew (keyName (key), KEY_END);
	keyAddBaseName (widthKey, "Width");
	keySetString (widthKey, value);

	Key * heightKey = keyNew (keyName (key), KEY_END);
	keyAddBaseName (heightKey, "Height");
	keySetString (heightKey, dptr + elektraStrLen (sep) - 1);

	ksAppendKey (appendKS, widthKey);
	ksAppendKey (appendKS, heightKey);

	elektraFree (value);
	return 1;
}


int elektraSizeClose (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	KeySet * ks = elektraPluginGetData (handle);
	if (ks)
	{
		ksDel (ks);
	}
	elektraPluginSetData (handle, NULL);
	return 0;
}

int elektraSizeSet (Plugin * handle, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	Key * cur = NULL;
	KeySet * iterKS = elektraPluginGetData (handle);
	if (!iterKS) return 0;
	ksRewind (iterKS);
	while ((cur = ksNext (iterKS)) != NULL)
	{
		keyDel (ksLookup (returned, cur, KDB_O_POP));
	}
	ksDel (iterKS);
	elektraPluginSetData (handle, NULL);
	return 0;
}
int elektraSizeGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/size"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/size", KEY_VALUE, "size plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/size/exports", KEY_END),
			       keyNew ("system/elektra/modules/size/exports/close", KEY_FUNC, elektraSizeClose, KEY_END),
			       keyNew ("system/elektra/modules/size/exports/get", KEY_FUNC, elektraSizeGet, KEY_END),
			       keyNew ("system/elektra/modules/size/exports/set", KEY_FUNC, elektraSizeSet, KEY_END),
			       keyNew ("system/elektra/modules/size/exports/checkconf", KEY_FUNC, elektraSizeCheckConfig, KEY_END),
#include ELEKTRA_README (size)
			       keyNew ("system/elektra/modules/size/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys
	KeySet * conf = elektraPluginGetConfig (handle);
	const Key * sepKey = ksLookupByName (conf, "/separator", KDB_O_NONE);
	Key * cur = NULL;
	KeySet * appendKS = elektraPluginGetData (handle);
	if (!appendKS)
	{
		appendKS = ksNew (0, KS_END);
	}
	int rc = 0;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != NULL)
	{
		const Key * meta = keyGetMeta (cur, "transform/size");
		if (!meta) continue;
		const char * metaSep = keyString (meta);
		const char * sep = DEFAULT_SEPARATOR;
		if (metaSep && (metaSep[0] != '\0'))
		{
			sep = metaSep;
		}
		else if (sepKey)
		{
			sep = keyString (sepKey);
		}
		rc += transform (appendKS, cur, sep);
	}
	ksAppend (returned, appendKS);
	elektraPluginSetData (handle, appendKS);
	if (rc)
	{
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	else
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}
}

int elektraSizeCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (size)
{
	// clang-format off
	return elektraPluginExport ("size",
        ELEKTRA_PLUGIN_CLOSE, &elektraSizeClose,
		ELEKTRA_PLUGIN_GET,	&elektraSizeGet,
		ELEKTRA_PLUGIN_SET,	&elektraSizeSet,
		ELEKTRA_PLUGIN_END);
}
