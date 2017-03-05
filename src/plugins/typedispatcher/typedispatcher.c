/**
 * @file
 *
 * @brief Source for typedispatcher plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include "typedispatcher.h"
#include "typehelper.h"

#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbproposal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int elektraTypedispatcherOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	KeySet * config = elektraPluginGetConfig (handle);
	if (elektraTypedispatcherCheckConfig (errorKey, config) == -1)
		return -1;
	else
		return SUCCESS; // success
}


int elektraTypedispatcherClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional
	closeDispatchConfig (handle);
	return SUCCESS; // success
}

static int iterate (DispatchConfig * config, KeySet * returned, Key * parentKey)
{
	RC rc = SUCCESS;
	ksRewind (returned);
	Key * cur;
	Key * specCut = keyNew ("spec", KEY_END);
	KeySet * specKS = ksCut (returned, specCut);
	keyDel (specCut);
	ksRewind (specKS);
	while ((cur = ksNext (specKS)) != NULL)
	{
		RC r = getTypeDefinitions (cur, config, parentKey);
		if (r == ERROR)
		{
#ifdef DEVBUILD
			fprintf (stderr, "getDefinitions on %s failed\n", keyName (cur));
#endif
			ksAppend (returned, specKS);
			ksDel (specKS);
			return ERROR;
		}
	}
	KeySet * iterateKS = ksDup (returned);
	ksRewind (iterateKS);
	while ((cur = ksNext (iterateKS)) != NULL)
	{
		RC r = getTypeDefinitions (cur, config, parentKey);
		if (r == ERROR)
		{
#ifdef DEVBUILD
			fprintf (stderr, "getDefinitions on %s failed\n", keyName (cur));
#endif
			ksAppend (returned, specKS);
			ksDel (specKS);
			ksDel (iterateKS);
			return ERROR;
		}
		r = validateKey (cur, returned, config, parentKey);
		if (r == ERROR)
		{
#ifdef DEVBUILD
			fprintf (stderr, "Key %s failed to validate\n", keyName (cur));
#endif
			ksAppend (returned, specKS);
			ksDel (specKS);
			ksDel (iterateKS);
			return ERROR;
		}
	}
	ksDel (iterateKS);
	ksAppend (returned, specKS);
	ksDel (specKS);
	return rc;
}

static void setOnErrorStrategy (DispatchConfig * config, Plugin * handle)
{
	KeySet * configKS = elektraPluginGetConfig (handle);
	Key * onError = ksLookupByName (configKS, "/error", KDB_O_NONE);
	if (onError)
	{
		const char * strVal = keyString (onError);
		if (!strcasecmp (strVal, "FAIL"))
			config->onError = FAIL;
		else if (!strcasecmp (strVal, "IGNORE"))
			config->onError = IGNORE;
		else if (!strcasecmp (strVal, "DROPKEY"))
			config->onError = DROPKEY;
		else
			config->onError = DROPKEY;
	}
	else
	{
		config->onError = DROPKEY;
	}
}

int elektraTypedispatcherGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/typedispatcher"))
	{
		KeySet * contract = ksNew (
			30,
			keyNew ("system/elektra/modules/typedispatcher", KEY_VALUE, "typedispatcher plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/typedispatcher/exports", KEY_END),
			keyNew ("system/elektra/modules/typedispatcher/exports/open", KEY_FUNC, elektraTypedispatcherOpen, KEY_END),
			keyNew ("system/elektra/modules/typedispatcher/exports/close", KEY_FUNC, elektraTypedispatcherClose, KEY_END),
			keyNew ("system/elektra/modules/typedispatcher/exports/get", KEY_FUNC, elektraTypedispatcherGet, KEY_END),
			keyNew ("system/elektra/modules/typedispatcher/exports/set", KEY_FUNC, elektraTypedispatcherSet, KEY_END),
			keyNew ("system/elektra/modules/typedispatcher/exports/error", KEY_FUNC, elektraTypedispatcherError, KEY_END),
			keyNew ("system/elektra/modules/typedispatcher/exports/checkconf", KEY_FUNC, elektraTypedispatcherCheckConfig,
				KEY_END),
#include ELEKTRA_README (typedispatcher)
			keyNew ("system/elektra/modules/typedispatcher/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return SUCCESS; // success
	}

	DispatchConfig * config = NULL;
	config = initDispatchConfig ();
	if (!config) return ERROR;
	setOnErrorStrategy (config, handle);
	KeySet * conf = elektraPluginGetConfig (handle);
	config->pluginMeta = conf;
	elektraPluginSetData (handle, config);
	RC rc = SUCCESS;
	rc = iterate (config, returned, parentKey);
	closeDispatchConfig (handle);
	elektraPluginSetData (handle, NULL);
	return rc;
}

int elektraTypedispatcherSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional
	DispatchConfig * config = NULL;
	config = initDispatchConfig ();
	if (!config) return ERROR;
	config->onError = FAIL;
	KeySet * conf = elektraPluginGetConfig (handle);
	config->pluginMeta = conf;
	elektraPluginSetData (handle, config);
	RC rc = SUCCESS;
	rc = iterate (config, returned, parentKey);
	closeDispatchConfig (handle);
	elektraPluginSetData (handle, NULL);
	return rc;
}

int elektraTypedispatcherError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// handle errors (commit failed)
	// this function is optional

	return 1; // success
}

int elektraTypedispatcherCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	// the return codes have the following meaning:
	// 0: The configuration was OK and has not been changed
	// 1: The configuration has been changed and now it is OK
	// -1: The configuration was not OK and could not be fixed. An error has to be set to errorKey.
	Key * metaini = keyNew ("system/info/elektra/metadata/#0", KEY_END);
	KDB * handle = kdbOpen (metaini);
	if (!handle)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_TYPEDISP_METAINI_FAILED, errorKey,
				   "failed to access METADATA.ini in system/info/elektra/metadata/#0. Please mount it manually or run "
				   "\"kdb mount-info\"");
		keyDel (metaini);
		return -1;
	}
	KeySet * KS = ksNew (0, KS_END);
	int rc = kdbGet (handle, KS, metaini);
	if (rc != 1)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_TYPEDISP_METAINI_FAILED, errorKey,
				   "failed to access METADATA.ini in system/info/elektra/metadata/#0. Please mount it manually or run "
				   "\"kdb mount-info\"");
		keyDel (metaini);
		ksDel (KS);
		kdbClose (handle, NULL);
		return -1;
	}
	Key * onErrorKey = ksLookupByName (conf, "/error", KDB_O_POP);
	ksClear (conf);
	parseMetadata (metaini, KS, conf);
	ksAppendKey (conf, onErrorKey);
	keyDel (metaini);
	ksDel (KS);
	kdbClose (handle, NULL);
	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (typedispatcher)
{
	// clang-format off
	return elektraPluginExport ("typedispatcher",
		ELEKTRA_PLUGIN_OPEN,	&elektraTypedispatcherOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraTypedispatcherClose,
		ELEKTRA_PLUGIN_GET,	&elektraTypedispatcherGet,
		ELEKTRA_PLUGIN_SET,	&elektraTypedispatcherSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraTypedispatcherError,
		ELEKTRA_PLUGIN_END);
}

