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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <kdbproposal.h>
#include <kdbhelper.h>



int elektraTypedispatcherOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
    DispatchConfig *config = elektraPluginGetData(handle);
    if(!config)
    {
	config=initDispatchConfig();
	if(!config)
	    return ERROR;
	elektraPluginSetData(handle, config);
    }
    return SUCCESS; // success
}


int elektraTypedispatcherClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional
	closeDispatchConfig(handle);
	return SUCCESS; // success
}

static int iterate(DispatchConfig *config, KeySet *returned, Key *parentKey)
{
    RC rc = SUCCESS;
    ksRewind(returned);
    Key *cur;
    while((cur = ksNext(returned)) != NULL)
    {
	RC r = getTypeDefinitions(cur, config, parentKey);
	if(r == ERROR)
	{
#ifdef DEVBUILD
	    fprintf(stderr, "getDefinitions on %s failed\n", keyName(cur));
#endif
	    return ERROR;
	}
	r = validateKey(cur, config, parentKey);
	if(r == ERROR)
	{
#ifdef DEVBUILD
	    fprintf(stderr, "Key %s failed to validate\n", keyName(cur));
#endif
	    return ERROR;
	}
    }
    return rc;
}

int elektraTypedispatcherGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/typedispatcher"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/typedispatcher", KEY_VALUE, "typedispatcher plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/typedispatcher/exports", KEY_END),
			       keyNew ("system/elektra/modules/typedispatcher/exports/open", KEY_FUNC, elektraTypedispatcherOpen, KEY_END),
			       keyNew ("system/elektra/modules/typedispatcher/exports/close", KEY_FUNC, elektraTypedispatcherClose, KEY_END),
			       keyNew ("system/elektra/modules/typedispatcher/exports/get", KEY_FUNC, elektraTypedispatcherGet, KEY_END),
			       keyNew ("system/elektra/modules/typedispatcher/exports/set", KEY_FUNC, elektraTypedispatcherSet, KEY_END),
			       keyNew ("system/elektra/modules/typedispatcher/exports/error", KEY_FUNC, elektraTypedispatcherError, KEY_END),
			       keyNew ("system/elektra/modules/typedispatcher/exports/checkconf", KEY_FUNC, elektraTypedispatcherCheckConfig, KEY_END),
#include ELEKTRA_README (typedispatcher)
			       keyNew ("system/elektra/modules/typedispatcher/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return SUCCESS; // success
	}
	// get all keys

	DispatchConfig *config = elektraPluginGetData(handle);
	if(!config)
	{
	    return ERROR;
	}
	RC rc = SUCCESS;
	rc = iterate(config, returned, parentKey);
	return rc;
}

int elektraTypedispatcherSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional

	return 1; // success
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

