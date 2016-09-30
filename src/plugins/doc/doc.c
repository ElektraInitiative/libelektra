/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include "doc.h"

//! [plugin include]
#include <kdbplugin.h>
//! [plugin include]
//
//! [plugin errors include]
#include <kdberrors.h>
//! [plugin errors include]

#include <stdio.h>
#include <stdlib.h>

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif


//! [global data]
typedef struct
{
	int global;
} GlobalData;
//! [global data]


#define DOC_PLUGIN_NAME "doc"
#define DOC_PLUGIN_VERSION "1.0.0"


//! [doc open]
int elektraDocOpen (Plugin * handle, Key * warningsKey ELEKTRA_UNUSED)
{
	GlobalData * data;
	KeySet * config = elektraPluginGetConfig (handle);
	Key * kg = ksLookupByName (config, "/global", 0);

	data = elektraMalloc (sizeof (GlobalData));
	data->global = 0;
	if (kg) data->global = atoi (keyString (kg));
	elektraPluginSetData (handle, data);
	//! [doc open]

	//! [doc module]
	if (ksLookupByName (config, "/module", 0))
	{
		return 0;
	}
	// do some setup that will fail without configuration
	//! [doc module]

	return 0; /* success */
}


//! [doc close]
int elektraDocClose (Plugin * handle, Key * warningsKey ELEKTRA_UNUSED)
{
	elektraFree (elektraPluginGetData (handle));

	return 0; /* success */
}
//! [doc close]

static int parseKey (FILE * fp ELEKTRA_UNUSED, char ** key ELEKTRA_UNUSED, char ** value ELEKTRA_UNUSED)
{
	return 0;
}

static void doAction (Key * k ELEKTRA_UNUSED)
{
}

/*
//! [plugin errors spec]
number:60
description:Invalid Line encountered
severity:error
ingroup:plugin
macro:NOEOF
module:simpleini
//! [plugin errors spec]

//! [plugin errors usage]
ELEKTRA_SET_ERROR (ELEKTRA_ERROR_NOEOF, parentKey, "not at the end of file");
//! [plugin errors usage]
*/

//![get contract]
int elektraDocGet (Plugin * plugin ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!strcmp (keyName (parentKey), "system/elektra/modules/doc"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/doc", KEY_VALUE, "doc plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/doc/exports", KEY_END),
			       keyNew ("system/elektra/modules/doc/exports/open", KEY_FUNC, elektraDocOpen, KEY_END),
			       keyNew ("system/elektra/modules/doc/exports/close", KEY_FUNC, elektraDocClose, KEY_END),
			       keyNew ("system/elektra/modules/doc/exports/get", KEY_FUNC, elektraDocGet, KEY_END),
			       keyNew ("system/elektra/modules/doc/exports/set", KEY_FUNC, elektraDocSet, KEY_END),
			       keyNew ("system/elektra/modules/doc/exports/error", KEY_FUNC, elektraDocError, KEY_END),
			       keyNew ("system/elektra/modules/doc/exports/checkconf", KEY_FUNC, elektraDocCheckConf, KEY_END),
#include ELEKTRA_README (doc)
			       keyNew ("system/elektra/modules/doc/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}
	//![get contract]

	//![get storage]
	FILE * fp = fopen (keyString (parentKey), "r");
	char * key;
	char * value;

	while (parseKey (fp, &key, &value) >= 1)
	{
		Key * read = keyNew (0);
		if (keySetName (read, key) == -1)
		{
			ELEKTRA_ADD_WARNING (ELEKTRA_WARNING_INVALID_KEY, parentKey, key);
			keyDel (read);
			continue;
		}
		keySetString (read, value);

		ksAppendKey (returned, read);
		elektraFree (key);
		elektraFree (value);
	}

	if (feof (fp) == 0)
	{
		fclose (fp);
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_NOEOF, parentKey, "not at the end of file");
		return -1;
	}

	fclose (fp);
	//![get storage]


	//![get filter]
	Key * k;
	ksRewind (returned);
	while ((k = ksNext (returned)) != 0)
	{
		doAction (k);
	}

	return 1; // success
}
//![get filter]

int elektraDocSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	ssize_t nr_keys = 0;
	/* set all keys below parentKey and count them with nr_keys */

	return nr_keys;
}

int elektraDocError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return 0;
}

static Plugin * findPlugin (KDB * handle ELEKTRA_UNUSED)
{
	return 0;
}

static void saveToDisc (Key * k ELEKTRA_UNUSED)
{
}

//![validate configuration]
int elektraDocCheckConf (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	/* validate plugin configuration */

	// the return codes have the following meaning:
	// 0: The configuration was OK and has not been changed
	// 1: The configuration has been changed and now it is OK
	// -1: The configuration was not OK and could not be fixed. An error has to be set to errorKey.
	return 0;
}
//![validate configuration]

//![set full]
static void usercode (KDB * handle, KeySet * keyset, Key * key)
{
	// some more user code
	keySetString (key, "mycomment"); // the user changes the key
	ksAppendKey (keyset, key);       // append the key to the keyset
	kdbSet (handle, keyset, 0);      // and syncs it to disc
}

// so now kdbSet is called
int elektraKdbSet (KDB * handle, KeySet * keyset, Key * parentKey)
{
	int ret = 0;
	// find appropriate plugin and then call it:
	Plugin * plugin = findPlugin (handle);
	ret = elektraDocSet (plugin, keyset, parentKey);
	// the keyset with the key (and others for this plugin)
	// will be passed to this function
	return ret;
}

// so now elektraPluginSet(), which is the function described here,
// is called:
int elektraPluginSet (Plugin * plugin ELEKTRA_UNUSED, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	// the task of elektraPluginSet is now to store the keys
	Key * k;
	ksRewind (returned);
	while ((k = ksNext (returned)) != 0)
	{
		saveToDisc (k);
	}

	return 1; /* success */
}
//![set full]


void elektraUsercodeUselessSymbol ()
{
	usercode (0, 0, 0);
}

//![export]
Plugin * ELEKTRA_PLUGIN_EXPORT (doc)
{
	// clang-format off
	return elektraPluginExport(DOC_PLUGIN_NAME,
		ELEKTRA_PLUGIN_OPEN,	&elektraDocOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraDocClose,
		ELEKTRA_PLUGIN_GET,	&elektraDocGet,
		ELEKTRA_PLUGIN_SET,	&elektraDocSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraDocError,
		ELEKTRA_PLUGIN_END);
}
//![export]

/**
 * @}
 */
