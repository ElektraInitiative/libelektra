/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "doc.h"

//! [plugin include]
#include <kdbplugin.h>
//! [plugin include]
//
//! [plugin errors include]
// using namespace ckdb; // for C++
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
int elektraDocOpen (Plugin * handle, ElektraKey * warningsKey ELEKTRA_UNUSED)
{
	GlobalData * data;
	ElektraKeyset * config = elektraPluginGetConfig (handle);
	ElektraKey * kg = ksLookupByName (config, "/global", 0);

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
int elektraDocClose (Plugin * handle, ElektraKey * warningsKey ELEKTRA_UNUSED)
{
	elektraFree (elektraPluginGetData (handle));

	return 0; /* success */
}
//! [doc close]

static int parseKey (FILE * fp ELEKTRA_UNUSED, char ** key ELEKTRA_UNUSED, char ** value ELEKTRA_UNUSED)
{
	return 0;
}

static void doAction (ElektraKey * k ELEKTRA_UNUSED)
{
}

/*
//! [plugin errors spec]
number:60
description:Invalid Line encountered
severity:error
macro:NOEOF
module:simpleini
//! [plugin errors spec]

//! [plugin errors usage]
ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR ( parentKey, "Not at the end of file");
//! [plugin errors usage]
*/

//![get contract]
int elektraDocGet (Plugin * plugin ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/doc"))
	{
		ElektraKeyset * contract =
			ksNew (30, keyNew ("system:/elektra/modules/doc", ELEKTRA_KEY_VALUE, "doc plugin waits for your orders", ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/doc/exports", ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/doc/exports/open", ELEKTRA_KEY_FUNC, elektraDocOpen, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/doc/exports/close", ELEKTRA_KEY_FUNC, elektraDocClose, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/doc/exports/get", ELEKTRA_KEY_FUNC, elektraDocGet, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/doc/exports/set", ELEKTRA_KEY_FUNC, elektraDocSet, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/doc/exports/commit", ELEKTRA_KEY_FUNC, elektraDocCommit, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/doc/exports/error", ELEKTRA_KEY_FUNC, elektraDocError, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/doc/exports/checkconf", ELEKTRA_KEY_FUNC, elektraDocCheckConf, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/doc/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}
	//![get contract]

	//![get storage]
	FILE * fp = fopen (keyString (parentKey), "r");
	char * key = 0;
	char * value = 0;

	while (parseKey (fp, &key, &value) >= 1)
	{
		ElektraKey * read = keyNew (keyName (parentKey), ELEKTRA_KEY_END);
		if (keyAddName (read, key) == -1)
		{
			ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF (parentKey, "Key name %s is not valid, discarding key", key);
			keyDel (read);
			continue;
		}
		keySetString (read, value);

		ksAppendKey (returned, read);
	}

	if (feof (fp) == 0)
	{
		fclose (fp);
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (parentKey, "Invalid line encountered: not at the end of file");
		return -1;
	}

	fclose (fp);
	//![get storage]

	//![get global keyset]
	ElektraKeyset * globalKS = elektraPluginGetGlobalKeySet (plugin);
	// now we can read something from the global keyset
	// or add something for us or others to read
	ElektraKey * important = keyNew ("user:/global/myDocKey", ELEKTRA_KEY_VALUE, "global plugins can see me", ELEKTRA_KEY_END);
	ksAppendKey (globalKS, important);
	//![get global keyset]

	//![get global keyset cleanup]
	// clean up parts of the global keyset which we do not need
	ElektraKey * cutKey = keyNew ("user:/global/myDocKey", ELEKTRA_KEY_END);
	ElektraKeyset * notNeeded = ksCut (globalKS, cutKey);
	ksDel (notNeeded);
	//![get global keyset cleanup]

	//![get filter]
	ElektraKey * k;
	ksRewind (returned);
	while ((k = ksNext (returned)) != 0)
	{
		doAction (k);
	}

	return 1; // success
}
//![get filter]

int elektraDocSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey)
{
	ssize_t nr_keys = 0;
	/* set all keys below parentKey and count them with nr_keys */


	//![opening files]
	FILE * fp = fopen (keyString (parentKey), "w");
	if (!fp)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		return -1;
	}
	//![opening files]
	fclose (fp);

	return nr_keys;
}

int elektraDocError (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	return 0;
}

int elektraDocCommit (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	return 0;
}

static Plugin * findPlugin (ElektraKdb * handle ELEKTRA_UNUSED)
{
	return 0;
}

static void saveToDisc (ElektraKey * k ELEKTRA_UNUSED)
{
}

//![validate configuration]
int elektraDocCheckConf (ElektraKey * errorKey ELEKTRA_UNUSED, ElektraKeyset * conf ELEKTRA_UNUSED)
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
static void usercode (ElektraKdb * handle, ElektraKeyset * keyset, ElektraKey * key)
{
	// some more user code
	keySetString (key, "mycomment"); // the user changes the key
	ksAppendKey (keyset, key);	 // append the key to the keyset
	kdbSet (handle, keyset, 0);	 // and syncs it to disc
}

// so now kdbSet is called
int elektraKdbSet (ElektraKdb * handle, ElektraKeyset * keyset, ElektraKey * parentKey)
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
int elektraPluginSet (Plugin * plugin ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	// the task of elektraPluginSet is now to store the keys
	ElektraKey * k;
	ksRewind (returned);
	while ((k = ksNext (returned)) != 0)
	{
		saveToDisc (k);
	}

	return 1; /* success */
}
//![set full]


void elektraUsercodeUselessSymbol (void)
{
	usercode (0, 0, 0);
}

//![export]
Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport(DOC_PLUGIN_NAME,
		ELEKTRA_PLUGIN_OPEN,	&elektraDocOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraDocClose,
		ELEKTRA_PLUGIN_GET,	&elektraDocGet,
		ELEKTRA_PLUGIN_SET,	&elektraDocSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraDocError,
		ELEKTRA_PLUGIN_COMMIT,	&elektraDocCommit,
		ELEKTRA_PLUGIN_END);
}
//![export]

/**
 * @}
 */
