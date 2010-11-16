/***************************************************************************
          Success.c  -  Skeleton of a plugin to be copied
                             -------------------
    begin                : Fri May 21 2010
    copyright            : (C) 2010 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/



/***************************************************************************
 *                                                                         *
 *   This is the skeleton of the methods you'll have to implement in order *
 *   to provide libelektra.so a valid plugin.                             *
 *   Simple fill the empty Success functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "success.h"

int elektraSuccessOpen(Plugin *handle, Key *errorKey)
{
	return 0; /* success */
}

int elektraSuccessClose(Plugin *handle, Key *errorKey)
{
	return 0; /* success */
}

int elektraSuccessGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	Key *root = keyNew("system/elektra/modules/success", KEY_END);

	if (keyRel(root, parentKey) >= 0)
	{
		keyDel (root);
		KeySet *info = ksNew (50, keyNew ("system/elektra/modules/success",
				KEY_VALUE, "success plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/success/constants", KEY_END),
			keyNew ("system/elektra/modules/success/constants/NO_FILE",
				KEY_VALUE, "no file used", KEY_END),
			keyNew ("system/elektra/modules/success/exports", KEY_END),
			keyNew ("system/elektra/modules/success/exports/open",
				KEY_FUNC, elektraSuccessOpen,
				KEY_END),
			keyNew ("system/elektra/modules/success/exports/close",
				KEY_FUNC, elektraSuccessClose,
				KEY_END),
			keyNew ("system/elektra/modules/success/exports/get",
				KEY_FUNC, elektraSuccessGet,
				KEY_END),
			keyNew ("system/elektra/modules/success/exports/set",
				KEY_FUNC, elektraSuccessSet,
				KEY_END),
			keyNew ("system/elektra/modules/success/exports/checkfile",
				KEY_FUNC, elektraSuccessCheckFile,
				KEY_END),
			keyNew ("system/elektra/modules/success/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew ("system/elektra/modules/success/infos/author",
				KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
			keyNew ("system/elektra/modules/success/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
			keyNew ("system/elektra/modules/success/infos/description",
				KEY_VALUE, "Dumps complete Elektra Semantics", KEY_END),
			keyNew ("system/elektra/modules/success/infos/provides",
				KEY_VALUE, "resolver", KEY_END),
			keyNew ("system/elektra/modules/success/infos/placements",
				KEY_VALUE, "rollback getresolver setresolver", KEY_END),
			keyNew ("system/elektra/modules/success/infos/needs",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/success/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END);
		ksAppend(returned, info);
		ksDel (info);
		return 1;
	}
	keyDel (root);

	return 1; /* Always success, always update needed */
}

int elektraSuccessSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	return 1; /* Always success, no conflict*/
}

int elektraSuccessError(Plugin *handle, KeySet *returned, Key *parentKey)
{
	return 0;
}

/**
 * @return 1 on success (Relative path)
 * @returns 0 on success (Absolut path)
 * @return never -1 (success guaranteed)
 */
int elektraSuccessCheckFile (const char* filename)
{
	if (filename[0] == '/') return 0;

	return 1;
}


Plugin *ELEKTRA_PLUGIN_EXPORT(success)
{
	return elektraPluginExport("success",
		ELEKTRA_PLUGIN_OPEN,	&elektraSuccessOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraSuccessClose,
		ELEKTRA_PLUGIN_GET,	&elektraSuccessGet,
		ELEKTRA_PLUGIN_SET,	&elektraSuccessSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraSuccessError,
		ELEKTRA_PLUGIN_END);
}

