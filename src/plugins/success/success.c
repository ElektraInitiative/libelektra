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


#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include "success.h"

int elektraSuccessOpen(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	return 0; /* success */
}

int elektraSuccessClose(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	return 0; /* success */
}

int elektraSuccessGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
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
			keyNew ("system/elektra/modules/success/exports/error",
				KEY_FUNC, elektraSuccessError,
				KEY_END),
			keyNew ("system/elektra/modules/success/exports/checkfile",
				KEY_FUNC, elektraSuccessCheckFile,
				KEY_END),
#include "readme_success.c"
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

int elektraSuccessSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	return 1; /* Always success, no conflict*/
}

int elektraSuccessError(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	return 0;
}

/**
 * @return 1 on success (Relative path)
 * @returns 0 on success (Absolute path)
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

