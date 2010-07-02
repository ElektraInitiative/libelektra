/***************************************************************************
                     hidden.c  -  Skeleton of a plugin
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
 *   to provide a valid plugin.                                            *
 *   Simple fill the empty functions with your code and you are            *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "hidden.h"

#include <string.h>

int elektraHiddenOpen(Plugin *handle, Key *errorKey)
{
	elektraPluginSetData(handle, ksNew(0));

	return 1; /* success */
}

int elektraHiddenClose(Plugin *handle, Key *errorKey)
{
	ksDel (elektraPluginGetData(handle));

	return 1; /* success */
}

int elektraHiddenGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	if (!strcmp (keyName(parentKey), "system/elektra/modules/hidden"))
	{
		ksAppend (returned, ksNew (30,
			keyNew ("system/elektra/modules/hidden",
				KEY_VALUE, "hidden plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/hidden/exports", KEY_END),
			keyNew ("system/elektra/modules/hidden/exports/get",
				KEY_SIZE, sizeof (&elektraHiddenGet),
				KEY_BINARY,
				KEY_VALUE, &elektraHiddenGet, KEY_END),
			keyNew ("system/elektra/modules/hidden/exports/set",
				KEY_SIZE, sizeof (&elektraHiddenSet),
				KEY_BINARY,
				KEY_VALUE, &elektraHiddenSet, KEY_END),
			keyNew ("system/elektra/modules/hidden/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew ("system/elektra/modules/hidden/infos/author",
				KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
			keyNew ("system/elektra/modules/hidden/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
			keyNew ("system/elektra/modules/hidden/infos/description",
				KEY_VALUE, "Hides keys which start with a .", KEY_END),
			keyNew ("system/elektra/modules/hidden/infos/provides",
				KEY_VALUE, "filter", KEY_END),
			keyNew ("system/elektra/modules/hidden/infos/placements",
				KEY_VALUE, "postgetstorage presetstorage", KEY_END),
			keyNew ("system/elektra/modules/hidden/infos/needs",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/hidden/infos/version",
				KEY_VALUE, "1.0", KEY_END),
			KS_END));
		return 1;
	}

	Key *cur = 0;
	KeySet *newReturned = ksNew (ksGetSize (returned), KS_END);
	KeySet *hiddenKeys = elektraPluginGetData (handle);
	ksClear (hiddenKeys);

	while ((cur = ksNext(returned)) != 0)
	{
		if (keyBaseName(cur)[0] != '.') ksAppendKey (newReturned, cur);
		else ksAppendKey (hiddenKeys, cur);
	}

	ksCopy (returned, newReturned);
	ksDel (newReturned);

	return 1; /* success */
}

int elektraHiddenSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	KeySet *hiddenKeys = elektraPluginGetData (handle);
	ksAppend (returned, hiddenKeys);

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(hidden)
{
	return elektraPluginExport("hidden",
		ELEKTRA_PLUGIN_OPEN,	&elektraHiddenOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraHiddenClose,
		ELEKTRA_PLUGIN_GET,	&elektraHiddenGet,
		ELEKTRA_PLUGIN_SET,	&elektraHiddenSet,
		ELEKTRA_PLUGIN_END);
}

