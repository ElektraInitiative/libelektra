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

