/***************************************************************************
            tracer.c  -  Skeleton of backends to access the Key Database
                             -------------------
 *  begin                : Wed 19 May, 2010
 *  copyright            : (C) 2010 by Markus Raab
 *  email                : elektra@markus-raab.org
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
 *   to provide libelektra.so a valid backend.                             *
 *   Simple fill the empty _tracer functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "tracer.h"

int elektraTracerOpen(Plugin *handle, Key *errorKey)
{
	ssize_t nr_keys = 0;
	KeySet *config = elektraPluginGetConfig(handle);
	Key *k;

	printf ("tracer: open(%p): ", (void*)handle);
	while ((k = ksNext(config))!=0) { printf ("%s=%s ", keyName(k), keyString(k)); ++nr_keys; }
	printf ("%zd\n", nr_keys);

	return 0;
}

int elektraTracerClose(Plugin *handle, Key *errorKey)
{
	/* free all backend resources and shut it down */

	printf ("tracer: close(%p)\n", (void*)handle);

	return 0; /* success */
}

int elektraTracerGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	ssize_t nr_keys = 0;
	Key *k=0;

	Key *root = keyNew("system/elektra/modules/tracer", KEY_END);
	if (keyRel(root, parentKey) >= 0)
	{
		KeySet *info =
			ksNew(50,
			keyNew ("system/elektra/modules/tracer",
				KEY_VALUE, "tracer plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/tracer/exports", KEY_END),
			keyNew ("system/elektra/modules/tracer/exports/open",
				KEY_FUNC, elektraTracerOpen,
				KEY_END),
			keyNew ("system/elektra/modules/tracer/exports/close",
				KEY_FUNC, elektraTracerClose,
				KEY_END),
			keyNew ("system/elektra/modules/tracer/exports/get",
				KEY_FUNC, elektraTracerGet,
				KEY_END),
			keyNew ("system/elektra/modules/tracer/exports/set",
				KEY_FUNC, elektraTracerSet,
				KEY_END),
			keyNew ("system/elektra/modules/tracer/exports/error",
				KEY_FUNC, elektraTracerError,
				KEY_END),
			keyNew ("system/elektra/modules/tracer/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew ("system/elektra/modules/tracer/infos/author",
				KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
			keyNew ("system/elektra/modules/tracer/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
			keyNew ("system/elektra/modules/tracer/infos/description",
				KEY_VALUE, "The first plugin", KEY_END),
			keyNew ("system/elektra/modules/tracer/infos/provides",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/tracer/infos/needs",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/tracer/infos/placements",
				KEY_VALUE, "pregetstorage postgetstorage presetstorage precommit postcommit prerollback postrollback", KEY_END),
			keyNew ("system/elektra/modules/tracer/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END);
		ksAppend(returned, info);
		ksDel (info);
	}

	keyDel (root);

	printf ("tracer: get(%p, %s): ", (void*)handle, keyName(parentKey));
	while ((k = ksNext(returned))!=0) { printf ("%s ", keyName(k)); ++nr_keys; }
	printf ("%zd\n", nr_keys);

	return nr_keys;
}

int elektraTracerSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	ssize_t nr_keys = 0;
	Key *k=0;

	printf ("tracer: set(%p, %s): ", (void*)handle, keyName(parentKey));
	while ((k = ksNext(returned))!=0) { printf ("%s ", keyName(k)); ++nr_keys; }
	printf ("%zd\n", nr_keys);

	return nr_keys;
}

int elektraTracerError(Plugin *handle, KeySet *returned, Key *parentKey)
{
	ssize_t nr_keys = 0;
	Key *k=0;

	printf ("tracer: error(%p, %s): ", (void*)handle, keyName(parentKey));
	while ((k = ksNext(returned))!=0) { printf ("%s ", keyName(k)); ++nr_keys; }
	printf ("%zd\n", nr_keys);

	return nr_keys;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(tracer)
{
	return elektraPluginExport("tracer",
		ELEKTRA_PLUGIN_OPEN,	&elektraTracerOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraTracerClose,
		ELEKTRA_PLUGIN_GET,	&elektraTracerGet,
		ELEKTRA_PLUGIN_SET,	&elektraTracerSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraTracerError,
		ELEKTRA_PLUGIN_END);
}

