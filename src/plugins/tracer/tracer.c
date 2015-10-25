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


#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include "tracer.h"

int elektraTracerOpen(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	/*
	ssize_t nr_keys = 0;
	KeySet *config = elektraPluginGetConfig(handle);
	Key *k;

	printf ("tracer: open(%p, %s = %s): ", (void*)handle, keyName(errorKey), keyString(errorKey));
	while ((k = ksNext(config))!=0) { printf ("%s=%s ", keyName(k), keyString(k)); ++nr_keys; }
	printf ("%zd\n", nr_keys);
	*/

	return 0;
}

int elektraTracerClose(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	/*
	printf ("tracer: close(%p, %s = %s)\n", (void*)handle, keyName(errorKey), keyString(errorKey));
	*/

	return 0;
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
#include "readme_tracer.c"
			keyNew ("system/elektra/modules/tracer/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END);
		ksAppend(returned, info);
		ksDel (info);
	}

	keyDel (root);

	printf ("tracer: get(%p, %s, %s): ", (void*)handle, keyName(parentKey), keyString(parentKey));
	while ((k = ksNext(returned))!=0) { printf ("%s ", keyName(k)); ++nr_keys; }
	printf ("%zd\n", nr_keys);

	return nr_keys;
}

int elektraTracerSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	ssize_t nr_keys = 0;
	Key *k=0;

	printf ("tracer: set(%p, %s, %s): ", (void*)handle, keyName(parentKey), keyString(parentKey));
	while ((k = ksNext(returned))!=0) { printf ("%s ", keyName(k)); ++nr_keys; }
	printf ("%zd\n", nr_keys);

	return nr_keys;
}

int elektraTracerError(Plugin *handle, KeySet *returned, Key *parentKey)
{
	ssize_t nr_keys = 0;
	Key *k=0;

	printf ("tracer: error(%p, %s, %s): ", (void*)handle, keyName(parentKey), keyString(parentKey));
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

