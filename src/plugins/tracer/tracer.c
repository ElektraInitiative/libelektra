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

int kdbOpen_tracer(Plugin *handle)
{
	ssize_t nr_keys = 0;
	KeySet *config = elektraPluginGetConfig(handle);
	Key *k;

	printf ("tracer: kdbOpen(%p): ", (void*)handle);
	while ((k = ksNext(config))!=0) { printf ("%s=%s ", keyName(k), keyString(k)); ++nr_keys; }
	printf ("%zd\n", nr_keys);

	return 0;
}

int kdbClose_tracer(Plugin *handle)
{
	/* free all backend resources and shut it down */

	printf ("tracer: kdbClose(%p)\n", (void*)handle);

	return 0; /* success */
}

ssize_t kdbGet_tracer(Plugin *handle, KeySet *returned, const Key *parentKey)
{
	ssize_t nr_keys = 0;
	Key *k=0;

	Key *root = keyNew("system/elektra/modules/tracer", KEY_END);
	if (keyRel(root, parentKey) >= 0)
	{
		Key *k;
		KeySet *info =
			ksNew(50,
			keyNew ("system/elektra/modules/tracer",
				KEY_VALUE, "tracer plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/tracer/exports", KEY_END),
			keyNew ("system/elektra/modules/tracer/exports/open",
				KEY_SIZE, sizeof (&kdbOpen_tracer),
				KEY_BINARY,
				KEY_VALUE, &kdbOpen_tracer, KEY_END),
			keyNew ("system/elektra/modules/tracer/exports/close",
				KEY_SIZE, sizeof (&kdbClose_tracer),
				KEY_BINARY,
				KEY_VALUE, &kdbClose_tracer, KEY_END),
			keyNew ("system/elektra/modules/tracer/exports/get",
				KEY_SIZE, sizeof (&kdbGet_tracer),
				KEY_BINARY,
				KEY_VALUE, &kdbGet_tracer, KEY_END),
			keyNew ("system/elektra/modules/tracer/exports/set",
				KEY_SIZE, sizeof (&kdbSet_tracer),
				KEY_BINARY,
				KEY_VALUE, &kdbSet_tracer, KEY_END),
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
			keyNew ("system/elektra/modules/tracer/infos/version",
				KEY_VALUE, BACKENDVERSION, KEY_END),
			KS_END);
		ksAppend(returned, info);
		ksRewind(returned);

		while ((k = ksNext(returned)) != 0) keyClearSync(k);
		nr_keys = ksGetSize(returned);
	}

	keyDel (root);

	printf ("tracer: kdbGet(%p, %s): ", (void*)handle, keyName(parentKey));
	while ((k = ksNext(returned))!=0) { printf ("%s ", keyName(k)); ++nr_keys; }
	printf ("%zd\n", nr_keys);

	return nr_keys;
}

ssize_t kdbSet_tracer(Plugin *handle, KeySet *returned, const Key *parentKey)
{
	ssize_t nr_keys = 0;
	Key *k=0;

	printf ("tracer: kdbSet(%p, %s): ", (void*)handle, keyName(parentKey));
	while ((k = ksNext(returned))!=0) { printf ("%s ", keyName(k)); ++nr_keys; }
	printf ("%zd\n", nr_keys);

	return nr_keys;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(tracer)
{
	return elektraPluginExport(BACKENDNAME,
		ELEKTRA_PLUGIN_OPEN,	&kdbOpen_tracer,
		ELEKTRA_PLUGIN_CLOSE,	&kdbClose_tracer,
		ELEKTRA_PLUGIN_GET,	&kdbGet_tracer,
		ELEKTRA_PLUGIN_SET,	&kdbSet_tracer,
		ELEKTRA_PLUGIN_END);
}

