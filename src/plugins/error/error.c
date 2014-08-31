/***************************************************************************
          error.c  -  Skeleton of a plugin to be copied
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
 *   Simple fill the empty _error functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "error.h"

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <stdlib.h>

// #include <stdio.h>

int elektraErrorOpen(Plugin *handle ELEKTRA_UNUSED, Key *parentKey)
{
	KeySet *conf = elektraPluginGetConfig(handle);

	/*
	FILE *f = fopen("error_plugin_debug.log", "a");
	fprintf (f, "HUHU %s\n", keyName(parentKey));
	ksRewind(conf);
	while(ksNext(conf)) fprintf(f, "%s\n", keyName(ksCurrent(conf)));
	fclose(f);
	*/

	if (ksLookupByName(conf, "/module", 0))
	{
		// suppress warnings + errors if it is just a module
		return 0;
	}

	Key *warning = ksLookupByName(conf, "/on_open/warning", 0);
	if (warning)
	{
		elektraTriggerWarnings (atoi(keyString(warning)), parentKey, "from error plugin in kdbOpen");
	}

	Key *error = ksLookupByName(conf, "/on_open/error", 0);
	if (error)
	{
		if (parentKey)
		{
			elektraTriggerError (atoi(keyString(error)), parentKey, "from error plugin in kdbOpen");
		}
		return -1;
	}
	return 0;
}

int elektraErrorGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (keyName(parentKey), "system/elektra/modules/error"))
	{
		KeySet *n;
		ksAppend (returned, n = ksNew (30,
			keyNew ("system/elektra/modules/error",
				KEY_VALUE, "error plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/error/exports", KEY_END),
			keyNew ("system/elektra/modules/error/exports/open",
				KEY_FUNC, elektraErrorOpen,
				KEY_END),
			keyNew ("system/elektra/modules/error/exports/get",
				KEY_FUNC, elektraErrorGet,
				KEY_END),
			keyNew ("system/elektra/modules/error/exports/set",
				KEY_FUNC, elektraErrorSet,
				KEY_END),
#include "readme_error.c"
			keyNew ("system/elektra/modules/error/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END));
		ksDel (n);

		ksAppend (returned, n = elektraErrorSpecification());
		ksDel (n);
	}
	return 1;
}

int elektraErrorSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	Key *cur;
	while ((cur = ksNext(returned)) != 0)
	{
		const Key *meta = 0;

		meta = keyGetMeta (cur, "trigger/warnings");
		if (meta)
		{
			elektraTriggerWarnings(atoi(keyString(meta)), parentKey, "from error plugin in kdbSet");
		}

		meta = keyGetMeta (cur, "trigger/error");
		if (meta)
		{
			elektraTriggerError (atoi(keyString(meta)), parentKey, "from error plugin in kdbSet");
			return -1; /* error */
		}
	}

	return 0;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(error)
{
	return elektraPluginExport("error",
		ELEKTRA_PLUGIN_OPEN,	&elektraErrorOpen,
		ELEKTRA_PLUGIN_GET,	&elektraErrorGet,
		ELEKTRA_PLUGIN_SET,	&elektraErrorSet,
		ELEKTRA_PLUGIN_END);
}

