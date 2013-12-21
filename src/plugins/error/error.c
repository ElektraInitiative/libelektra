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

int elektraErrorGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey ELEKTRA_UNUSED)
{
	KeySet *n;
	ksAppend (returned, n = ksNew (30,
		keyNew ("system/elektra/modules/error",
			KEY_VALUE, "error plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/error/exports", KEY_END),
		keyNew ("system/elektra/modules/error/exports/get",
			KEY_FUNC, elektraErrorGet,
			KEY_END),
		keyNew ("system/elektra/modules/error/exports/set",
			KEY_FUNC, elektraErrorSet,
			KEY_END),
		keyNew ("system/elektra/modules/error/infos",
			KEY_VALUE, "All information you want to know", KEY_END),
		keyNew ("system/elektra/modules/error/infos/author",
			KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
		keyNew ("system/elektra/modules/error/infos/licence",
			KEY_VALUE, "BSD", KEY_END),
		keyNew ("system/elektra/modules/error/infos/description",
			KEY_VALUE, "Validates key values using regular expressions", KEY_END),
		keyNew ("system/elektra/modules/error/infos/provides",
			KEY_VALUE, "error", KEY_END),
		keyNew ("system/elektra/modules/error/infos/placements",
			KEY_VALUE, "presetstorage", KEY_END),
		keyNew ("system/elektra/modules/error/infos/needs",
			KEY_VALUE, "", KEY_END),
		keyNew ("system/elektra/modules/error/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END));
	ksDel (n);

	ksAppend (returned, n = elektraErrorSpecification());
	ksDel (n);
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
			elektraTriggerWarnings(atoi(keyString(meta)), parentKey, "from error plugin");
		}

		meta = keyGetMeta (cur, "trigger/error");
		if (meta)
		{
			elektraTriggerError (atoi(keyString(meta)), parentKey, "from error plugin");
			return -1; /* error */
		}
	}

	return 0;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(error)
{
	return elektraPluginExport("error",
		ELEKTRA_PLUGIN_GET,	&elektraErrorGet,
		ELEKTRA_PLUGIN_SET,	&elektraErrorSet,
		ELEKTRA_PLUGIN_END);
}

