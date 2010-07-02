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

#include <stdlib.h>

static inline KeySet *elektraErrorSpecification (void)
{
	return ksNew (30,
		keyNew ("system/elektra/modules/error/specification",
			KEY_VALUE, "TODO: add the specification of all error codes below", KEY_END),
		keyNew ("system/elektra/modules/error/specification/1",
			KEY_VALUE, "", KEY_END),
		keyNew ("system/elektra/modules/error/specification/1/module",
			KEY_VALUE, "dl", KEY_END),
		keyNew ("system/elektra/modules/error/specification/1/ingroup",
			KEY_VALUE, "modules", KEY_END),
		keyNew ("system/elektra/modules/error/specification/1/description",
			KEY_VALUE, "could not load module, dlopen failed", KEY_END),
		KS_END);

}

int elektraErrorGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	ksAppend (returned, ksNew (30,
		keyNew ("system/elektra/modules/error",
			KEY_VALUE, "error plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/error/exports", KEY_END),
		keyNew ("system/elektra/modules/error/exports/get",
			KEY_SIZE, sizeof (&elektraErrorGet),
			KEY_BINARY,
			KEY_VALUE, &elektraErrorGet, KEY_END),
		keyNew ("system/elektra/modules/error/exports/set",
			KEY_SIZE, sizeof (&elektraErrorSet),
			KEY_BINARY,
			KEY_VALUE, &elektraErrorSet, KEY_END),
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
			KEY_VALUE, "1.0", KEY_END),
		KS_END));
	ksAppend (returned, elektraErrorSpecification());
	return 1;
}

static inline void elektraTriggerWarnings (int nr, Key *parentKey, const char *message)
{
	switch (nr)
	{
		case 7: ELEKTRA_ADD_WARNING (7, parentKey, message);
			break;
		default: ELEKTRA_ADD_WARNING (45, parentKey, "in default branch");
			 break;
	}
}

static inline void elektraTriggerError (int nr, Key *parentKey, const char *message)
{
	switch (nr)
	{
		case 1: ELEKTRA_SET_ERROR (1, parentKey, "from error plugin");
			break;
		default: ELEKTRA_SET_ERROR (44, parentKey, "in default branch");
			 break;
	}
}

int elektraErrorSet(Plugin *handle, KeySet *returned, Key *parentKey)
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

