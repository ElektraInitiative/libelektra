/***************************************************************************
                     noresolver.c  -  Skeleton of a plugin
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


#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include "noresolver.h"

int elektraNoresolverOpen(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	/* plugin initialization logic */

	return 1; /* success */
}

int elektraNoresolverClose(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	/* free all plugin resources and shut it down */

	return 1; /* success */
}

#define ELEKTRA_PLUGIN_NAME noresolver

static KeySet * elektraNoresolverModules()
{
	return ksNew (50,
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "",
			KEY_VALUE, "" ELEKTRA_PLUGIN_NAME " plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants", KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/KDB_DB_SYSTEM",
			KEY_VALUE, KDB_DB_SYSTEM, KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/KDB_DB_HOME",
			KEY_VALUE, KDB_DB_HOME, KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/KDB_DB_USER",
			KEY_VALUE, KDB_DB_USER, KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports", KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/open",
			KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(resolver, open),
			KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/close",
			KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(resolver, close),
			KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/get",
			KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(resolver, get),
			KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/set",
			KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(resolver, set),
			KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/error",
			KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(resolver, error),
			KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/checkfile",
			KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(resolver, checkFile),
			KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos",
			KEY_VALUE, "All information you want to know are in keys below", KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/author",
			KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/licence",
			KEY_VALUE, "BSD", KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/description",
			KEY_VALUE, "The " ELEKTRA_PLUGIN_NAME " just does nothing instead of resolving file names.\n" ,KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/provides",
			KEY_VALUE, "resolver", KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/placements",
			KEY_VALUE, "rollback getresolver setresolver commit", KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/needs",
			KEY_VALUE, "", KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END);
}

int elektraNoresolverGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	/* get all keys */

	return 1; /* success */
}

int elektraNoresolverSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}

int elektraNoresolverError(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(noresolver)
{
	return elektraPluginExport("noresolver",
		ELEKTRA_PLUGIN_OPEN,	&elektraNoresolverOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraNoresolverClose,
		ELEKTRA_PLUGIN_GET,	&elektraNoresolverGet,
		ELEKTRA_PLUGIN_SET,	&elektraNoresolverSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraNoresolverError,
		ELEKTRA_PLUGIN_END);
}

