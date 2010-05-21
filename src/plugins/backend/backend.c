/***************************************************************************
            backend.c  -  Plugin which composes other plugins together
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
 *   Simple fill the empty _backend functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "backend.h"

int kdbOpen_backend(Plugin *handle)
{
	/* plugin initialization logic */

	return 0; /* success */
}

int kdbClose_backend(Plugin *handle)
{
	/* free all plugin resources and shut it down */

	return 0; /* success */
}

ssize_t kdbGet_backend(Plugin *handle, KeySet *returned, const Key *parentKey)
{
	ssize_t nr_keys = 0;
	/* get all keys below parentKey and count them with nr_keys */

	return nr_keys; /* success */
}

ssize_t kdbSet_backend(Plugin *handle, KeySet *returned, const Key *parentKey)
{
	ssize_t nr_keys = 0;
	/* set all keys below parentKey and count them with nr_keys */

	return nr_keys;
}

Plugin *KDBEXPORT(backend)
{
	return pluginExport(BACKENDNAME,
		KDB_PLUGIN_OPEN,	&kdbOpen_backend,
		KDB_PLUGIN_CLOSE,	&kdbClose_backend,
		KDB_PLUGIN_GET,		&kdbGet_backend,
		KDB_PLUGIN_SET,		&kdbSet_backend,
		KDB_PLUGIN_VERSION,	BACKENDVERSION,
		KDB_PLUGIN_AUTHOR,	"Markus Raab <elektra@markus-raab.org>",
		KDB_PLUGIN_LICENCE,	"BSD",
		KDB_PLUGIN_DESCRIPTION,	"This plugin composes other plugins together to a backend",
		KDB_PLUGIN_NEEDS,	"tracer storage logger",
		KDB_PLUGIN_PROVIDES,	"",
		KDB_PLUGIN_END);
}

