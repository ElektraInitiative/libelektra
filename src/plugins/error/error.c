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

int kdbOpen_error(Plugin *handle)
{
	/* plugin initialization logic */

	return 0; /* success */
}

int kdbClose_error(Plugin *handle)
{
	/* free all plugin resources and shut it down */

	return 0; /* success */
}

ssize_t kdbGet_error(Plugin *handle, KeySet *returned, const Key *parentKey)
{
	return -1; /* error */
}

ssize_t kdbSet_error(Plugin *handle, KeySet *returned, const Key *parentKey)
{
	return -1; /* error */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(error)
{
	return elektraPluginExport(BACKENDNAME,
		ELEKTRA_PLUGIN_OPEN,	&kdbOpen_error,
		ELEKTRA_PLUGIN_CLOSE,	&kdbClose_error,
		ELEKTRA_PLUGIN_GET,	&kdbGet_error,
		ELEKTRA_PLUGIN_SET,	&kdbSet_error,
		ELEKTRA_PLUGIN_END);
}

