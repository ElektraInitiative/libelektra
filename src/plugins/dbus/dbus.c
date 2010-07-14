/***************************************************************************
                     dbus.c  -  Skeleton of a plugin
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


#include "dbus.h"

int elektraDbusOpen(Plugin *handle, Key *errorKey)
{
	/* plugin initialization logic */

	return 1; /* success */
}

int elektraDbusClose(Plugin *handle, Key *errorKey)
{
	/* free all plugin resources and shut it down */

	return 1; /* success */
}

int elektraDbusGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* get all keys */

	return 1; /* success */
}

int elektraDbusSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* set all keys */

	return 1; /* success */
}

int elektraDbusError(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* set all keys */

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(dbus)
{
	return elektraPluginExport("dbus",
		ELEKTRA_PLUGIN_OPEN,	&elektraDbusOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraDbusClose,
		ELEKTRA_PLUGIN_GET,	&elektraDbusGet,
		ELEKTRA_PLUGIN_SET,	&elektraDbusSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraDbusError,
		ELEKTRA_PLUGIN_END);
}

