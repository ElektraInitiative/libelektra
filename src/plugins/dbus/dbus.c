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

int elektraDbusGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	KeySet *pluginConfig = ksNew (30,
		keyNew ("system/elektra/modules/dbus",
			KEY_VALUE, "dbus plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/dbus/exports", KEY_END),
		// TODO exports
		keyNew ("system/elektra/modules/dbus/infos",
			KEY_VALUE, "All information you want to know", KEY_END),
		keyNew ("system/elektra/modules/dbus/infos/author",
			KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
		keyNew ("system/elektra/modules/dbus/infos/licence",
			KEY_VALUE, "BSD", KEY_END),
		keyNew ("system/elektra/modules/dbus/infos/description",
			KEY_VALUE, "Prints timestamps when a method is called", KEY_END),
		keyNew ("system/elektra/modules/dbus/infos/provides",
			KEY_VALUE, "dbus", KEY_END),
		keyNew ("system/elektra/modules/dbus/infos/placements",
			KEY_VALUE, "postcommit", KEY_END),
		keyNew ("system/elektra/modules/dbus/infos/needs",
			KEY_VALUE, "", KEY_END),
		keyNew ("system/elektra/modules/dbus/infos/version",
			KEY_VALUE, "1", KEY_END), // TODO PLUGINVERSION
		KS_END);
	ksAppend (returned, pluginConfig);
	ksDel (pluginConfig);

	return 1; /* success */
}

int elektraDbusSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	if (!strncmp (keyName(parentKey), "user", 4)) elektraDbusSendMessage (DBUS_BUS_SESSION);
	if (!strncmp (keyName(parentKey), "system", 6)) elektraDbusSendMessage (DBUS_BUS_SYSTEM);

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(dbus)
{
	return elektraPluginExport("dbus",
		ELEKTRA_PLUGIN_GET,	&elektraDbusGet,
		ELEKTRA_PLUGIN_SET,	&elektraDbusSet,
		ELEKTRA_PLUGIN_END);
}

