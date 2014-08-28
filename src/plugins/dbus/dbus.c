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

#include "kdbconfig.h"

int elektraDbusGet(Plugin *handle ELEKTRA_UNUSED,
		   KeySet *returned,
		   Key *parentKey ELEKTRA_UNUSED)
{
	KeySet *pluginConfig = ksNew (30,
		keyNew ("system/elektra/modules/dbus",
			KEY_VALUE, "dbus plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/dbus/exports", KEY_END),
		keyNew ("system/elektra/modules/dbus/exports/get",
			KEY_FUNC, elektraDbusGet, KEY_END),
		keyNew ("system/elektra/modules/dbus/exports/set",
			KEY_FUNC, elektraDbusSet, KEY_END),
#include "readme_dbus.c"
		keyNew ("system/elektra/modules/dbus/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END);
	ksAppend (returned, pluginConfig);
	ksDel (pluginConfig);

	return 1; /* success */
}

int elektraDbusSet(Plugin *handle ELEKTRA_UNUSED,
		   KeySet *returned ELEKTRA_UNUSED,
		   Key *parentKey)
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

