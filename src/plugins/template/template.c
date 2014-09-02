/***************************************************************************
                     template.c  -  Skeleton of a plugin
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

#include <string.h>

#include "template.h"

int elektraTemplateOpen(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	/* plugin initialization logic */

	return 1; /* success */
}

int elektraTemplateClose(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	/* free all plugin resources and shut it down */

	return 1; /* success */
}

int elektraTemplateGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	if (!strcmp(keyName(parentKey), "system/elektra/modules/template"))
	{
		KeySet *contract = ksNew (30,
		keyNew ("system/elektra/modules/template",
			KEY_VALUE, "dbus plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/template/exports", KEY_END),
		keyNew ("system/elektra/modules/template/exports/open",
			KEY_FUNC, elektraTemplateOpen, KEY_END),
		keyNew ("system/elektra/modules/template/exports/close",
			KEY_FUNC, elektraTemplateClose, KEY_END),
		keyNew ("system/elektra/modules/template/exports/get",
			KEY_FUNC, elektraTemplateGet, KEY_END),
		keyNew ("system/elektra/modules/template/exports/set",
			KEY_FUNC, elektraTemplateSet, KEY_END),
		keyNew ("system/elektra/modules/template/exports/error",
			KEY_FUNC, elektraTemplateError, KEY_END),
#include ELEKTRA_README(template)
		keyNew ("system/elektra/modules/template/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}
	/* get all keys */

	return 1; /* success */
}

int elektraTemplateSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}

int elektraTemplateError(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(template)
{
	return elektraPluginExport("template",
		ELEKTRA_PLUGIN_OPEN,	&elektraTemplateOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraTemplateClose,
		ELEKTRA_PLUGIN_GET,	&elektraTemplateGet,
		ELEKTRA_PLUGIN_SET,	&elektraTemplateSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraTemplateError,
		ELEKTRA_PLUGIN_END);
}

