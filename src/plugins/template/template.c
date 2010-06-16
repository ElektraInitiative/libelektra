/***************************************************************************
          template.c  -  Skeleton of a plugin to be copied
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
 *   Simple fill the empty _template functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "template.h"

int kdbOpen_template(Plugin *handle)
{
	/* plugin initialization logic */

	return 0; /* success */
}

int kdbClose_template(Plugin *handle)
{
	/* free all plugin resources and shut it down */

	return 0; /* success */
}

int kdbGet_template(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* get all keys */

	return 1; /* success */
}

int kdbSet_template(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* set all keys */

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(template)
{
	return elektraPluginExport("template",
		ELEKTRA_PLUGIN_OPEN,	&kdbOpen_template,
		ELEKTRA_PLUGIN_CLOSE,	&kdbClose_template,
		ELEKTRA_PLUGIN_GET,	&kdbGet_template,
		ELEKTRA_PLUGIN_SET,	&kdbSet_template,
		ELEKTRA_PLUGIN_END);
}

