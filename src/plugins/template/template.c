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


#include "template.h"

int elektraTemplateOpen(Plugin *handle, Key *errorKey)
{
	/* plugin initialization logic */

	return 1; /* success */
}

int elektraTemplateClose(Plugin *handle, Key *errorKey)
{
	/* free all plugin resources and shut it down */

	return 1; /* success */
}

int elektraTemplateGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* get all keys */

	return 1; /* success */
}

int elektraTemplateSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* set all keys */

	return 1; /* success */
}

int elektraTemplateError(Plugin *handle, KeySet *returned, Key *parentKey)
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

