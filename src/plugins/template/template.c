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

