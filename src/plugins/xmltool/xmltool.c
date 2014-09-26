/***************************************************************************
                     xmltool.c  -  Skeleton of a plugin
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


#include "xmltool.h"
#include "kdbtools.h"

#include "kdbconfig.h"

#include <string.h>
#include <stdio.h>

int elektraXmltoolGet(Plugin *handle ELEKTRA_UNUSED,
		      KeySet *returned,
		      Key *parentKey)
{
	if (!strcmp (keyName(parentKey), "system/elektra/modules/xmltool"))
	{
		KeySet *moduleConfig = ksNew (30,
			keyNew ("system/elektra/modules/xmltool",
				KEY_VALUE, "xmltool plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/xmltool/exports", KEY_END),
			keyNew ("system/elektra/modules/xmltool/exports/get",
				KEY_FUNC, elektraXmltoolGet, KEY_END),
			keyNew ("system/elektra/modules/xmltool/exports/set",
				KEY_FUNC, elektraXmltoolSet, KEY_END),
			keyNew ("system/elektra/modules/xmltool/exports/ksFromXML",
				KEY_FUNC, ksFromXMLfile, KEY_END),
			keyNew ("system/elektra/modules/xmltool/exports/ksToStream",
				KEY_FUNC, ksToStream, KEY_END),
#include "readme_xmltool.c"
			keyNew ("system/elektra/modules/xmltool/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END);
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	/* get all keys */
	return ksFromXMLfile(returned, keyString(parentKey));
}

int elektraXmltoolSet(Plugin *handle ELEKTRA_UNUSED,
		      KeySet *returned, Key *parentKey)
{
	/* set all keys */

	FILE *fout = fopen (keyString(parentKey), "w");
	if (!fout) return 0;

	ksToStream (returned, fout, KDB_O_HEADER);

	if (fclose (fout)) return -1;

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(xmltool)
{
	return elektraPluginExport("xmltool",
		ELEKTRA_PLUGIN_GET,	&elektraXmltoolGet,
		ELEKTRA_PLUGIN_SET,	&elektraXmltoolSet,
		ELEKTRA_PLUGIN_END);
}

