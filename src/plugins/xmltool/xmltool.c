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

#include <string.h>
#include <stdio.h>

int elektraXmltoolGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	if (!strcmp (keyName(parentKey), "system/elektra/modules/xmltool"))
	{
		ksAppend (returned, ksNew (30,
			keyNew ("system/elektra/modules/xmltool",
				KEY_VALUE, "xmltool plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/xmltool/exports", KEY_END),
			keyNew ("system/elektra/modules/xmltool/exports/get",
				KEY_SIZE, sizeof (&elektraXmltoolGet),
				KEY_BINARY,
				KEY_VALUE, &elektraXmltoolGet, KEY_END),
			keyNew ("system/elektra/modules/xmltool/exports/set",
				KEY_SIZE, sizeof (&elektraXmltoolSet),
				KEY_BINARY,
				KEY_VALUE, &elektraXmltoolSet, KEY_END),
			keyNew ("system/elektra/modules/xmltool/exports/ksFromXMLfile",
				KEY_SIZE, sizeof (&ksFromXMLfile),
				KEY_BINARY,
				KEY_VALUE, &ksFromXMLfile, KEY_END),
			keyNew ("system/elektra/modules/xmltool/exports/ksToStream",
				KEY_SIZE, sizeof (&ksToStream),
				KEY_BINARY,
				KEY_VALUE, &ksToStream, KEY_END),
			keyNew ("system/elektra/modules/xmltool/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew ("system/elektra/modules/xmltool/infos/author",
				KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
			keyNew ("system/elektra/modules/xmltool/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
			keyNew ("system/elektra/modules/xmltool/infos/description",
				KEY_VALUE, "Validates key values using regular expressions", KEY_END),
			keyNew ("system/elektra/modules/xmltool/infos/provides",
				KEY_VALUE, "storage", KEY_END),
			keyNew ("system/elektra/modules/xmltool/infos/placements",
				KEY_VALUE, "getstorage setstorage", KEY_END),
			keyNew ("system/elektra/modules/xmltool/infos/needs",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/xmltool/infos/version",
				KEY_VALUE, "1.0", KEY_END),
			KS_END));
		return 1;
	}

	/* get all keys */
	return ksFromXMLfile(returned, keyString(parentKey));
}

int elektraXmltoolSet(Plugin *handle, KeySet *returned, Key *parentKey)
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

