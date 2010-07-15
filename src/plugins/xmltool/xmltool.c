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
		void (*get) () = (void (*) ()) elektraXmltoolGet;
		void (*set) () = (void (*) ()) elektraXmltoolSet;
		void (*fromxml) () = (void (*) ()) ksFromXMLfile;
		void (*toxml) () = (void (*) ()) ksToStream;
		KeySet *moduleConfig = ksNew (30,
			keyNew ("system/elektra/modules/xmltool",
				KEY_VALUE, "xmltool plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/xmltool/exports", KEY_END),
			keyNew ("system/elektra/modules/xmltool/exports/get",
				KEY_SIZE, sizeof (get),
				KEY_BINARY,
				KEY_VALUE, &get, KEY_END),
			keyNew ("system/elektra/modules/xmltool/exports/set",
				KEY_SIZE, sizeof (set),
				KEY_BINARY,
				KEY_VALUE, &set, KEY_END),
			keyNew ("system/elektra/modules/xmltool/exports/ksFromXMLfile",
				KEY_SIZE, sizeof (fromxml),
				KEY_BINARY,
				KEY_VALUE, &fromxml, KEY_END),
			keyNew ("system/elektra/modules/xmltool/exports/ksToStream",
				KEY_SIZE, sizeof (toxml),
				KEY_BINARY,
				KEY_VALUE, &toxml, KEY_END),
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
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END);
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
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

