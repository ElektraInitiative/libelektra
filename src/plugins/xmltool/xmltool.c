/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "xmltool.h"
#include "kdbtools.h"

#include <errno.h>

#include <elektra/kdb/errors.h>
#include <internal/kdb/config.h>
#include <internal/macros/plugin_errors.h>

#include <stdio.h>
#include <string.h>

int elektraXmltoolGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/xmltool"))
	{
		KeySet * moduleConfig =
			ksNew (30, keyNew ("system:/elektra/modules/xmltool", KEY_VALUE, "xmltool plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/xmltool/exports", KEY_END),
			       keyNew ("system:/elektra/modules/xmltool/exports/get", KEY_FUNC, elektraXmltoolGet, KEY_END),
			       keyNew ("system:/elektra/modules/xmltool/exports/set", KEY_FUNC, elektraXmltoolSet, KEY_END),
			       keyNew ("system:/elektra/modules/xmltool/exports/ksFromXML", KEY_FUNC, ksFromXMLfile, KEY_END),
			       keyNew ("system:/elektra/modules/xmltool/exports/ksToStream", KEY_FUNC, ksToStream, KEY_END),
#include "readme_xmltool.c"
			       keyNew ("system:/elektra/modules/xmltool/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	/* get all keys */
	int errnosave = errno;
	if (ksFromXMLfile (returned, keyString (parentKey)) == -1)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return -1;
	}
	return 1;
}

int elektraXmltoolSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	/* set all keys */

	int errnosave = errno;
	FILE * fout = fopen (keyString (parentKey), "w");

	if (fout == 0)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		errno = errnosave;
		return -1;
	}

	// TODO: proper error handling + use correct errors
	ksToStream (returned, fout, KDB_O_HEADER);

	if (fclose (fout))
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		errno = errnosave;
		return -1;
	}

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("xmltool",
		ELEKTRA_PLUGIN_GET,	&elektraXmltoolGet,
		ELEKTRA_PLUGIN_SET,	&elektraXmltoolSet,
		ELEKTRA_PLUGIN_END);
}

