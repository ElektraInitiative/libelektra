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

#include "kdbconfig.h"
#include "kdberrors.h"

#include <stdio.h>
#include <string.h>

int elektraXmltoolGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/xmltool"))
	{
		ElektraKeyset * moduleConfig =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/xmltool", ELEKTRA_KEY_VALUE, "xmltool plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/xmltool/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/xmltool/exports/get", ELEKTRA_KEY_FUNC, elektraXmltoolGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/xmltool/exports/set", ELEKTRA_KEY_FUNC, elektraXmltoolSet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/xmltool/exports/ksFromXML", ELEKTRA_KEY_FUNC, ksFromXMLfile, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/xmltool/exports/ksToStream", ELEKTRA_KEY_FUNC, ksToStream, ELEKTRA_KEY_END),
#include "readme_xmltool.c"
			       elektraKeyNew ("system:/elektra/modules/xmltool/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, moduleConfig);
		elektraKeysetDel (moduleConfig);
		return 1;
	}

	/* get all keys */
	int errnosave = errno;
	if (ksFromXMLfile (returned, elektraKeyString (parentKey)) == -1)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return -1;
	}
	return 1;
}

int elektraXmltoolSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	/* set all keys */

	int errnosave = errno;
	FILE * fout = fopen (elektraKeyString (parentKey), "w");

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

