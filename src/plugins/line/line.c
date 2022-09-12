/**
 * @file
 *
 * @brief A plugin that reads configuration files and saves keys on a line by line basis *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include "line.h"

#include <kdbease.h>
#include <kdberrors.h>

#include <errno.h>
#include <stddef.h>
// The definition `_WITH_GETLINE` is required for FreeBSD
#define _WITH_GETLINE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static inline ElektraKeyset * elektraLineContract (void)
{
	return ksNew (30, keyNew ("system:/elektra/modules/line", ELEKTRA_KEY_VALUE, "line plugin waits for your orders", ELEKTRA_KEY_END),
		      keyNew ("system:/elektra/modules/line/exports", ELEKTRA_KEY_END),
		      keyNew ("system:/elektra/modules/line/exports/get", ELEKTRA_KEY_FUNC, elektraLineGet, ELEKTRA_KEY_END),
		      keyNew ("system:/elektra/modules/line/exports/set", ELEKTRA_KEY_FUNC, elektraLineSet, ELEKTRA_KEY_END),
#include "readme_line.c"
		      keyNew ("system:/elektra/modules/line/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

int elektraLineRead (FILE * fp, ElektraKeyset * returned)
{
	char * value = NULL;
	size_t len = 0;
	ssize_t n = 0;
	ElektraKey * read = NULL;

	// Read in each line
	while ((n = getline (&value, &len, fp)) != -1)
	{
		// Remove trailing newline
		if (value[n - 1] == '\n')
		{
			value[n - 1] = '\0';
		}
		read = keyDup (ksTail (returned), ELEKTRA_KEY_CP_ALL);
		if (elektraArrayIncName (read) == -1)
		{
			elektraFree (value);
			keyDel (read);
			return -1;
		}
		keySetString (read, value);

		ksAppendKey (returned, read);
	}
	elektraFree (value);

	return 1;
}


int elektraLineGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	/* get all keys */

	if (!strcmp (keyName (parentKey), "system:/elektra/modules/line"))
	{
		ElektraKeyset * moduleConfig = elektraLineContract ();
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	int errnosave = errno;
	FILE * fp = fopen (keyString (parentKey), "r");

	if (!fp)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return -1;
	}

	ElektraKey * b = keyNew (keyName (parentKey), ELEKTRA_KEY_END);
	ksAppendKey (returned, keyDup (b, ELEKTRA_KEY_CP_ALL)); // start with parentKey
	keyAddName (b, "#");				// start point for our array
	ksAppendKey (returned, b);

	int ret = elektraLineRead (fp, returned);

	// get rid of startpoint, if it was an empty file
	keyDel (ksLookup (returned, b, ELEKTRA_KDB_O_POP));

	if (ret == -1)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Could not increment array from %s", keyName (ksTail (returned)));
		ret = -1;
	}
	else if (feof (fp) == 0)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (parentKey, "Invalid line encountered: not at the end of file");
		ret = -1;
	}

	fclose (fp);

	return ret; /* success */
}

int elektraLineSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	/* set all keys */

	int errnosave = errno;
	FILE * fp = fopen (keyString (parentKey), "w");

	if (!fp)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		errno = errnosave;
		return -1;
	}

	ElektraKey * cur;
	if (!ksLookup (returned, parentKey, 0))
	{
		// ignore parentKey if found
		ksRewind (returned);
	}

	while ((cur = ksNext (returned)) != 0)
	{
		fprintf (fp, "%s\n", keyString (cur));
	}

	fclose (fp);

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("line",
		ELEKTRA_PLUGIN_GET,	&elektraLineGet,
		ELEKTRA_PLUGIN_SET,	&elektraLineSet,
		ELEKTRA_PLUGIN_END);
}
