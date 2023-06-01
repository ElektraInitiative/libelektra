/**
 * @file
 *
 * @brief A plugin that reads configuration files and saves keys on a line by line basis *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


// The definition `_WITH_GETLINE` is required for FreeBSD
#define _WITH_GETLINE
#include <stdio.h>

#include <internal/config.h>
#include <internal/macros/plugin_errors.h>
#include <internal/utility/old_helper.h>

#include "./line.h"

#include <elektra/core/errors.h>
#include <elektra/ease/array.h>

#include <errno.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

static inline KeySet * elektraLineContract (void)
{
	return ksNew (30, keyNew ("system:/elektra/modules/line", KEY_VALUE, "line plugin waits for your orders", KEY_END),
		      keyNew ("system:/elektra/modules/line/exports", KEY_END),
		      keyNew ("system:/elektra/modules/line/exports/get", KEY_FUNC, elektraLineGet, KEY_END),
		      keyNew ("system:/elektra/modules/line/exports/set", KEY_FUNC, elektraLineSet, KEY_END),
#include "./readme_line.c"
		      keyNew ("system:/elektra/modules/line/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
}

int elektraLineRead (FILE * fp, KeySet * returned)
{
	char * value = NULL;
	size_t len = 0;
	ssize_t n = 0;
	Key * read = NULL;

	// Read in each line
	while ((n = getline (&value, &len, fp)) != -1)
	{
		// Remove trailing newline
		if (value[n - 1] == '\n')
		{
			value[n - 1] = '\0';
		}

		read = keyDup (ksAtCursor (returned, ksGetSize (returned) - 1), KEY_CP_ALL);
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


int elektraLineGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	/* get all keys */

	if (!strcmp (keyName (parentKey), "system:/elektra/modules/line"))
	{
		KeySet * moduleConfig = elektraLineContract ();
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

	Key * b = keyNew (keyName (parentKey), KEY_END);
	ksAppendKey (returned, keyDup (b, KEY_CP_ALL)); // start with parentKey
	keyAddName (b, "#");				// start point for our array
	ksAppendKey (returned, b);

	int ret = elektraLineRead (fp, returned);

	// get rid of startpoint, if it was an empty file
	keyDel (ksLookup (returned, b, KDB_O_POP));

	if (ret == -1)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Could not increment array from %s",
							 keyName (ksAtCursor (returned, ksGetSize (returned) - 1)));
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

int elektraLineSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
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

	Key * cur;
	elektraCursor it = ksSearch (returned, parentKey);

	if (it < 0)
		it = 0;
	else
		it++; // ignore parentKey if found

	for (; it < ksGetSize (returned); ++it)
	{
		cur = ksAtCursor (returned, it);
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
