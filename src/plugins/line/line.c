/**
 * \file
 *
 * \brief A plugin that reads configuration files and saves keys on a line by line basis *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include "line.h"

#include <kdberrors.h>
#include <kdbproposal.h>

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

static inline KeySet *elektraLineContract()
{
	return ksNew (30,
		keyNew ("system/elektra/modules/line",
			KEY_VALUE, "line plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/line/exports", KEY_END),
		keyNew ("system/elektra/modules/line/exports/get",
			KEY_FUNC, elektraLineGet, KEY_END),
		keyNew ("system/elektra/modules/line/exports/set",
			KEY_FUNC, elektraLineSet, KEY_END),
#include "readme_line.c"
		keyNew ("system/elektra/modules/line/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
	KS_END);
}

int elektraLineRead(FILE * fp, KeySet * returned)
{
	char *value = NULL;
	size_t len = 0;
	ssize_t n = 0;
	Key *read = NULL;

	//Read in each line
	while ((n = getline(&value, &len, fp)) != -1)
	{
		//Remove trailing newline
		if (value[n - 1] == '\n')
		{
			value[n - 1] = '\0';
		}
		read = keyDup(ksTail(returned));
		if (elektraArrayIncName(read) == -1)
		{
			free (value);
			keyDel (read);
			return -1;
		}
		// TODO: check for null keys
		keySetString(read, value);

		ksAppendKey (returned, read);
	}
	free(value);

	return 1;
}


int elektraLineGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	/* get all keys */

	if (!strcmp (keyName(parentKey), "system/elektra/modules/line"))
	{
		KeySet *moduleConfig = elektraLineContract();
		ksAppend(returned, moduleConfig);
		ksDel(moduleConfig);
		return 1;
	}

	FILE *fp = fopen (keyString(parentKey), "r");
	if (!fp)
	{
		// ELEKTRA_SET_ERROR(74, parentKey, keyString(parentKey));
		// return -1;
		return 0; // we just ignore if we could not open file
	}

	ksAppendKey (returned, keyDup(parentKey)); // start with parentKey

	int ret = elektraLineRead(fp, returned);

	if (ret == -1)
	{
			ELEKTRA_SET_ERROR(59, parentKey,
					"could not increment array");
	}
	else if (feof(fp) == 0)
	{
		ELEKTRA_SET_ERROR(60, parentKey, "not at the end of file");
		ret = -1;
	}

	fclose (fp);

	return ret ; /* success */
}

int elektraLineSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	/* set all keys */

	FILE *fp = fopen(keyString(parentKey), "w");

	if (!fp)
	{
		ELEKTRA_SET_ERROR(74, parentKey, keyString(parentKey));
		return -1;
	}

	Key *cur;
	ksRewind (returned);
	while ((cur = ksNext(returned)) != 0)
	{
		fprintf (fp, "%s\n", keyString(cur));
	}

	fclose (fp);

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(line)
{
	return elektraPluginExport("line",
		ELEKTRA_PLUGIN_GET,	&elektraLineGet,
		ELEKTRA_PLUGIN_SET,	&elektraLineSet,
		ELEKTRA_PLUGIN_END);
}
