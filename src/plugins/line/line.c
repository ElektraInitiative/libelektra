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

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>


int elektraLineGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	/* get all keys */

	if (!strcmp (keyName(parentKey), "system/elektra/modules/line"))
	{
		KeySet *moduleConfig = ksNew (30,
			keyNew ("system/elektra/modules/line",
				KEY_VALUE, "line plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/line/exports", KEY_END),
			keyNew ("system/elektra/modules/line/exports/get",
				KEY_FUNC, elektraLineGet, KEY_END),
			keyNew ("system/elektra/modules/line/exports/set",
				KEY_FUNC, elektraLineSet, KEY_END),
			keyNew ("system/elektra/modules/line/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew ("system/elektra/modules/line/infos/author",
				KEY_VALUE, "Ian Donnelly <ian.s.donnelly@gmail.com>", KEY_END),
			keyNew ("system/elektra/modules/line/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
			keyNew ("system/elektra/modules/line/infos/description",
				KEY_VALUE, "Very simple storage which writes out line by line", KEY_END),
			keyNew ("system/elektra/modules/line/infos/provides",
				KEY_VALUE, "storage", KEY_END),
			keyNew ("system/elektra/modules/line/infos/placements",
				KEY_VALUE, "getstorage setstorage", KEY_END),
			keyNew ("system/elektra/modules/line/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END);
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}
	char *value = NULL;
	char *key;
	Key *read;
	int i;
	int digits = 0;
	size_t numberSize;
	size_t stringSize;
	size_t len = 0;
	ssize_t n;
	FILE *fp = fopen (keyString(parentKey), "r");
	if (!fp)
	{
		// ELEKTRA_SET_ERROR(74, parentKey, keyString(parentKey));
		// return -1;
		return 0; // we just ignore if we could not open file
	}
	//Find # of lines
	char ch;
	while(!feof(fp)){
  		ch = fgetc(fp);
  		if(ch == '\n')
    			digits++;
	}
	for(i = 0; digits > 0; i++) 
		digits /= 10;
	digits = i;
	rewind(fp);
	i = 0;
	//Read in each line
	while ((n = getline(&value, &len, fp)) != -1)
	{
		//Remove trailing newline
		if (value[strlen(value) - 1] == '\n') {
  			value[strlen(value) - 1] = '\0';
		}
		i++;
		//Set key to correct size
		numberSize = snprintf(0, 0, "%0*d", digits, i);
		stringSize = sizeof("line") + numberSize + 1;
		key = malloc(stringSize);
		//Append i to line
		snprintf (key, stringSize, "line%0*d", digits, i);
		read = keyDup(parentKey);
		if (keyAddBaseName(read, key) == -1)
		{
			fclose (fp);
			keyDel (read);
			ELEKTRA_SET_ERROR(59, parentKey, key);
			return -1;
		}
		keySetString(read, value);

		ksAppendKey (returned, read);
		free (key);
		len=0;
		free(value);
	}

	if (feof(fp) == 0)
	{
		fclose (fp);
		ELEKTRA_SET_ERROR(60, parentKey, "not at the end of file");
		return -1;
	}

	fclose (fp);

	return 1; /* success */
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
