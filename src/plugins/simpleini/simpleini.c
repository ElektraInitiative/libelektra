/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#define _GNU_SOURCE
#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include "simpleini.h"
#include <errno.h>

#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <kdblogger.h>

#include <stdio.h>
#include <stdlib.h>

/**
 * @brief Builds together a format string by the plugin's configuration
 *
 * @param handle to plugin
 * @param first format string for key
 * @param second format string for value
 *
 * @return newly allocated format (to be freed with elektraFree);
 */
static char * getFormat (Plugin * handle, const char * first, const char * second)
{
	char * format;
	Key * key = ksLookupByName (elektraPluginGetConfig (handle), "/format", 0);
	if (!key)
	{
		format = elektraStrDup ("%s = %s\n");
	}
	else
	{
		const size_t maxFactor = 2; // at maximum every char is a %, %% -> %%%%
		const size_t newLineAtEnd = 2;
		const size_t userFormatSize = keyGetValueSize (key);
		format = elektraMalloc (userFormatSize * maxFactor + newLineAtEnd);

		const char * userFormat = keyString (key);
		int gotPercent = 0;
		size_t j = 0;
		for (size_t i = 0; i < userFormatSize; ++i, ++j)
		{
			const char c = userFormat[i];
			if (gotPercent)
			{
				if (c == '%')
				{
					// escaped %% -> %%%%
					format[j++] = '%';
					format[j++] = '%';
					format[j] = '%';
				}
				else
				{
					// single % -> %s
					format[j++] = 's';
					format[j] = c;
				}
				gotPercent = 0;
			}
			else if (c == '%')
			{
				format[j] = c;
				gotPercent = 1;
			}
			else
			{
				format[j] = c;
			}
		}
		--j; // discard null byte that is already there
		ELEKTRA_ASSERT (format[j] == '\0', "should be null byte at end of string but was %c", format[j]);
		format[j++] = '\n';
		format[j] = '\0';
	}

	char * ret = elektraFormat (format, first, second);
	elektraFree (format);
	return ret;
}

int elektraSimpleiniGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	/* get all keys */

	if (!strcmp (keyName (parentKey), "system/elektra/modules/simpleini"))
	{
		KeySet * moduleConfig = ksNew (
			30, keyNew ("system/elektra/modules/simpleini", KEY_VALUE, "simpleini plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/simpleini/exports", KEY_END),
			keyNew ("system/elektra/modules/simpleini/exports/get", KEY_FUNC, elektraSimpleiniGet, KEY_END),
			keyNew ("system/elektra/modules/simpleini/exports/set", KEY_FUNC, elektraSimpleiniSet, KEY_END),
#include "readme_simpleini.c"
			keyNew ("system/elektra/modules/simpleini/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
			keyNew ("system/elektra/modules/simpleini/config/needs", KEY_VALUE, "the needed configuration to work in a backend",
				KEY_END),
			keyNew ("system/elektra/modules/simpleini/config/needs/chars", KEY_VALUE, "Characters needed", KEY_END),
			// space in value now works:
			// TODO: characters present in format should be escaped
			/*
			keyNew ("system/elektra/modules/simpleini/config/needs/chars/20", KEY_VALUE, "61", KEY_END), // space -> a
			keyNew ("system/elektra/modules/simpleini/config/needs/chars/23", KEY_VALUE, "62", KEY_END), // # -> b
			keyNew ("system/elektra/modules/simpleini/config/needs/chars/25", KEY_VALUE, "63",
				KEY_END), // % -> c (escape character)
			keyNew ("system/elektra/modules/simpleini/config/needs/chars/3B", KEY_VALUE, "64", KEY_END), // ; -> d
			keyNew ("system/elektra/modules/simpleini/config/needs/chars/3D", KEY_VALUE, "65", KEY_END), // = -> e
			keyNew ("system/elektra/modules/simpleini/config/needs/chars/5C", KEY_VALUE, "66", KEY_END), // \\ -> f
			*/
			keyNew ("system/elektra/modules/simpleini/config/needs/chars/0A", KEY_VALUE, "67", KEY_END), // enter (NL) -> g
			keyNew ("system/elektra/modules/simpleini/config/needs/chars/0D", KEY_VALUE, "68", KEY_END), // CR -> h
			keyNew ("system/elektra/modules/simpleini/config/needs/escape", KEY_VALUE, "25", KEY_END), KS_END);
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	char * key = 0;
	char * value = 0;
	int errnosave = errno;
	FILE * fp = fopen (keyString (parentKey), "r");
	if (!fp)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return -1;
	}

	char * format = getFormat (handle, "%ms", "%m[^\n]");

	ELEKTRA_LOG ("Read from '%s' with format '%s'", keyString (parentKey), format);

	int n = 0;
	size_t size = 0;
	ssize_t ksize = 0;
#pragma GCC diagnostic ignored "-Wformat"
	// icc warning #269: invalid format string conversion
	while ((n = fscanf (fp, format, &key, &value)) >= 0)
	{
		ELEKTRA_LOG_DEBUG ("Read %d parts: '%s' with value '%s'", n, key, value);
		if (n == 0)
		{
			// discard line
			getline (&key, &size, fp);
			ELEKTRA_LOG_DEBUG ("Discard '%s'", key);
			elektraFree (key);
			key = 0;
			continue;
		}

		Key * read = keyNew (keyName (parentKey), KEY_END);

		if (keyAddName (read, key) == -1)
		{
			ELEKTRA_ADD_WARNING (ELEKTRA_WARNING_INVALID_KEY, parentKey, key);
			keyDel (read);
			continue;
		}

		if (n == 2)
		{
			keySetString (read, value);
			elektraFree (value);
			value = 0;
		}

		if (ksAppendKey (returned, read) != ksize + 1)
		{
			ELEKTRA_SET_ERROR (ELEKTRA_ERROR_NOEOF, parentKey, "duplicated key");
			return -1;
		}
		++ksize;
		elektraFree (key);
		key = 0;
	}

	if (feof (fp) == 0)
	{
		elektraFree (format);
		fclose (fp);
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_NOEOF, parentKey, "not at the end of file");
		return -1;
	}

	elektraFree (format);
	fclose (fp);

	return 1; /* success */
}

int elektraSimpleiniSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	/* set all keys */

	FILE * fp = fopen (keyString (parentKey), "w");
	if (!fp)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		return -1;
	}

	char * format = getFormat (handle, "%s", "%s");

	ELEKTRA_LOG ("Write to '%s' with format '%s'", keyString (parentKey), format);

	Key * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != 0)
	{
		const char * name = elektraKeyGetRelativeName (cur, parentKey);
		fprintf (fp, format, name, keyString (cur));
	}

	fclose (fp);

	elektraFree (format);
	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT (simpleini)
{
	// clang-format off
	return elektraPluginExport("simpleini",
		ELEKTRA_PLUGIN_GET,	&elektraSimpleiniGet,
		ELEKTRA_PLUGIN_SET,	&elektraSimpleiniSet,
		ELEKTRA_PLUGIN_END);
}

