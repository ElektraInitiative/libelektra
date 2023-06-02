/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#define _GNU_SOURCE
#include "./simpleini.h"
#include <errno.h>
#include <internal/config.h>
#include <internal/macros/plugin_errors.h>
#include <internal/utility/alloc.h>
#include <internal/utility/format.h>

#include <elektra/core/errors.h>
#include <elektra/ease/name.h>
#include <internal/utility/assert.h>
#include <internal/utility/logger.h>
#include <internal/utility/string.h>

#include <stdio.h>
#include <stdlib.h>


struct lineFormat
{
	char * format;
	char * delimiter;
};

/**
 * @brief Builds together a format string by the plugin's configuration
 *
 * @param handle to plugin
 * @param first format string for key
 * @param second format string for value
 * @param delimiter pointer to store a newly allocated found delimiter string (string between key and value)
 *
 * @return lineFormat struct with two newly allocated strings (if not NULL)
 */
static struct lineFormat getFormat (Plugin * handle)
{
	struct lineFormat ret;
	// char * format;
	Key * key = ksLookupByName (elektraPluginGetConfig (handle), "/format", 0);
	if (!key)
	{
		ret.format = elektraStrDup ("%s = %s\n");
		ret.delimiter = elektraStrDup (" = ");
	}
	else
	{
		const size_t maxFactor = 2; // at maximum every char is a %, %% -> %%%%
		const size_t newLineAtEnd = 2;
		const size_t userFormatSize = keyGetValueSize (key);
		ret.format = elektraMalloc (userFormatSize * maxFactor + newLineAtEnd);

		char * delimiterStart = NULL;
		char * delimiterEnd = NULL;
		int numFormatSpecifier = 0;

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
					ret.format[j++] = '%';
					ret.format[j++] = '%';
					ret.format[j] = '%';
				}
				else
				{
					// single % -> %s
					numFormatSpecifier++;
					// we only accept 2 format spec
					// (otherwise scanf would access internal mem -> security issue?)
					// use it as '%' so write it twice
					ret.format[j++] = numFormatSpecifier <= 2 ? 's' : '%';
					ret.format[j] = c;

					if (numFormatSpecifier == 1)
					{
						// first format conversion specifier
						// position after '%'
						delimiterStart = (char *) &(userFormat[i]);
					}
					else if (numFormatSpecifier == 2)
					{
						// second format spec.
						// position of '%'
						delimiterEnd = (char *) &(userFormat[i - 1]);
					}
				}
				gotPercent = 0;
			}
			else if (c == '%')
			{
				ret.format[j] = c;
				gotPercent = 1;
			}
			else
			{
				ret.format[j] = c;
			}
		}
		--j; // discard null byte that is already there
		ELEKTRA_ASSERT (ret.format[j] == '\0', "should be null byte at end of string but was %c", ret.format[j]);
		ret.format[j++] = '\n';
		ret.format[j] = '\0';

		// be more robust, if no delimiter was found
		if (delimiterStart == NULL || delimiterEnd == NULL)
		{
			ELEKTRA_LOG_DEBUG ("no delimiter found");
			ret.delimiter = NULL;
		}
		else
		{
			// copy delimiter
			const size_t delimiterLen = delimiterEnd - delimiterStart;
			ret.delimiter = elektraMemDup (delimiterStart, (delimiterLen + 1));
			ret.delimiter[delimiterLen] = '\0';
			ELEKTRA_LOG_DEBUG ("found delimiter:  '%s'", ret.delimiter);
		}
		ELEKTRA_LOG_DEBUG ("format: %s", ret.format);
	}

	return ret;
}

static char * replaceStringFormatSpec (char * format, const char * replace)
{
	size_t formatLen = strlen (format);
	size_t replaceLen = strlen (replace);
	size_t needleLen = strlen ("%s");
	size_t offset = replaceLen - needleLen;

	char * posRepl = strstr (format, "%s");
	if (posRepl)
	{
		char * result = elektraMalloc (formatLen + offset + 1);

		size_t preReplLen = posRepl - format;
		// copy pre replacement
		strncpy (result, format, preReplLen);
		// copy replacement
		strcpy (result + preReplLen, replace);
		// copy post replacement
		strncpy (result + preReplLen + replaceLen, posRepl + needleLen, formatLen - needleLen - preReplLen);
		result[formatLen + offset] = '\0';
		return result;
	}
	else
	{
		return NULL;
	}
}

static char * getReadFormat (Plugin * handle)
{
	struct lineFormat f = getFormat (handle);

	char * keyFormat = NULL;
	if (f.delimiter)
	{
		// scanf key format pattern: read everything until first char of delimiter
		keyFormat = elektraFormat ("%%m[^%c]", f.delimiter[0]);
	}
	else
	{
		// without delimiter, we also do not have two '%s' in the format which would be
		// replaced by our key and value scanf format spec.
		elektraFree (f.format);
		return 0;
	}

	// make scanf format pattern with key format pattern and value format pattern
	// (we do not use simple printf style here, since it would replace '%%' to '%' but we need those
	// escaped '%' for a save scanf format pattern
	char * tmp = replaceStringFormatSpec (f.format, keyFormat);
	ELEKTRA_ASSERT (tmp != 0, "format has to have a '%%s' for the key");

	// replace value format specifier
	char * ret = replaceStringFormatSpec (tmp, "%m[^\n]");
	ELEKTRA_ASSERT (ret != 0, "format has to have a '%%s' for the value");
	elektraFree (tmp);

	elektraFree (keyFormat);
	elektraFree (f.format);
	elektraFree (f.delimiter);

	return ret;
}

static char * getWriteFormat (Plugin * handle)
{
	struct lineFormat f = getFormat (handle);

	elektraFree (f.delimiter);
	return f.format;
}

int elektraSimpleiniGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	/* get all keys */

	if (!strcmp (keyName (parentKey), "system:/elektra/modules/simpleini"))
	{
		KeySet * moduleConfig = ksNew (
			30, keyNew ("system:/elektra/modules/simpleini", KEY_VALUE, "simpleini plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/simpleini/exports", KEY_END),
			keyNew ("system:/elektra/modules/simpleini/exports/get", KEY_FUNC, elektraSimpleiniGet, KEY_END),
			keyNew ("system:/elektra/modules/simpleini/exports/set", KEY_FUNC, elektraSimpleiniSet, KEY_END),
#include "./readme_simpleini.c"
			keyNew ("system:/elektra/modules/simpleini/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
			keyNew ("system:/elektra/modules/simpleini/config/needs", KEY_VALUE,
				"the needed configuration to work in a backend", KEY_END),
			keyNew ("system:/elektra/modules/simpleini/config/needs/chars", KEY_VALUE, "Characters needed", KEY_END),
			// space in value now works:
			// TODO: characters present in format should be escaped
			/*
			keyNew ("system:/elektra/modules/simpleini/config/needs/chars/20", KEY_VALUE, "61", KEY_END), // space -> a
			keyNew ("system:/elektra/modules/simpleini/config/needs/chars/23", KEY_VALUE, "62", KEY_END), // # -> b
			keyNew ("system:/elektra/modules/simpleini/config/needs/chars/25", KEY_VALUE, "63",
				KEY_END), // % -> c (escape character)
			keyNew ("system:/elektra/modules/simpleini/config/needs/chars/3B", KEY_VALUE, "64", KEY_END), // ; -> d
			keyNew ("system:/elektra/modules/simpleini/config/needs/chars/3D", KEY_VALUE, "65", KEY_END), // = -> e
			keyNew ("system:/elektra/modules/simpleini/config/needs/chars/5C", KEY_VALUE, "66", KEY_END), // \\ -> f
			*/
			keyNew ("system:/elektra/modules/simpleini/config/needs/chars/0A", KEY_VALUE, "67", KEY_END), // enter (NL) -> g
			keyNew ("system:/elektra/modules/simpleini/config/needs/chars/0D", KEY_VALUE, "68", KEY_END), // CR -> h
			keyNew ("system:/elektra/modules/simpleini/config/needs/escape", KEY_VALUE, "25", KEY_END), KS_END);
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	char * key = 0;
	char * strippedkey = 0;
	char * value = 0;
	int errnosave = errno;

	char * format = getReadFormat (handle);
	if (!format)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (parentKey, "Invalid 'format' specified");
		return -1;
	}

	ELEKTRA_LOG ("Read from '%s' with format '%s'", keyString (parentKey), format);

	const char * filename = keyString (parentKey);
	FILE * fp = fopen (filename, "r");
	if (!fp)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		elektraFree (format);
		return -1;
	}

	int n = 0;
	size_t size = 0;
	ssize_t ksize = 0;
#pragma GCC diagnostic ignored "-Wformat"
	// icc warning #269: invalid format string conversion
	// key and value will be both newly allocated strings
	while ((n = fscanf (fp, format, &key, &value)) >= 0)
	{
		ELEKTRA_LOG_DEBUG ("Read %d parts: '%s' with value '%s'", n, key, value);
		if (n == 0)
		{
			// discard line
			if (getline (&key, &size, fp) == -1 && !feof (fp))
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
					parentKey, "Failed discarding rest of line of file %s at position %ld with key %s", filename,
					ftell (fp), key);
				elektraFree (key);
				fclose (fp);
				return -1;
			}
			ELEKTRA_LOG_DEBUG ("Discard '%s'", key);
			elektraFree (key);
			key = 0;
			continue;
		}

		Key * read = keyNew (keyName (parentKey), KEY_END);
		strippedkey = elektraStrip (key);

		if (keyAddName (read, strippedkey) == -1)
		{
			ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF (parentKey, "Key name '%s' is not valid, discarding key", strippedkey);
			keyDel (read);
			elektraFree (key);
			if (n == 2)
			{
				elektraFree (value);
			}
			continue;
		}

		if (n == 2)
		{
			keySetString (read, value);
			elektraFree (value);
			value = 0;
		}

		elektraFree (key);
		key = 0;

		if (ksAppendKey (returned, read) != ksize + 1)
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Duplicated key '%s' at position %ld in file %s",
								 keyName (read), ftell (fp), filename);
			elektraFree (format);
			fclose (fp);
			return -1;
		}
		++ksize;
	}

	if (feof (fp) == 0)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Not at the end of file at position %ld in file %s", ftell (fp),
							 filename);
		elektraFree (format);
		fclose (fp);
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

	char * format = getWriteFormat (handle);

	ELEKTRA_LOG ("Write to '%s' with format '%s'", keyString (parentKey), format);

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		Key * cur = ksAtCursor (returned, it);
		const char * name = elektraKeyGetRelativeName (cur, parentKey);
		fprintf (fp, format, name, keyString (cur));
	}

	fclose (fp);

	elektraFree (format);
	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("simpleini",
		ELEKTRA_PLUGIN_GET,	&elektraSimpleiniGet,
		ELEKTRA_PLUGIN_SET,	&elektraSimpleiniSet,
		ELEKTRA_PLUGIN_END);
}

