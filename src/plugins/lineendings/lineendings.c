/**
 * @file
 *
 * @brief Source for lineendings plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef HAVE_KDBCONFIG
#include <internal/kdb/config.h>
#endif

#include "./lineendings.h"
#include <elektra/kdb/errors.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define LF_BYTE 0x0A
#define CR_BYTE 0x0D

#define ERROR(asWarning, warning, error, key, reason, va)                                                                                  \
	if (asWarning)                                                                                                                     \
		warning (key, reason, va);                                                                                                 \
	else                                                                                                                               \
		error (key, reason, va);

typedef enum
{
	NA,
	CR,
	LF,
	CRLF,
	LFCR,
	NUM_TYPES
} Lineending;

static inline char * LEString (Lineending index)
{
	static char * strings[] = { "NA", "CR", "LF", "CRLF", "LFCR" };
	if (index > NUM_TYPES) return NULL;
	return strings[index];
}
static Lineending strToLE (const char * str)
{
	uint8_t counter = 0;
	for (; counter < NUM_TYPES; ++counter)
	{
		if (!strcmp (LEString (counter), str)) return counter;
	}
	return NA;
}

/**
 * Check the line endings for inconsistencies and invalid values
 * @param fileName[in] The absolute path of the file to check
 * @param validLineEnding[in] The line ending that should be considered as valid
 * @param errorsAsWarnings[in] Produce warnings instead of errors
 * @retval 0 if everything was ok, -1 if file not found, -2 if invalid line ending detected, -3 if inconsistent line ending detected
 */
static int checkLineEndings (const char * fileName, Lineending validLineEnding, Key * parentKey, bool errorsAsWarnings)
{
	FILE * fp;
	fp = fopen (fileName, "rb");
	if (fp == NULL)
	{
		ERROR (errorsAsWarnings, ELEKTRA_ADD_RESOURCE_WARNINGF, ELEKTRA_SET_RESOURCE_ERRORF, parentKey, "Couldn't open file %s\n",
		       keyString (parentKey))
		return -1;
	}

	Lineending lineEnding = NA;
	Lineending found = NA;
	uint8_t fc, sc;
	unsigned long line = 1;
	fc = sc = 0;
	(void) fread (&fc, 1, 1, fp);
	while (!feof (fp))
	{
		(void) fread (&sc, 1, 1, fp);
		switch (fc)
		{
		case LF_BYTE:
			if (sc == CR_BYTE)
				found = LFCR;
			else if (sc == LF_BYTE)
				found = LF;
			else if (sc)
				found = LF;
			break;
		case CR_BYTE:
			if (sc == LF_BYTE)
				found = CRLF;
			else if (sc == CR_BYTE)
				found = CR;
			else if (sc)
				found = CR;
			break;
		}
		if (found == CRLF || found == LFCR)
		{
			(void) fread (&sc, 1, 1, fp);
		}
		if (lineEnding == NA && found != NA)
		{
			lineEnding = found;
			if (validLineEnding != NA && lineEnding != validLineEnding)
			{
				fclose (fp);
				ERROR (errorsAsWarnings, ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF, ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF,
				       parentKey, "Invalid line ending at line %lu", line)
				return -2;
			}
		}
		else if (lineEnding != found && found != NA)
		{
			fclose (fp);
			ERROR (errorsAsWarnings, ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF, ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF,
			       parentKey, "Inconsistent line endings at line %lu", line)
			return -3;
		}
		if (found != NA) line++;
		fc = sc;
		found = NA;
	}
	fclose (fp);
	return 0;
}

int elektraLineendingsGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/lineendings"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system:/elektra/modules/lineendings", KEY_VALUE, "lineendings plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/lineendings/exports", KEY_END),
			keyNew ("system:/elektra/modules/lineendings/exports/get", KEY_FUNC, elektraLineendingsGet, KEY_END),
			keyNew ("system:/elektra/modules/lineendings/exports/commit", KEY_FUNC, elektraLineendingsCommit, KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/lineendings/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS; /* success */
	}
	/* get all keys */
	KeySet * config = elektraPluginGetConfig (handle);
	Key * valid = ksLookupByName (config, "/valid", 0);
	Lineending validLineEnding = strToLE (keyString (valid));

	(void) checkLineEndings (keyString (parentKey), validLineEnding, parentKey, true);

	/* Always return ELEKTRA_PLUGIN_STATUS_SUCCESS. We don't want kdbGet() to fail because of validation problems. */
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraLineendingsCommit (Plugin * handle, KeySet * returned ELEKTRA_UNUSED, Key * parentKey)
{
	KeySet * config = elektraPluginGetConfig (handle);
	Key * valid = ksLookupByName (config, "/valid", 0);
	Lineending validLineEnding = strToLE (keyString (valid));

	int ret = checkLineEndings (keyString (parentKey), validLineEnding, parentKey, false);
	switch (ret)
	{
	case (-1):
	case (-2):
	case (-3):
		return ELEKTRA_PLUGIN_STATUS_ERROR;
		break;
	case 0:
	default:
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
		break;
	}
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("lineendings",
			ELEKTRA_PLUGIN_GET,	&elektraLineendingsGet,
			ELEKTRA_PLUGIN_COMMIT,	&elektraLineendingsCommit,
			//	ELEKTRA_PLUGIN_ERROR,	&elektraLineendingsError,
			ELEKTRA_PLUGIN_END);
}

