/**
 * @file
 *
 * @brief Source for lineendings plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include "lineendings.h"
#include <kdberrors.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define LF_BYTE 0x0A
#define CR_BYTE 0x0D

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
static int checkLineEndings (const char * fileName, Lineending validLineEnding, ElektraKey * parentKey)
{
	FILE * fp;
	fp = fopen (fileName, "rb");
	if (fp == NULL)
	{
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
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Invalid line ending at line %lu", line);
				return -2;
			}
			++line;
		}
		else if (lineEnding != found && found != NA)
		{
			fclose (fp);
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Inconsistent line endings at line %lu", line);
			return -3;
		}
		fc = sc;
		found = NA;
	}
	fclose (fp);
	return 0;
}

int elektraLineendingsGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/lineendings"))
	{
		ElektraKeyset * contract = elektraKeysetNew (
			30, elektraKeyNew ("system:/elektra/modules/lineendings", ELEKTRA_KEY_VALUE, "lineendings plugin waits for your orders", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/lineendings/exports", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/lineendings/exports/get", ELEKTRA_KEY_FUNC, elektraLineendingsGet, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/lineendings/exports/set", ELEKTRA_KEY_FUNC, elektraLineendingsSet, ELEKTRA_KEY_END),
#include ELEKTRA_README
			elektraKeyNew ("system:/elektra/modules/lineendings/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return 1; /* success */
	}
	/* get all keys */
	ElektraKeyset * config = elektraPluginGetConfig (handle);
	ElektraKey * valid = elektraKeysetLookupByName (config, "/valid", 0);
	Lineending validLineEnding = strToLE (elektraKeyString (valid));
	int ret;
	ret = checkLineEndings (elektraKeyString (parentKey), validLineEnding, parentKey);
	if (ret == (-3))
	{
		return -1;
	}
	else
		return 1;
}

int elektraLineendingsSet (Plugin * handle, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey)
{
	ElektraKeyset * config = elektraPluginGetConfig (handle);
	ElektraKey * valid = elektraKeysetLookupByName (config, "/valid", 0);
	Lineending validLineEnding = strToLE (elektraKeyString (valid));
	int ret;
	ret = checkLineEndings (elektraKeyString (parentKey), validLineEnding, parentKey);
	switch (ret)
	{
	case (-1):
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Couldn't open file %s\n", elektraKeyString (parentKey));
		return 1;
		break;
	case (-2):
		return -1;
		break;
	case (-3):
		return -1;
		break;
	case 0:
	default:
		return 1;
		break;
	}
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("lineendings",
			ELEKTRA_PLUGIN_GET,	&elektraLineendingsGet,
			ELEKTRA_PLUGIN_SET,	&elektraLineendingsSet,
			//	ELEKTRA_PLUGIN_ERROR,	&elektraLineendingsError,
			ELEKTRA_PLUGIN_END);
}

