/**
 * @file
 *
 * @brief Source for lineendings plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
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

typedef enum { NA, CR, LF, CRLF, LFCR, NUM_TYPES } Lineending;

static inline char * LEString (Lineending index)
{
	static char * strings[] = { "NA", "CR", "LF", "CRLF", "LFCR" };
	if (index > NUM_TYPES)
		return NULL;
	return strings[index];
}
static Lineending strToLE (const char * str)
{
	uint8_t counter = 0;
	for (; counter < NUM_TYPES; ++counter)
	{
		if (!strcmp (LEString (counter), str))
			return counter;
	}
	return NA;
}
static int checkLineEndings (const char * fileName, Lineending validLineEnding, Key * parentKey)
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
	fread (&fc, 1, 1, fp);
	while (!feof (fp))
	{
		fread (&sc, 1, 1, fp);
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
			fread (&sc, 1, 1, fp);
		}
		if (lineEnding == NA && found != NA)
		{
			lineEnding = found;
			if (validLineEnding != NA && lineEnding != validLineEnding)
			{
				fclose (fp);
				ELEKTRA_SET_ERRORF (114, parentKey, "Invalid line ending at line %lu", line);
				return -2;
			}
			++line;
			found = NA;
		}
		else if (lineEnding != found && found != NA)
		{
			fclose (fp);
			ELEKTRA_SET_ERRORF (115, parentKey, "inconsistent line endings at line %lu", line);
			return -3;
		}
		fc = sc;
		found = NA;
	}
	fclose (fp);
	return 0;
}

int elektraLineendingsGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (keyName (parentKey), "system/elektra/modules/lineendings"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/lineendings", KEY_VALUE, "lineendings plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/lineendings/exports", KEY_END),
			keyNew ("system/elektra/modules/lineendings/exports/get", KEY_FUNC, elektraLineendingsGet, KEY_END),
			keyNew ("system/elektra/modules/lineendings/exports/set", KEY_FUNC, elektraLineendingsSet, KEY_END),
#include ELEKTRA_README (lineendings)
			keyNew ("system/elektra/modules/lineendings/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}
	/* get all keys */
	KeySet * config = elektraPluginGetConfig (handle);
	Key * valid = ksLookupByName (config, "/valid", 0);
	Lineending validLineEnding = strToLE (keyString (valid));
	int ret;
	ret = checkLineEndings (keyString (parentKey), validLineEnding, parentKey);
	if (ret == (-3))
	{
		return -1;
	}
	else
		return 1;
}

int elektraLineendingsSet (Plugin * handle, KeySet * returned ELEKTRA_UNUSED, Key * parentKey)
{
	KeySet * config = elektraPluginGetConfig (handle);
	Key * valid = ksLookupByName (config, "/valid", 0);
	Lineending validLineEnding = strToLE (keyString (valid));
	int ret;
	ret = checkLineEndings (keyString (parentKey), validLineEnding, parentKey);
	switch (ret)
	{
	case (-1):
		ELEKTRA_SET_ERRORF (113, parentKey, "Couldn't open file %s\n", keyString (parentKey));
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

Plugin * ELEKTRA_PLUGIN_EXPORT (lineendings)
{
	// clang-format off
	return elektraPluginExport("lineendings",
			ELEKTRA_PLUGIN_GET,	&elektraLineendingsGet,
			ELEKTRA_PLUGIN_SET,	&elektraLineendingsSet,
			//	ELEKTRA_PLUGIN_ERROR,	&elektraLineendingsError,
			ELEKTRA_PLUGIN_END);
}

