/**
 * @file
 *
 * @brief Source for filecheck plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef HAVE_KDBCONFIG
#include <internal/config.h>
#endif

#include "./filecheck.h"
#include <elektra/core/errors.h>
#include <errno.h>
#include <iconv.h>
#include <internal/macros/attributes.h>
#include <internal/utility/alloc.h>
#include <internal/utility/string.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


static inline char * LEString (Lineending index)
{
	static char * strings[] = { "NA", "CR", "LF", "CRLF", "LFCR" };
	if (index > NUM_TYPES) return NULL;
	return strings[index];
}

static inline Lineending strToLE (const char * str)
{
	uint8_t counter = 0;
	for (; counter < NUM_TYPES; ++counter)
	{
		if (!strcmp (LEString (counter), str)) return counter;
	}
	return NA;
}

int elektraFilecheckOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	KeySet * config = elektraPluginGetConfig (handle);
	checkStruct * checkConf = (checkStruct *) elektraMalloc (sizeof (checkStruct));
	checkConf->checkLineEnding = ksLookupByName (config, "/check/lineending", 0) != NULL;
	checkConf->validLE = strToLE (keyString (ksLookupByName (config, "/valid/lineending", 0)));
	checkConf->rejectNullByte = ksLookupByName (config, "/reject/null", 0) != NULL;
	checkConf->checkEncoding = ksLookupByName (config, "/check/encoding", 0) != NULL;
	checkConf->encoding = (char *) keyString (ksLookupByName (config, "/valid/encoding", 0));
	checkConf->rejectBom = ksLookupByName (config, "/reject/bom", 0) != NULL;
	checkConf->rejectUnprintable = ksLookupByName (config, "reject/unprintable", 0) != NULL;
	elektraPluginSetData (handle, checkConf);
	return 1; // success
}

int elektraFilecheckClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	checkStruct * checkConf = (checkStruct *) elektraPluginGetData (handle);
	if (checkConf) elektraFree (checkConf);
	return 1; // success
}

static int checkNull (const uint8_t * line, ssize_t bytes)
{
	int i;
	for (i = 0; i < bytes; ++i)
	{
		if (line[i] == 0)
		{
			return i;
		}
	}
	return 0;
}

static int checkBom (const uint8_t * line)
{
	uint8_t i, j;
	uint8_t found = 0;
	for (i = 0; i < BOM_COUNT; ++i)
	{
		found = 1;
		for (j = 0; j < BOM_SIZE_MAX && BOMS[i][j] != INTERNAL_BOM_DELIMITER; ++j)
		{
			if (line[j] != BOMS[i][j])
			{
				found = 0;
			}
		}
		if (found) break;
	}
	if (found)
		return -1;
	else
		return 0;
}

static int validateLineEnding (const uint8_t * line, Lineending * valid, int reset)
{
	static uint8_t lastByte = 0;
	if (reset || !line)
	{
		lastByte = 0;
		return 0;
	}
	Lineending found = NA;
	uint8_t fc;
	uint16_t i = 0;
	if (lastByte != 0)
	{
		fc = lastByte;
	}
	else
	{
		fc = line[0];
		i = 1;
	}
	const ssize_t lineLength = (elektraStrLen ((char *) line) - 1);
	for (; i < lineLength; ++i)
	{
		found = NA;
		uint8_t sc = line[i];
		switch (fc)
		{
		case LF_BYTE:
			if (sc == CR_BYTE)
				found = LFCR;
			else if (sc == LF_BYTE)
				found = LF;
			else
				found = LF;
			break;
		case CR_BYTE:
			if (sc == LF_BYTE)
				found = CRLF;
			else if (sc == CR_BYTE)
				found = CR;
			else
				found = CR;
			break;
		}
		fc = sc;
		lastByte = sc;
		if ((found == CRLF || found == LFCR) && (i < (lineLength - 2)))
		{
			fc = line[i + 1];
			lastByte = fc;
			++i;
		}
		if (*valid != NA)
		{
			if (found != NA && found != *valid)
			{
				return i;
			}
		}
		else
		{
			*valid = found;
		}
	}

	// because we work an pairs of 2 bytes we need handle the last byte explicitly
	if (found == NA)
	{
		if (fc == CR_BYTE && *valid != CR)
		{
			return i;
		}
		if (fc == LF_BYTE && *valid != LF)
		{
			return i;
		}
	}
	return 0;
}

static int validateEncoding (const uint8_t * line, iconv_t conv, size_t bytesRead)
{
	char * ptr = (char *) line;
	char outBuffer[LINE_BYTES];
	char * outPtr = outBuffer;
	size_t inBytes = bytesRead;
	size_t outSize = sizeof (outBuffer);
	size_t ret = iconv (conv, &ptr, &inBytes, &outPtr, &outSize);
	if (ret == (size_t) -1 && errno == EILSEQ)
	{
		return ((uint8_t *) ptr - line);
	}
	return 0;
}
static int checkUnprintable (const uint8_t * line)
{
	unsigned int i;
	for (i = 0; i < elektraStrLen ((char *) line); ++i)
	{ // \n is > 0x7E too
		if (line[i] < 0x20 || line[i] > 0x7E || line[i] == '\r') return i;
	}
	return 0;
}
static long checkFile (Key * parentKey, const char * filename, checkStruct * checkConf)
{
	FILE * fp = fopen (filename, "rb");
	if (fp == NULL)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Couldn't open file %s. Reason: %s", filename, strerror (errno));
		return -1;
	}
	iconv_t conv = NULL;
	if (checkConf->checkEncoding)
	{
		if (checkConf->encoding != NULL)
		{
			conv = iconv_open (checkConf->encoding, checkConf->encoding);
		}
		else
		{
			conv = iconv_open ("UTF-8", "UTF-8");
		}
		if (conv == (iconv_t) (-1))
		{
			ELEKTRA_SET_INSTALLATION_ERRORF (parentKey, "Couldn't initialize iconv with encoding %s\n", checkConf->encoding);
			fclose (fp);
			return -2;
		}
	}
	uint8_t line[LINE_BYTES];
	int iconv_ret = 0;
	int bom_ret = 0;
	int le_ret = 0;
	int null_ret = 0;
	int unprintable_ret = 0;
	uint8_t firstLine = 1;
	int retVal = 0;
	if (checkConf->checkLineEnding) validateLineEnding (NULL, NULL, 1);
	while (!feof (fp))
	{
		memset (line, 0, sizeof (line));
		size_t bytesRead = fread (line, 1, sizeof (line), fp);
		if (checkConf->checkLineEnding)
		{
			le_ret = validateLineEnding (line, &(checkConf->validLE), 0);
			if (le_ret)
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Invalid lineending at position %zd in file %s",
									 bytesRead + le_ret, filename);
				retVal = -1;
				break;
			}
		}
		if (checkConf->rejectNullByte)
		{
			null_ret = checkNull (line, bytesRead);
			if (null_ret)
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Found null-byte at position %zd in file %s",
									 bytesRead + null_ret, filename);
				retVal = -1;
				break;
			}
		}
		if (checkConf->checkEncoding)
		{
			iconv_ret = validateEncoding (line, conv, bytesRead);
			if (iconv_ret)
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Invalid encoding at position %zd in file %s",
									 bytesRead + iconv_ret, filename);
				retVal = -1;
				break;
			}
		}
		if (firstLine && checkConf->rejectBom)
		{
			bom_ret = checkBom (line);
			if (bom_ret)
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Found no Byte Order Mark (BOM) in file %s", filename);
				retVal = -1;
				break;
			}
			firstLine = 0;
		}
		if (checkConf->rejectUnprintable)
		{
			unprintable_ret = checkUnprintable (line);
			if (unprintable_ret)
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Unprintable character at position %zd in file %s",
									 bytesRead + unprintable_ret, filename);
				retVal = -1;
				break;
			}
		}
	}
	if (checkConf->checkEncoding)
	{
		iconv_close (conv);
	}
	fclose (fp);
	return retVal;
}

int elektraFilecheckGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/filecheck"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system:/elektra/modules/filecheck", KEY_VALUE, "filecheck plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/filecheck/exports", KEY_END),
			keyNew ("system:/elektra/modules/filecheck/exports/open", KEY_FUNC, elektraFilecheckOpen, KEY_END),
			keyNew ("system:/elektra/modules/filecheck/exports/close", KEY_FUNC, elektraFilecheckClose, KEY_END),
			keyNew ("system:/elektra/modules/filecheck/exports/get", KEY_FUNC, elektraFilecheckGet, KEY_END),
			keyNew ("system:/elektra/modules/filecheck/exports/commit", KEY_FUNC, elektraFilecheckCommit, KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/filecheck/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys
	checkStruct * checkConf = elektraPluginGetData (handle);
	const char * filename = keyString (parentKey);
	int ret = checkFile (parentKey, filename, checkConf);
	if (ret != 0) return -1;
	return 1; // success
}

int elektraFilecheckCommit (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// get all keys
	checkStruct * checkConf = elektraPluginGetData (handle);
	const char * filename = keyString (parentKey);
	int ret = checkFile (parentKey, filename, checkConf);
	if (ret != 0) return -1;
	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("filecheck",
			ELEKTRA_PLUGIN_OPEN,	&elektraFilecheckOpen,
			ELEKTRA_PLUGIN_CLOSE,	&elektraFilecheckClose,
			ELEKTRA_PLUGIN_GET,	&elektraFilecheckGet,
			ELEKTRA_PLUGIN_COMMIT,	&elektraFilecheckCommit,
			ELEKTRA_PLUGIN_END);
}

