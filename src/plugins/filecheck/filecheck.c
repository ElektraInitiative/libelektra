/**
 * @file
 *
 * @brief Source for filecheck plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iconv.h>
#include <errno.h>
#include <kdbhelper.h>

#include "filecheck.h"


static inline char *LEString(Lineending index)
{
	static char *strings[] = {"NA", "CR", "LF", "CRLF", "LFCR"};	
	if(index > NUM_TYPES)
		return NULL;
	return strings[index];
}

static inline Lineending strToLE(const char *str)
{
	uint8_t counter = 0;
	for(; counter < NUM_TYPES; ++counter)
	{
		if(!strcmp(LEString(counter), str))
			return counter;
	}
	return NA;
}

int elektraFilecheckOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	KeySet *config = elektraPluginGetConfig(handle);
	checkStruct *checkConf = (checkStruct *)elektraMalloc(sizeof(checkStruct));
	checkConf->checkLineEnding = ksLookupByName(config, "/checkLE", 0) != NULL;
	checkConf->validLE = strToLE(keyString(ksLookupByName(config, "/validLE", 0)));
	checkConf->rejectNullByte = ksLookupByName(config, "/rejectNull", 0) != NULL;
	checkConf->checkEncoding = ksLookupByName(config, "/checkEncoding", 0) != NULL;
	checkConf->encoding = (char *)keyString(ksLookupByName(config, "/encoding", 0));
	checkConf->rejectBom = ksLookupByName(config, "/rejectBom", 0) != NULL;
	checkConf->rejectUnprintable = ksLookupByName(config, "rejectUnprintable", 0) != NULL;
	elektraPluginSetData(handle, checkConf);
	return 1; // success
}

int elektraFilecheckClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	checkStruct *checkConf = (checkStruct *)elektraPluginGetData(handle);
	if(checkConf)
		elektraFree(checkConf);
	return 1; // success
}

static int checkNull(const uint8_t * line, ssize_t bytes)
{
	int i;
	for(i = 0; i < bytes; ++i)
	{
		if(line[i] == 0)
		{
			return -1;
		}
	}
	return 0;
}

static int checkBom(const uint8_t *line)
{
	uint8_t i, j;
	uint8_t found = 0;
	for(i = 0; i < BOM_COUNT; ++i)
	{
		found = 1;
		for(j = 0; j < BOM_SIZE_MAX && BOMS[i][j] != INTERNAL_BOM_DELIMITER; ++j)
		{
			if(line[j] != BOMS[i][j])
			{
				found = 0;
			}
		}
		if(found)
			break;
	}
	if(found)
		return -1;
	else 
		return 0;
}

static int validateLineEnding(const uint8_t *line, Lineending *valid)
{
	static uint8_t lastByte = 0;
	Lineending found = NA;
	uint8_t fc;
	uint16_t i = 0;	
	if(lastByte != 0)
	{
		fc = lastByte;
	}
	else
	{
		fc = line[0];
		i = 1;
	}
	uint8_t sc;
	for(; i < elektraStrLen((char *)line); ++i)
	{
		sc = line[i];
//		fprintf(stderr, "fc: 0x%x, sc: 0x%x\n", fc, sc);
		switch(fc)
		{
			case LF_BYTE:
				if(sc == CR_BYTE)
					found = LFCR;
				else if(sc == LF_BYTE)
					found = LF;
				else
					found = LF;
				break;
			case CR_BYTE:
				if(sc == LF_BYTE)
					found = CRLF;
				else if(sc == CR_BYTE)
					found = CR;
				else
					found = CR;
				break;
		}
		fc = sc;
		lastByte = sc;
		if((found == CRLF || found == LFCR) && (i < (LINE_BYTES-1)))
		{
			fc = line[i+1];
			lastByte = fc;
			++i;
		}
		if(*valid != NA)
		{
			if(found != NA && found != *valid)
			{
//				fprintf(stderr, "found != *valid\n *valid = %d, found = %d\n", *valid, found);
				return -1;
			}
//			fprintf(stderr, "valid != NA\n *valid = %d, found = %d\n", *valid, found);
		}
		else
		{
			*valid = found;
//			fprintf(stderr, "valid == NA\n *valid = %d, found = %d\n", *valid, found);
		}
		found = NA;
	}
	return 0;
}

static int validateEncoding(const uint8_t *line, iconv_t conv, size_t bytesRead)
{
	char *ptr = (char *)line;
	char outBuffer[LINE_BYTES];
	char *outPtr = outBuffer;
	size_t inBytes = bytesRead;
	size_t outSize = sizeof(outBuffer);
	int ret = iconv(conv, &ptr, &inBytes, &outPtr, &outSize);
	if(ret == -1 && errno == EILSEQ)
	{
		return ((uint8_t *)ptr - line);
	}
	return 0;
}
static int checkUnprintable(const uint8_t *line)
{
	unsigned int i;
	for(i = 0; i < elektraStrLen((char *)line); ++i)
	{
		if(line[i] < 0x20 || line[i] > 0x7E || line[i] != '\n' || line[i] != '\r')
			return i;
	}
	return 0;
}
static long checkFile(const char *filename, checkStruct *checkConf)
{
	fprintf(stderr, "checkLineEnding: %d\t validLE: %d\t rejectNullByte: %d\t checkEncoding: %d:%s\t rejectBom: %d\t rejectUnprintable: %d\n", checkConf->checkLineEnding, checkConf->validLE, checkConf->rejectNullByte, checkConf->checkEncoding, checkConf->encoding, checkConf->rejectBom, checkConf->rejectUnprintable);
	FILE *fp = fopen(filename, "rb");
	if(fp == NULL)
	{
		return -1;
	}
	iconv_t conv = NULL;
	if(checkConf->checkEncoding)
	{
		if(checkConf->encoding != NULL)
		{
			conv = iconv_open(checkConf->encoding, checkConf->encoding);
		}
		else
		{
			conv = iconv_open("UTF-8", "UTF-8");
		}
		if(conv == (iconv_t)(-1))
		{
			fclose(fp);
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
	unsigned long counter = 0;
	int retVal = 0;
	while(!feof(fp))
	{
		size_t bytesRead = fread(line, 1, sizeof(line), fp);
		if(checkConf->checkLineEnding)
		{
			le_ret = validateLineEnding(line, &(checkConf->validLE));
			fprintf(stderr, "validateLineEnding returned %d\n", le_ret);
		}
		if(checkConf->rejectNullByte)
		{
			null_ret = checkNull(line, bytesRead);
			fprintf(stderr, "checkNull returned %d\n", null_ret);
		}
		if(checkConf->checkEncoding)
		{
			iconv_ret = validateEncoding(line, conv, bytesRead);
			fprintf(stderr, "validateEncoding returned %d\n", iconv_ret);
		}
		if(firstLine && checkConf->rejectBom)
		{
			bom_ret = checkBom(line);
			fprintf(stderr, "checkBom returned %d\n", bom_ret);
			firstLine = 0;
		}
		if(checkConf->rejectUnprintable)
		{
			unprintable_ret = checkUnprintable(line);
			fprintf(stderr, "checkUnprintable returned %d\n", unprintable_ret);

		}
		counter += bytesRead;
		retVal |= (iconv_ret | bom_ret | le_ret | null_ret);
	}
	return retVal;
}

int elektraFilecheckGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (keyName (parentKey), "system/elektra/modules/filecheck"))
	{
		KeySet * contract = ksNew (30,
				keyNew ("system/elektra/modules/filecheck",
					KEY_VALUE, "filecheck plugin waits for your orders", KEY_END),
				keyNew ("system/elektra/modules/filecheck/exports", KEY_END),
				keyNew ("system/elektra/modules/filecheck/exports/open",
					KEY_FUNC, elektraFilecheckOpen, KEY_END),
				keyNew ("system/elektra/modules/filecheck/exports/close",
					KEY_FUNC, elektraFilecheckClose, KEY_END),
				keyNew ("system/elektra/modules/filecheck/exports/get",
					KEY_FUNC, elektraFilecheckGet, KEY_END),
				keyNew ("system/elektra/modules/filecheck/exports/set",
					KEY_FUNC, elektraFilecheckSet, KEY_END),
				keyNew ("system/elektra/modules/filecheck/exports/error",
					KEY_FUNC, elektraFilecheckError, KEY_END),
#include ELEKTRA_README (filecheck)
				keyNew ("system/elektra/modules/filecheck/infos/version",
					KEY_VALUE, PLUGINVERSION, KEY_END),
				KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys
	checkStruct *checkConf = elektraPluginGetData(handle);
	const char *filename = keyString(parentKey);
	int ret = checkFile(filename, checkConf);

	return 1; // success
}

int elektraFilecheckSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// get all keys
	checkStruct *checkConf = elektraPluginGetData(handle);
	const char *filename = keyString(parentKey);
	int ret = checkFile(filename, checkConf);

	return ret; // success
}

int elektraFilecheckError (Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	// set all keys

	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT (filecheck)
{
	return elektraPluginExport ("filecheck",
			ELEKTRA_PLUGIN_OPEN,	&elektraFilecheckOpen,
			ELEKTRA_PLUGIN_CLOSE,	&elektraFilecheckClose,
			ELEKTRA_PLUGIN_GET,	&elektraFilecheckGet,
			ELEKTRA_PLUGIN_SET,	&elektraFilecheckSet,
			ELEKTRA_PLUGIN_ERROR,	&elektraFilecheckError,
			ELEKTRA_PLUGIN_END);
}

