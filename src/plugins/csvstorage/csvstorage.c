/**
 * @file
 *
 * @brief Source for csvstorage plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbproposal.h>
#include "csvstorage.h"

#define INTSTR_MAX 15
static char *parseLine(char *origLine, char delim, unsigned long offset)
{
	char *line = (origLine + offset);
	if(*line == '\0')
		return NULL;
	char *ptr = strchr(line, delim);

	if(ptr == NULL)
	{
		line[elektraStrLen(line)-2]='\0';
	}
	else
	{
		*ptr = '\0';
	}
	return line;
}

static unsigned long getLineLength(FILE *fp)
{
	int startPos = ftell(fp);
	char c;
	while((c = fgetc(fp)) && (!feof(fp)))
	{
		if(c == '\n')
			break;
	}
	int  endPos = ftell(fp);
	fseek(fp, startPos, SEEK_SET);
	if((endPos - startPos) == 0)
		return 0;
	else
		return (endPos - startPos)+1;
}

static unsigned long getColumnCount(char *lineBuffer, char delim)
{
	char *ptr = lineBuffer;
	unsigned long counter = 0;
	while(*ptr != '\0')
	{
		if(*ptr == delim)
			++counter;
		++ptr;
	}
	++counter;
	return counter;
}

static char *itostr(char *buf, unsigned long i, uint8_t len)
{
	snprintf(buf, len, "%lu", i);
	return buf;
}

static Key *getKeyByOrderNr(KeySet *ks, unsigned long n)
{
	Key *cur;
	char buf[INTSTR_MAX];
	ksRewind(ks);
	while((cur = ksNext(ks)) != NULL)
	{
		if(!strcmp(keyString(keyGetMeta(cur, "csv/order")), itostr(buf, n, sizeof(buf)-1)))
			return cur; 
	}
	return NULL;
}

static int csvRead(KeySet *returned, Key *parentKey, char delim, short useHeader)
{
	const char *fileName;
	fileName = keyString(parentKey);
	FILE *fp = NULL;
	fp = fopen(fileName, "rb");
	if(!fp)
	{
		ELEKTRA_SET_ERRORF(116, parentKey, "couldn't open file %s\n", fileName);
		return -1;
	}

	unsigned long length = 0;
	length = getLineLength(fp);
	if(length == 0)
	{
		ELEKTRA_ADD_WARNING(118, parentKey, "Empty file");
		fclose(fp);
		return -2;
	}

	char *lineBuffer;
	lineBuffer = elektraMalloc((length * sizeof(char))+1);
	if(!lineBuffer)
	{
		ELEKTRA_SET_ERROR(87, parentKey, "Out of memory");
		return -1;
	}
	if(!fgets(lineBuffer, length, fp))
	{
		ELEKTRA_SET_ERROR(116, parentKey, "Cant read File");
		return -1;
	}
	
	unsigned long columns = 0;
	columns = getColumnCount(lineBuffer, delim);

	unsigned long colCounter = 0;
	unsigned long lineCounter = 0;
	unsigned long offset = 0;
	char *col;
	char buf[INTSTR_MAX];
	int nr_keys = 1;
	KeySet *header = ksNew(0, KS_END);
	Key *key;
	
	if(useHeader == 1)
	{
		colCounter = 0;
		offset = 0;
		while((col = parseLine(lineBuffer, delim, offset)) != NULL)
		{
			offset += elektraStrLen(col);
			key = keyDup(parentKey);
			keyAddBaseName(key, col);
			keySetMeta(key, "csv/order", itostr(buf, colCounter, sizeof(buf)-1));
			ksAppendKey(header, key);
			++colCounter;
		}
	}
	else
	{
		colCounter = 0;
		//if no headerline exists name the columns 0..N where N is the number of columns
		key = keyDup(parentKey);
		keyAddName(key, "#");
		while(colCounter < columns)
		{
			if(elektraArrayIncName(key) == -1)
			{
				elektraFree(lineBuffer);
				keyDel(key);
				ksDel(header);
				fclose(fp);
				return -1;
			}
			keySetMeta(key, "csv/order", itostr(buf, colCounter, sizeof(buf)-1));
			ksAppendKey(header, keyDup(key));
			++colCounter;
		}
		keyDel(key);
		fseek(fp, 0, SEEK_SET);
	}
	Key *dirKey;
	Key *cur;
	dirKey = keyDup(parentKey);
	keyAddName(dirKey, "#");
	while(!feof(fp))
	{
		length = getLineLength(fp);
		if(length == 0)
			break;
		if(elektraRealloc((void **)&lineBuffer, (length * sizeof(char))+1) < 0)
		{
			fclose(fp);
			elektraFree(lineBuffer);
			ksDel(header);
			keyDel(dirKey);
			ELEKTRA_SET_ERROR(87, parentKey, "Out of memory");
			return -1;
		}
		fgets(lineBuffer, length, fp);
		if(elektraArrayIncName(dirKey) == -1)
		{
			elektraFree(lineBuffer);
			keyDel(dirKey);
			ksDel(header);
			fclose(fp);
			return -1;
		}
		++nr_keys;
		offset = 0;
		colCounter = 0;
		char *lastIndex = "#0";
		while((col = parseLine(lineBuffer, delim, offset)) != NULL)
		{
			cur = getKeyByOrderNr(header, colCounter);
			offset += elektraStrLen(col);
			key = keyDup(dirKey);
			keyAddBaseName(key, keyBaseName(cur));
			keySetString(key, col);
			keySetMeta(key, "csv/order", itostr(buf, colCounter, sizeof(buf)-1));
			ksAppendKey(returned, key);
			lastIndex = (char *)keyString(key);
			++nr_keys;
			++colCounter;
		}
		keySetString(dirKey, lastIndex);
		ksAppendKey(returned, keyDup(dirKey));
		if(colCounter != columns)
		{
			ELEKTRA_ADD_WARNING(118, parentKey, "illegal number of columns");
		}
		++lineCounter;
	}
	key = keyDup(parentKey);
	keySetString(key, keyBaseName(dirKey));
	ksAppendKey(returned, key);
	keyDel(dirKey);
	fclose(fp);
	elektraFree(lineBuffer);
	ksDel(header);
	return nr_keys;
}

int elektraCsvstorageGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	if (!strcmp(keyName(parentKey), "system/elektra/modules/csvstorage"))
	{
		KeySet *contract = ksNew (30,
				keyNew ("system/elektra/modules/csvstorage",
					KEY_VALUE, "csvstorage plugin waits for your orders", KEY_END),
				keyNew ("system/elektra/modules/csvstorage/exports", KEY_END),
				keyNew ("system/elektra/modules/csvstorage/exports/get",
					KEY_FUNC, elektraCsvstorageGet, KEY_END),
				keyNew ("system/elektra/modules/csvstorage/exports/set",
					KEY_FUNC, elektraCsvstorageSet, KEY_END),
#include ELEKTRA_README(csvstorage)
				keyNew ("system/elektra/modules/csvstorage/infos/version",
					KEY_VALUE, PLUGINVERSION, KEY_END),
				KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}

	KeySet *config = elektraPluginGetConfig(handle);
	Key *delimKey = ksLookupByName(config, "/delimiter", 0);
	char delim = ';';
	if(delimKey)
	{
		const char *delimString = keyString(delimKey);
		delim = delimString[0];
	}
	
	Key *readHeaderKey = ksLookupByName(config, "/useheader", 0);
	short useHeader = 0;
	if(readHeaderKey)
	{
		const char *printHeaderString = keyString(readHeaderKey);
		if((printHeaderString[0] - '0') == 1)
		{
			useHeader = 1;
		}
	}
	int nr_keys;
	nr_keys = csvRead(returned, parentKey, delim, useHeader);
	
	switch(nr_keys)
	{
		case (-2):
			return 1;
			break;
		case (-1):
			return -1;
			break;
		default:
			return nr_keys;
			break;
	}
}

static int csvWrite(KeySet *returned, Key *parentKey, char delim, short printHeader)
{
	FILE *fp;
	fp = fopen(keyString(parentKey), "w");
	if(!fp)
	{
		ELEKTRA_SET_ERROR_SET(parentKey);
		return -1;
	}

	keyDel(ksLookup(returned, parentKey, KDB_O_POP));
	
	unsigned long colCounter = 0;
	unsigned long columns = 0;
	Key *cur;
	KeySet *toWriteKS;
	Key *toWrite;
	while((cur = ksNext(returned)) != NULL)
	{
		if(keyRel(parentKey, cur) != 1)
			continue;
		toWriteKS = ksCut(returned, cur);
		ksRewind(toWriteKS);
		colCounter = 0;
		if(printHeader)
		{
			while(1)
			{
				if(colCounter == ULONG_MAX)
				{
					ELEKTRA_SET_ERROR(117, parentKey, "number of columns exceeds ULONG_MAX");
					fclose(fp);
					return -1;
				}
				toWrite = getKeyByOrderNr(toWriteKS, colCounter);
				if(!toWrite)
					break;
				if(colCounter)
					fprintf(fp, "%c", delim);
				++colCounter;
				fprintf(fp, "%s", keyBaseName(toWrite));
			}
			fprintf(fp, "\n");
			if(!colCounter)
			{
				ELEKTRA_SET_ERROR(117, parentKey, "no columns");
				fclose(fp);
				return -1;
			}
			columns = colCounter;
			colCounter = 0;
			printHeader = 0;
		}
		else
		{
			while(1)
			{
				toWrite = getKeyByOrderNr(toWriteKS, colCounter);
				if(!toWrite)
					break;
				if(colCounter)
					fprintf(fp, "%c", delim);
				++colCounter;
				fprintf(fp, "%s", keyString(toWrite));

			}
			ksDel(toWriteKS);
			fprintf(fp, "\n");
		}
		if(columns == 0)
			columns = colCounter;
		if(colCounter != columns)
		{    
			ELEKTRA_SET_ERROR(117, parentKey, "illegal number of columns\n");
			fclose(fp);
			return -1;
		}
	}
	fclose(fp);
	return 1;
}

int elektraCsvstorageSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	KeySet *config = elektraPluginGetConfig(handle);
	Key *delimKey = ksLookupByName(config, "/delimiter", 0);
	char outputDelim;
	if(delimKey)
	{
		const char *delimString = keyString(delimKey);
		outputDelim = delimString[0];
	}
	else
	{
		outputDelim = ';';
	}
	
	Key *printHeaderKey = ksLookupByName(config, "/useheader", 0);
	short printHeader = 0;
	if(printHeaderKey)
	{
		const char *printHeaderString = keyString(printHeaderKey);
		if((printHeaderString[0] - '0') == 1)
			printHeader = 1;
		else
			printHeader = 0;
	}
	else
	{
		printHeader = 0;
	}

	if(csvWrite(returned, parentKey, outputDelim, printHeader) == -1)
	{
		return -1;
	}
	else
	{
		return 1; /* success */
	}
}

Plugin *ELEKTRA_PLUGIN_EXPORT(csvstorage)
{
	return elektraPluginExport("csvstorage",
			ELEKTRA_PLUGIN_GET,	&elektraCsvstorageGet,
			ELEKTRA_PLUGIN_SET,	&elektraCsvstorageSet,
			ELEKTRA_PLUGIN_END);
}

