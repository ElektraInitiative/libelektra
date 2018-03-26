/**
 * @file
 *
 * @brief Source for csvstorage plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include "csvstorage.h"
#include "kdbproposal.h"
#include <errno.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define PARSE 1
#define COLCOUNT 2
#define READLINE 3

static char * parseRecord (char ** ptr, char delim, int * isQuoted, int * isCol, int * hasUnescapedDQuote, unsigned long * counter,
			   unsigned short mode)
{
	if (**ptr == '"')
	{
		if (!(*isCol) && !(*isQuoted))
		{
			*isCol = 1;
			*isQuoted = 1;
		}
		else if (*isCol && *isQuoted)
		{
			if (*(*ptr + 1) == '"')
			{
				++(*ptr);
			}
			else if (*(*ptr + 1) == delim)
			{
				*isQuoted = 0;
				*isCol = 0;
				++(*ptr);
				++(*counter);
				if (mode == PARSE) return NULL;
			}
			else if (*(*ptr + 1) == '\n')
			{
				*isQuoted = 0;
				*isCol = 0;
				++(*counter);
			}
			else
			{
				*hasUnescapedDQuote = 1;
			}
		}
	}
	else if (**ptr == delim)
	{
		if (!(*isQuoted))
		{
			*isCol = 0;
			++(*counter);
			if (mode == PARSE) return NULL;
		}
	}
	else if (**ptr != '\n')
	{
		if (!(*isCol))
		{
			*isCol = 1;
		}
	}
	else // it's \n
	{
		if (mode == READLINE)
		{
			if (*isQuoted && *isCol)
			{
				return *ptr;
			}
			else
			{
				*isCol = 0;
				*isQuoted = 0;
			}
		}
		else
		{
			// last column is empty
			if (!(*isQuoted))
			{
				*isCol = 0;
				++(*counter);
				if (mode == PARSE) return NULL;
			}
		}
	}
	++(*ptr);
	return *ptr;
}

// ignore record and field separators in quoted fields according to RFC 4180
// @returns next field in record

static char * parseLine (char * origLine, char delim, unsigned long offset, Key * parentKey, unsigned long lineNr, int lastLine)
{
	char * line = (origLine + offset);

	if (*line == '\0') return NULL;

	char * ptr = line;
	int isQuoted = 0;
	int isCol = 0;
	int hasUnescapedDQuote = 0;
	while (*ptr)
	{
		char * ret = parseRecord (&ptr, delim, &isQuoted, &isCol, &hasUnescapedDQuote, &(unsigned long){ 0 }, PARSE);
		if (!ret) break;
	}
	if (!(*ptr))
	{
		if (!isQuoted && isCol)
		{
			isCol = 0;
			if (!lastLine)
			{
				ELEKTRA_ADD_WARNINGF (136, parentKey,
						      "Unexpected end of line(%lu), all records except the last must and with a newline",
						      lineNr);
			}
		}
	}
	unsigned long len = elektraStrLen (line);
	if (isQuoted)
	{
		if (line[len - 2] == '\n')
		{
			line[len - 2] = '\0';
		}
		ELEKTRA_ADD_WARNINGF (136, parentKey, "Unexpected end of line(%lu). unbalanced number of double-quotes in (%s)", lineNr,
				      line);
	}
	else if (isCol)
	{
		if (line[len - 2] == '\n')
		{
			line[len - 2] = '\0';
		}
		ELEKTRA_ADD_WARNINGF (136, parentKey, "Unexpected end of line(%lu): (%s)", lineNr, line);
	}
	else
	{
		*ptr = '\0';
	}
	if (hasUnescapedDQuote)
	{
		ELEKTRA_ADD_WARNINGF (136, parentKey, "Quoted field in line(%lu) has an unescaped double-quote: (%s)", lineNr, line);
	}

	return line;
}

static unsigned long getLineLength (FILE * fp)
{
	int startPos = ftell (fp);
	char c;
	while ((c = fgetc (fp)) && (!feof (fp)))
	{
		if (c == '\n') break;
	}
	int endPos = ftell (fp);
	fseek (fp, startPos, SEEK_SET);
	if ((endPos - startPos) == 0)
		return 0;
	else
		return (endPos - startPos) + 1;
}

// count columns in lineBuffer
// ignore record and field separators in quoted fields

static unsigned long getColumnCount (char * lineBuffer, char delim)
{
	char * ptr = lineBuffer;
	unsigned long counter = 0;
	int isQuoted = 0;
	int isCol = 0;
	while (*ptr)
	{
		parseRecord (&ptr, delim, &isQuoted, &isCol, &(int){ 0 }, &counter, COLCOUNT);
	}
	if (!(*ptr))
	{
		if (!isQuoted && isCol)
		{
			++counter;
		}
	}
	return counter;
}

// reads next record from file according to RFC 4180
// if EOL is reached with unbalanced quotes, assume record continues at the next
// line. append succeeding lines until quotes are balanced or EOF is reached

static char * readNextLine (FILE * fp, char delim, int * lastLine, int * linesRead)
{
	int done = 0;
	unsigned long bufLen = 0;
	unsigned long offset = 0;
	*linesRead = 0;
	char * lineBuffer = NULL;
	*linesRead = 0;
	int isQuoted = 0;
	int isCol = 0;
	while (!done)
	{

		unsigned long len = getLineLength (fp);
		if (!len)
		{
			if (!lineBuffer)
			{
				*lastLine = 0;
				return NULL;
			}
			else
				return lineBuffer;
		}
		else
		{
			++(*linesRead);
		}
		char buffer[len];
		fgets (buffer, len, fp);
		char * ptr = buffer;
		while (*ptr)
		{
			parseRecord (&ptr, delim, &isQuoted, &isCol, &(int){ 0 }, &(unsigned long){ 0 }, COLCOUNT);
		}
		len = elektraStrLen (buffer);
		bufLen += len;
		lineBuffer = realloc (lineBuffer, bufLen);

		memcpy (lineBuffer + offset, buffer, len);
		offset += (len - 1);
		if (!isCol && !isQuoted) done = 1;
	}
	return lineBuffer;
}


// @returns a newly allocated keyset with the column names
static KeySet * createHeaders (Key * parentKey, int columns, const char ** colNames)
{
	KeySet * header = ksNew (0, KS_END);
	int colCounter = 0;
	// if no headerline exists name the columns 0..N where N is the number of columns
	Key * orderKey = keyDup (parentKey);
	keyAddName (orderKey, "#");
	while (colCounter < columns)
	{
		if (elektraArrayIncName (orderKey) == -1)
		{
			keyDel (orderKey);
			ksDel (header);
			return NULL;
		}
		Key * key = keyDup (orderKey);
		if (colNames && (colNames + colCounter))
			keySetString (key, colNames[colCounter]);
		else
			keySetString (key, keyBaseName (key));
		ksAppendKey (header, key);
		++colCounter;
	}
	keyDel (orderKey);
	return header;
}

// @returns a newly allocated keyset with the column names
static KeySet * readHeaders (Key * parentKey, char * lineBuffer, char delim, int lineCounter, int lastLine, const char ** colNames)
{
	int colCounter = 0;
	unsigned long offset = 0;
	char * col;
	offset = 0;
	Key * orderKey = keyDup (parentKey);
	keyAddName (orderKey, "#");
	KeySet * header = ksNew (0, KS_END);
	while ((col = parseLine (lineBuffer, delim, offset, parentKey, lineCounter, lastLine)) != NULL)
	{
		offset += elektraStrLen (col);
		if (elektraArrayIncName (orderKey) == -1)
		{
			elektraFree (lineBuffer);
			keyDel (orderKey);
			ksDel (header);
			return NULL;
		}
		Key * key = keyDup (orderKey);
		if (colNames && (colNames + colCounter))
		{
			keySetString (key, colNames[colCounter]);
		}
		else
		{
			keySetString (key, col);
		}
		ksAppendKey (header, key);
		++colCounter;
	}
	keyDel (orderKey);
	return header;
}

static int csvRead (KeySet * returned, Key * parentKey, char delim, Key * colAsParent, short useHeader, unsigned long fixColumnCount,
		    const char ** colNames)
{
	const char * fileName;
	fileName = keyString (parentKey);
	FILE * fp = fopen (fileName, "rb");
	if (!fp)
	{
		ELEKTRA_SET_ERRORF (116, parentKey, "couldn't open file %s\n", fileName);
		return -1;
	}
	int lastLine = 0;
	int linesRead = 0;
	char * lineBuffer = readNextLine (fp, delim, &lastLine, &linesRead);
	if (!lineBuffer)
	{
		fclose (fp);
		return 0;
	}
	unsigned long columns = 0;
	columns = getColumnCount (lineBuffer, delim);
	if (fixColumnCount)
	{
		if (columns != fixColumnCount)
		{
			ELEKTRA_SET_ERROR (117, parentKey, "illegal number of columns in Header line");
			elektraFree (lineBuffer);
			fclose (fp);
			return -1;
		}
	}
	unsigned long colCounter = 0;
	unsigned long lineCounter = 0;

	int nr_keys = 1;
	KeySet * header;
	Key * key;
	if (useHeader == 1)
	{
		header = readHeaders (parentKey, lineBuffer, delim, lineCounter, lastLine, colNames);
		if (!header)
		{
			fclose (fp);
			return -1;
		}
		fseek (fp, 0, SEEK_SET);
		lineCounter += linesRead;
	}
	else
	{
		header = createHeaders (parentKey, columns, colNames);
		if (!header)
		{
			elektraFree (lineBuffer);
			fclose (fp);
			return -1;
		}
		if (useHeader == 0)
		{
			fseek (fp, 0, SEEK_SET);
		}
		lineCounter += 1;
	}
	Key * dirKey;
	Key * cur;
	dirKey = keyDup (parentKey);
	keyAddName (dirKey, "#");
	elektraFree (lineBuffer);
	ksRewind (header);
	while (1)
	{
		lineBuffer = readNextLine (fp, delim, &lastLine, &linesRead);
		if (!lineBuffer)
		{
			fclose (fp);
			keyDel (dirKey);
			ksDel (header);
			return (lineCounter > 0) ? 1 : 0;
		}

		if (elektraArrayIncName (dirKey) == -1)
		{
			elektraFree (lineBuffer);
			keyDel (dirKey);
			ksDel (header);
			fclose (fp);
			return -1;
		}
		++nr_keys;
		unsigned long offset = 0;
		char * col;
		colCounter = 0;
		char * lastIndex = "#0";
		ksRewind (header);
		KeySet * tmpKs = ksNew (0, KS_END);
		while ((col = parseLine (lineBuffer, delim, offset, parentKey, lineCounter, lastLine)) != NULL)
		{
			cur = ksNext (header);
			offset += elektraStrLen (col);
			key = keyDup (dirKey);
			if (col[0] == '"')
			{
				if (col[elektraStrLen (col) - 2] == '"')
				{
					keySetMeta (key, "internal/csvstorage/quoted", "");
					++col;
					col[elektraStrLen (col) - 2] = '\0';
				}
			}
			keyAddName (key, keyString (cur));
			keySetString (key, col);
			ksAppendKey (tmpKs, key);
			lastIndex = (char *)keyBaseName (cur);
			++nr_keys;
			++colCounter;
		}
		if (colAsParent)
		{
			if (!(lineCounter <= 1 && useHeader))
			{
				keySetString (dirKey, lastIndex);
				ksAppendKey (tmpKs, keyDup (dirKey));
				Key * lookupKey = keyNew (keyName (dirKey), KEY_END);
				keyAddName (lookupKey, keyString (colAsParent));
				Key * indexKey = ksLookupByName (tmpKs, keyName (lookupKey), 0);
				Key * renameKey = keyNew (keyName (dirKey), 0);
				keySetBaseName (renameKey, keyString (indexKey));
				ksRewind (tmpKs);
				KeySet * renamedKs = ksRenameKeys (tmpKs, renameKey);
				ksAppendKey (renamedKs, keyDup (renameKey));
				ksRewind (renamedKs);
				keyDel (lookupKey);
				keyDel (renameKey);
				ksRewind (renamedKs);
				ksRewind (tmpKs);
				ksAppend (returned, renamedKs);
				ksDel (renamedKs);
			}
		}
		else
		{
			keySetString (dirKey, lastIndex);
			ksAppend (returned, tmpKs);
			ksAppendKey (returned, keyDup (dirKey));
		}
		ksDel (tmpKs);
		tmpKs = NULL;
		if (colCounter != columns)
		{
			if (fixColumnCount)
			{
				ELEKTRA_SET_ERRORF (117, parentKey, "illegal number of columns in line %lu", lineCounter);
				elektraFree (lineBuffer);
				fclose (fp);
				keyDel (dirKey);
				ksDel (header);
				if (tmpKs) ksDel (tmpKs);
				return -1;
			}
			ELEKTRA_ADD_WARNINGF (118, parentKey, "illegal number of columns in line %lu", lineCounter);
		}
		lineCounter += linesRead;
		elektraFree (lineBuffer);
		ksDel (tmpKs);
	}
	key = keyDup (parentKey);
	keySetString (key, keyBaseName (dirKey));
	ksAppendKey (returned, key);
	keyDel (dirKey);
	fclose (fp);
	ksDel (header);
	return 1;
}

int elektraCsvstorageGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!strcmp (keyName (parentKey), "system/elektra/modules/csvstorage"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/csvstorage", KEY_VALUE, "csvstorage plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/csvstorage/exports", KEY_END),
			keyNew ("system/elektra/modules/csvstorage/exports/get", KEY_FUNC, elektraCsvstorageGet, KEY_END),
			keyNew ("system/elektra/modules/csvstorage/exports/set", KEY_FUNC, elektraCsvstorageSet, KEY_END),
#include ELEKTRA_README (csvstorage)
			keyNew ("system/elektra/modules/csvstorage/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}

	KeySet * config = elektraPluginGetConfig (handle);
	Key * delimKey = ksLookupByName (config, "/delimiter", 0);
	char delim = ',';
	if (delimKey)
	{
		const char * delimString = keyString (delimKey);
		delim = delimString[0];
	}

	Key * readHeaderKey = ksLookupByName (config, "/header", 0);
	short useHeader = 0;
	if (readHeaderKey)
	{
		const char * printHeaderString = keyString (readHeaderKey);
		if (!strcmp (printHeaderString, "colname"))
		{
			useHeader = 1;
		}
		else if (!(strcmp (printHeaderString, "skip")))
		{
			useHeader = -1;
		}
		else if (!(strcmp (printHeaderString, "record")))
		{
			useHeader = 0;
		}
		else
		{
			useHeader = 0;
		}
	}
	unsigned long fixColumnCount = 0;
	Key * fixColumnCountKey = ksLookupByName (config, "/columns", 0);
	if (fixColumnCountKey)
	{
		if (keyString (fixColumnCountKey))
		{
			fixColumnCount = atol (keyString (fixColumnCountKey));
		}
	}
	Key * colAsParent = ksLookupByName (config, "/columns/index", 0);
	Key * setNamesKey = ksLookupByName (config, "/columns/names", 0);
	char * colNames = NULL;
	if (setNamesKey)
	{
		if (fixColumnCountKey)
		{
			KeySet * namesKS = ksCut (config, setNamesKey);
			unsigned long nrNames = (unsigned long)ksGetSize (namesKS) - 1;
			if (nrNames == fixColumnCount)
			{
				colNames = (char *)elektraMalloc (nrNames * sizeof (char *));
				Key * cur;
				char ** ptr = (char **)colNames;
				while ((cur = ksNext (namesKS)) != NULL)
				{
					if (!strcmp (keyName (cur), keyName (setNamesKey))) continue;
					if (!strcmp (keyString (cur), ""))
						*ptr = NULL;
					else
						*ptr = (char *)keyString (cur);
					++ptr;
				}
			}
			ksAppend (config, namesKS);
			ksDel (namesKS);
		}
	}
	int nr_keys;
	nr_keys = csvRead (returned, parentKey, delim, colAsParent, useHeader, fixColumnCount, (const char **)colNames);
	if (colNames) elektraFree (colNames);
	if (nr_keys == -1) return -1;
	return 1;
}

static int csvWrite (KeySet * returned, Key * parentKey, Key * colAsParent, char delim, short useHeader)
{
	FILE * fp;
	fp = fopen (keyString (parentKey), "w");
	if (!fp)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		return -1;
	}

	keyDel (ksLookup (returned, parentKey, KDB_O_POP));

	unsigned long colCounter = 0;
	unsigned long columns = 0;
	unsigned long lineCounter = 0;
	Key * cur;
	KeySet * toWriteKS;
	Key * toWrite;

	ksRewind (returned);
	while ((cur = ksNext (returned)) != NULL)
	{
		if (keyRel (parentKey, cur) != 1) continue;
		colCounter = 0;
		if (useHeader)
		{
			useHeader = 0;
			continue;
		}
		if (colAsParent)
		{
			KeySet * tmpKs = ksDup (returned);
			ksRewind (tmpKs);
			KeySet * headerKs = ksCut (tmpKs, cur);
			ksRewind (headerKs);
			ksDel (tmpKs);
			ksNext (headerKs);
			Key * tmp = ksNext (headerKs);
			fprintf (fp, "%s", keyName (tmp) + strlen (keyName (cur)) + 1);
			++colCounter;
			while ((tmp = ksNext (headerKs)) != NULL)
			{
				++colCounter;
				fprintf (fp, ";%s", keyName (tmp) + strlen (keyName (cur)) + 1);
			}
			fprintf (fp, "\n");
			if (columns == 0)
			{
				columns = colCounter;
			}
			colAsParent = NULL;
			ksDel (headerKs);
		}
		colCounter = 0;
		toWriteKS = ksCut (returned, cur);
		ksRewind (toWriteKS);
		while (1)
		{
			toWrite = ksNext (toWriteKS);
			if (!keyCmp (cur, toWrite)) continue;
			if (!toWrite) break;
			if (colCounter) fprintf (fp, "%c", delim);
			++colCounter;
			if (keyGetMeta (toWrite, "internal/csvstorage/quoted"))
			{
				fprintf (fp, "\"%s\"", keyString (toWrite));
			}
			else
			{
				fprintf (fp, "%s", keyString (toWrite));
			}
		}
		ksDel (toWriteKS);
		fprintf (fp, "\n");
		if (columns == 0)
		{
			columns = colCounter;
		}
		if (colCounter != columns)
		{
			ELEKTRA_SET_ERRORF (117, parentKey, "illegal number of columns in line %lu\n", lineCounter);
			fclose (fp);
			return -1;
		}
		++lineCounter;
	}
	fclose (fp);
	return 1;
}

int elektraCsvstorageSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	KeySet * config = elektraPluginGetConfig (handle);
	Key * delimKey = ksLookupByName (config, "/delimiter", 0);
	char outputDelim;
	if (delimKey)
	{
		const char * delimString = keyString (delimKey);
		outputDelim = delimString[0];
	}
	else
	{
		outputDelim = ',';
	}
	Key * colAsParent = ksLookupByName (config, "/columns/index", 0);
	Key * useHeaderKey = ksLookupByName (config, "/header", 0);
	short useHeader = 0;
	if (!strcmp (keyString (useHeaderKey), "skip")) useHeader = -1;
	if (csvWrite (returned, parentKey, colAsParent, outputDelim, useHeader) == -1)
	{
		return -1;
	}
	else
	{
		return 1; /* success */
	}
}

Plugin * ELEKTRA_PLUGIN_EXPORT (csvstorage)
{
	// clang-format off
    return elektraPluginExport("csvstorage",
            ELEKTRA_PLUGIN_GET,	&elektraCsvstorageGet,
            ELEKTRA_PLUGIN_SET,	&elektraCsvstorageSet,
            ELEKTRA_PLUGIN_END);
}

