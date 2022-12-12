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
#include <elektra/kdbease.h>
#include <elektra/kdbhelper.h>
#include <elektra/kdbprivate.h> // for ksRenameKeys
#include <errno.h>
#include <kdbassert.h>
#include <kdberrors.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define PARSE 1
#define COLCOUNT 2
#define READLINE 3

static char * parseRecord (char ** ptr, char delim, int * isQuoted, int * isCol, int * hasUnescapedDQuote, unsigned long * counter,
			   unsigned short mode)
{
	ELEKTRA_NOT_NULL (ptr);
	ELEKTRA_NOT_NULL (*ptr);
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
			else if (*(*ptr + 1) == '\n' || *(*ptr + 1) == '\r')
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
	else if (**ptr != '\n' && **ptr != '\r')
	{
		if (!(*isCol))
		{
			*isCol = 1;
		}
	}
	else // it's \n or \r
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
				ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF (
					parentKey, "Unexpected end of line(%lu), all records except the last must and with a newline",
					lineNr);
			}
		}
	}
	unsigned long len = elektraStrLen (line);
	if (isQuoted)
	{
		if (line[len - 2] == '\n' || line[len - 2] == '\r')
		{
			line[len - 2] = '\0';
		}
		ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF (
			parentKey, "Unexpected end of line(%lu). unbalanced number of double-quotes in (%s)", lineNr, line);
	}
	else if (isCol)
	{
		if (line[len - 2] == '\n' || line[len - 2] == '\r')
		{
			line[len - 2] = '\0';
		}
		ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF (parentKey, "Unexpected end of line(%lu): (%s)", lineNr, line);
	}
	else
	{
		*ptr = '\0';
	}
	if (hasUnescapedDQuote)
	{
		ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF (parentKey, "Quoted field in line(%lu) has an unescaped double-quote: (%s)",
							   lineNr, line);
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


/// @returns a newly allocated keyset with the column names
static KeySet * createHeaders (Key * parentKey, int columns, const char ** colNames)
{
	KeySet * header = ksNew (0, KS_END);
	int colCounter = 0;
	// if no headerline exists name the columns 0..N where N is the number of columns
	Key * orderKey = keyDup (parentKey, KEY_CP_ALL);
	keyAddName (orderKey, "#");
	while (colCounter < columns)
	{
		if (elektraArrayIncName (orderKey) == -1)
		{
			keyDel (orderKey);
			ksDel (header);
			return NULL;
		}
		Key * key = keyDup (orderKey, KEY_CP_ALL);
		if (colNames)
			keySetString (key, colNames[colCounter]);
		else
			keySetString (key, keyBaseName (key));
		ksAppendKey (header, key);
		++colCounter;
	}
	keyDel (orderKey);
	return header;
}

/// @returns a newly allocated keyset with the column names
static KeySet * readHeaders (Key * parentKey, char * lineBuffer, char delim, int lineCounter, int lastLine, const char ** colNames)
{
	int colCounter = 0;
	unsigned long offset = 0;
	char * col;
	offset = 0;
	Key * orderKey = keyDup (parentKey, KEY_CP_ALL);
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
		Key * key = keyDup (orderKey, KEY_CP_ALL);
		if (colNames)
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
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Couldn't open file %s", fileName);
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
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Illegal number of columns (%lu - %lu) in Header line: %s",
								 columns, fixColumnCount, lineBuffer);
			elektraFree (lineBuffer);
			fclose (fp);
			return -1;
		}
	}
	unsigned long colCounter = 0;
	unsigned long lineCounter = 0;

	// TODO: refactoring needed here
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
	dirKey = keyDup (parentKey, KEY_CP_ALL);
	keyAddName (dirKey, "#");
	elektraFree (lineBuffer);

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
		unsigned long offset = 0;
		char * col;
		colCounter = 0;
		char * lastIndex = "#0";
		KeySet * tmpKs = ksNew (0, KS_END);

		elektraCursor itHeader = 0;
		while ((col = parseLine (lineBuffer, delim, offset, parentKey, lineCounter, lastLine)) != NULL)
		{
			cur = ksAtCursor (header, itHeader++);
			offset += elektraStrLen (col);
			key = keyDup (dirKey, KEY_CP_ALL);
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
			lastIndex = (char *) keyBaseName (cur);
			++colCounter;
		}
		if (colAsParent)
		{
			if (!(lineCounter <= 1 && useHeader))
			{
				keySetString (dirKey, lastIndex);
				keySetMeta (dirKey, "array", lastIndex);
				ksAppendKey (tmpKs, keyDup (dirKey, KEY_CP_ALL));
				Key * lookupKey = keyNew (keyName (dirKey), KEY_END);
				keyAddName (lookupKey, keyString (colAsParent));
				Key * indexKey = ksLookupByName (tmpKs, keyName (lookupKey), 0);
				Key * renameKey = keyNew (keyName (dirKey), KEY_END);
				keySetBaseName (renameKey, keyString (indexKey));
				KeySet * renamedKs = ksRenameKeys (tmpKs, keyName (renameKey));
				ksAppendKey (renamedKs, keyDup (renameKey, KEY_CP_ALL));
				keyDel (lookupKey);
				keyDel (renameKey);
				ksAppend (returned, renamedKs);
				ksDel (renamedKs);
			}
		}
		else
		{
			keySetString (dirKey, lastIndex);
			keySetMeta (dirKey, "array", lastIndex);
			ksAppend (returned, tmpKs);
			ksAppendKey (returned, keyDup (dirKey, KEY_CP_ALL));
		}
		ksDel (tmpKs);
		tmpKs = NULL;
		if (colCounter != columns)
		{
			if (fixColumnCount)
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Illegal number of columns (%lu - %lu) in line %lu: %s",
									 colCounter, columns, lineCounter, lineBuffer);
				elektraFree (lineBuffer);
				fclose (fp);
				keyDel (dirKey);
				ksDel (header);
				return -1;
			}
			ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF (parentKey, "Illegal number of columns (%lu - %lu)  in line %lu: %s",
								   colCounter, columns, lineCounter, lineBuffer);
		}
		lineCounter += linesRead;
		elektraFree (lineBuffer);
		ksDel (tmpKs);
	}
	key = keyDup (parentKey, KEY_CP_ALL);
	keySetString (key, keyBaseName (dirKey));
	ksAppendKey (returned, key);
	keyDel (dirKey);
	fclose (fp);
	ksDel (header);
	return 1;
}

int elektraCsvstorageGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/csvstorage"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system:/elektra/modules/csvstorage", KEY_VALUE, "csvstorage plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/csvstorage/exports", KEY_END),
			keyNew ("system:/elektra/modules/csvstorage/exports/get", KEY_FUNC, elektraCsvstorageGet, KEY_END),
			keyNew ("system:/elektra/modules/csvstorage/exports/set", KEY_FUNC, elektraCsvstorageSet, KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/csvstorage/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
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
			unsigned long nrNames = (unsigned long) ksGetSize (namesKS) - 1;
			if (nrNames == fixColumnCount)
			{
				colNames = (char *) elektraMalloc (nrNames * sizeof (char *));
				Key * cur;
				char ** ptr = (char **) colNames;

				for (elektraCursor it = 0; it < ksGetSize (namesKS); ++it)
				{
					cur = ksAtCursor (namesKS, it);
					if (!strcmp (keyName (cur), keyName (setNamesKey))) continue;
					if (!strcmp (keyString (cur), ""))
						*ptr = NULL;
					else
						*ptr = (char *) keyString (cur);
					++ptr;
				}
			}
			ksAppend (config, namesKS);
			ksDel (namesKS);
		}
	}
	int nr_keys;
	nr_keys = csvRead (returned, parentKey, delim, colAsParent, useHeader, fixColumnCount, (const char **) colNames);
	if (colNames) elektraFree (colNames);
	if (nr_keys == -1) return -1;
	return 1;
}

static int isExportKey (const Key * key, const Key * parent, KeySet * ks)
{
	if (!ks) return 1;
	Key * lookupKey = keyNew ("/export", KEY_END);
	keyAddName (lookupKey, keyName (key) + strlen (keyName (parent)) + 1);
	if (!ksLookupByName (ks, keyName (lookupKey), KDB_O_NONE))
	{
		keyDel (lookupKey);
		return 0;
	}
	else
	{
		keyDel (lookupKey);
		return 1;
	}
}

static int csvWrite (KeySet * returned, Key * parentKey, KeySet * exportKS, Key * colAsParent, char delim, short useHeader)
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
	unsigned long columns = 0; // TODO: not needed?
	unsigned long lineCounter = 0;
	Key * cur;
	KeySet * toWriteKS;
	Key * toWrite;

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		cur = ksAtCursor (returned, it);
		if (keyIsDirectlyBelow (parentKey, cur) != 1) continue;
		colCounter = 0;
		if (useHeader)
		{
			useHeader = 0;
			continue;
		}
		if (colAsParent)
		{
			KeySet * tmpKs = ksDup (returned);
			KeySet * headerKs = ksCut (tmpKs, cur);
			ksDel (tmpKs);
			Key * tmp = ksAtCursor (headerKs, 1);
			int printDelim = 0;
			if (isExportKey (tmp, cur, exportKS))
			{
				fprintf (fp, "%s", keyName (tmp) + strlen (keyName (cur)) + 1);
				printDelim = 1;
				++colCounter;
			}

			for (elektraCursor itHeaderKs = 2; itHeaderKs < ksGetSize (headerKs); ++itHeaderKs)
			{
				tmp = ksAtCursor (headerKs, itHeaderKs);
				if (!isExportKey (tmp, cur, exportKS)) continue;
				++colCounter;
				if (printDelim) fprintf (fp, "%c", delim);
				if ((strchr (keyName (tmp), '\n') != NULL) && (keyName (tmp)[0] != '"'))
				{
					fprintf (fp, "\"%s\"", keyName (tmp) + strlen (keyName (cur)) + 1);
				}
				else
				{
					fprintf (fp, "%s", keyName (tmp) + strlen (keyName (cur)) + 1);
				}
				printDelim = 1;
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
		it--; /* Cut at current element */
		int printDelim = 0;

		for (elektraCursor itToWriteKs = 0; itToWriteKs < ksGetSize (toWriteKS); ++itToWriteKs)
		{
			toWrite = ksAtCursor (toWriteKS, itToWriteKs);
			if (!keyCmp (cur, toWrite)) continue;
			if (!isExportKey (toWrite, cur, exportKS))
			{
				continue;
			}
			if (printDelim) fprintf (fp, "%c", delim);
			++colCounter;
			if (keyGetMeta (toWrite, "internal/csvstorage/quoted"))
			{
				fprintf (fp, "\"%s\"", keyString (toWrite));
				printDelim = 1;
			}
			else if ((strchr (keyString (toWrite), '\n') != NULL) && (keyString (toWrite)[0] != '"'))
			{
				fprintf (fp, "\"%s\"", keyString (toWrite));
				printDelim = 1;
			}
			else
			{
				fprintf (fp, "%s", keyString (toWrite));
				printDelim = 1;
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
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Illegal number of columns (%lu - %lu) in line %lu", colCounter,
								 columns, lineCounter);
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
	Key * exportKey = ksLookupByName (config, "/export", 0);
	KeySet * exportKS = NULL;
	if (exportKey)
	{
		exportKS = ksCut (config, exportKey);
		ksAppend (config, exportKS);
		keyDel (ksLookup (exportKS, exportKey, KDB_O_POP));
	}
	short useHeader = 0;
	if (!strcmp (keyString (useHeaderKey), "skip")) useHeader = -1;
	int rc = csvWrite (returned, parentKey, exportKS, colAsParent, outputDelim, useHeader);
	ksDel (exportKS);
	return rc;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("csvstorage",
			ELEKTRA_PLUGIN_GET,	&elektraCsvstorageGet,
			ELEKTRA_PLUGIN_SET,	&elektraCsvstorageSet,
			ELEKTRA_PLUGIN_END);
}

