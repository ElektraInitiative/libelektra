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
#include <errno.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbprivate.h> // for ksRenameKeys
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

static char * parseLine (char * origLine, char delim, unsigned long offset, ElektraKey * parentKey, unsigned long lineNr, int lastLine)
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
static ElektraKeyset * createHeaders (ElektraKey * parentKey, int columns, const char ** colNames)
{
	ElektraKeyset * header = elektraKeysetNew (0, ELEKTRA_KS_END);
	int colCounter = 0;
	// if no headerline exists name the columns 0..N where N is the number of columns
	ElektraKey * orderKey = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	elektraKeyAddName (orderKey, "#");
	while (colCounter < columns)
	{
		if (elektraArrayIncName (orderKey) == -1)
		{
			elektraKeyDel (orderKey);
			elektraKeysetDel (header);
			return NULL;
		}
		ElektraKey * key = elektraKeyDup (orderKey, ELEKTRA_KEY_CP_ALL);
		if (colNames && (colNames + colCounter))
			elektraKeySetString (key, colNames[colCounter]);
		else
			elektraKeySetString (key, elektraKeyBaseName (key));
		elektraKeysetAppendKey (header, key);
		++colCounter;
	}
	elektraKeyDel (orderKey);
	return header;
}

/// @returns a newly allocated keyset with the column names
static ElektraKeyset * readHeaders (ElektraKey * parentKey, char * lineBuffer, char delim, int lineCounter, int lastLine, const char ** colNames)
{
	int colCounter = 0;
	unsigned long offset = 0;
	char * col;
	offset = 0;
	ElektraKey * orderKey = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	elektraKeyAddName (orderKey, "#");
	ElektraKeyset * header = elektraKeysetNew (0, ELEKTRA_KS_END);
	while ((col = parseLine (lineBuffer, delim, offset, parentKey, lineCounter, lastLine)) != NULL)
	{
		offset += elektraStrLen (col);
		if (elektraArrayIncName (orderKey) == -1)
		{
			elektraFree (lineBuffer);
			elektraKeyDel (orderKey);
			elektraKeysetDel (header);
			return NULL;
		}
		ElektraKey * key = elektraKeyDup (orderKey, ELEKTRA_KEY_CP_ALL);
		if (colNames && (colNames + colCounter))
		{
			elektraKeySetString (key, colNames[colCounter]);
		}
		else
		{
			elektraKeySetString (key, col);
		}
		elektraKeysetAppendKey (header, key);
		++colCounter;
	}
	elektraKeyDel (orderKey);
	return header;
}

static int csvRead (ElektraKeyset * returned, ElektraKey * parentKey, char delim, ElektraKey * colAsParent, short useHeader, unsigned long fixColumnCount,
		    const char ** colNames)
{
	const char * fileName;
	fileName = elektraKeyString (parentKey);
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
	int nr_keys = 1;
	ElektraKeyset * header;
	ElektraKey * key;
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
	ElektraKey * dirKey;
	ElektraKey * cur;
	dirKey = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	elektraKeyAddName (dirKey, "#");
	elektraFree (lineBuffer);
	elektraKeysetRewind (header);
	while (1)
	{
		lineBuffer = readNextLine (fp, delim, &lastLine, &linesRead);
		if (!lineBuffer)
		{
			fclose (fp);
			elektraKeyDel (dirKey);
			elektraKeysetDel (header);
			return (lineCounter > 0) ? 1 : 0;
		}

		if (elektraArrayIncName (dirKey) == -1)
		{
			elektraFree (lineBuffer);
			elektraKeyDel (dirKey);
			elektraKeysetDel (header);
			fclose (fp);
			return -1;
		}
		++nr_keys;
		unsigned long offset = 0;
		char * col;
		colCounter = 0;
		char * lastIndex = "#0";
		elektraKeysetRewind (header);
		ElektraKeyset * tmpKs = elektraKeysetNew (0, ELEKTRA_KS_END);
		while ((col = parseLine (lineBuffer, delim, offset, parentKey, lineCounter, lastLine)) != NULL)
		{
			cur = elektraKeysetNext (header);
			offset += elektraStrLen (col);
			key = elektraKeyDup (dirKey, ELEKTRA_KEY_CP_ALL);
			if (col[0] == '"')
			{
				if (col[elektraStrLen (col) - 2] == '"')
				{
					elektraKeySetMeta (key, "internal/csvstorage/quoted", "");
					++col;
					col[elektraStrLen (col) - 2] = '\0';
				}
			}
			elektraKeyAddName (key, elektraKeyString (cur));
			elektraKeySetString (key, col);
			elektraKeysetAppendKey (tmpKs, key);
			lastIndex = (char *) elektraKeyBaseName (cur);
			++nr_keys;
			++colCounter;
		}
		if (colAsParent)
		{
			if (!(lineCounter <= 1 && useHeader))
			{
				elektraKeySetString (dirKey, lastIndex);
				elektraKeysetAppendKey (tmpKs, elektraKeyDup (dirKey, ELEKTRA_KEY_CP_ALL));
				ElektraKey * lookupKey = elektraKeyNew (elektraKeyName (dirKey), ELEKTRA_KEY_END);
				elektraKeyAddName (lookupKey, elektraKeyString (colAsParent));
				ElektraKey * indexKey = elektraKeysetLookupByName (tmpKs, elektraKeyName (lookupKey), 0);
				ElektraKey * renameKey = elektraKeyNew (elektraKeyName (dirKey), ELEKTRA_KEY_END);
				elektraKeySetBaseName (renameKey, elektraKeyString (indexKey));
				elektraKeysetRewind (tmpKs);
				ElektraKeyset * renamedKs = elektraKeysetRenameKeys (tmpKs, elektraKeyName (renameKey));
				elektraKeysetAppendKey (renamedKs, elektraKeyDup (renameKey, ELEKTRA_KEY_CP_ALL));
				elektraKeysetRewind (renamedKs);
				elektraKeyDel (lookupKey);
				elektraKeyDel (renameKey);
				elektraKeysetRewind (renamedKs);
				elektraKeysetRewind (tmpKs);
				elektraKeysetAppend (returned, renamedKs);
				elektraKeysetDel (renamedKs);
			}
		}
		else
		{
			elektraKeySetString (dirKey, lastIndex);
			elektraKeysetAppend (returned, tmpKs);
			elektraKeysetAppendKey (returned, elektraKeyDup (dirKey, ELEKTRA_KEY_CP_ALL));
		}
		elektraKeysetDel (tmpKs);
		tmpKs = NULL;
		if (colCounter != columns)
		{
			if (fixColumnCount)
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Illegal number of columns (%lu - %lu) in line %lu: %s",
									 colCounter, columns, lineCounter, lineBuffer);
				elektraFree (lineBuffer);
				fclose (fp);
				elektraKeyDel (dirKey);
				elektraKeysetDel (header);
				return -1;
			}
			ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF (parentKey, "Illegal number of columns (%lu - %lu)  in line %lu: %s",
								   colCounter, columns, lineCounter, lineBuffer);
		}
		lineCounter += linesRead;
		elektraFree (lineBuffer);
		elektraKeysetDel (tmpKs);
	}
	key = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	elektraKeySetString (key, elektraKeyBaseName (dirKey));
	elektraKeysetAppendKey (returned, key);
	elektraKeyDel (dirKey);
	fclose (fp);
	elektraKeysetDel (header);
	return 1;
}

int elektraCsvstorageGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/csvstorage"))
	{
		ElektraKeyset * contract = elektraKeysetNew (
			30, elektraKeyNew ("system:/elektra/modules/csvstorage", ELEKTRA_KEY_VALUE, "csvstorage plugin waits for your orders", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/csvstorage/exports", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/csvstorage/exports/get", ELEKTRA_KEY_FUNC, elektraCsvstorageGet, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/csvstorage/exports/set", ELEKTRA_KEY_FUNC, elektraCsvstorageSet, ELEKTRA_KEY_END),
#include ELEKTRA_README
			elektraKeyNew ("system:/elektra/modules/csvstorage/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return 1; /* success */
	}

	ElektraKeyset * config = elektraPluginGetConfig (handle);
	ElektraKey * delimKey = elektraKeysetLookupByName (config, "/delimiter", 0);
	char delim = ',';
	if (delimKey)
	{
		const char * delimString = elektraKeyString (delimKey);
		delim = delimString[0];
	}

	ElektraKey * readHeaderKey = elektraKeysetLookupByName (config, "/header", 0);
	short useHeader = 0;
	if (readHeaderKey)
	{
		const char * printHeaderString = elektraKeyString (readHeaderKey);
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
	ElektraKey * fixColumnCountKey = elektraKeysetLookupByName (config, "/columns", 0);
	if (fixColumnCountKey)
	{
		if (elektraKeyString (fixColumnCountKey))
		{
			fixColumnCount = atol (elektraKeyString (fixColumnCountKey));
		}
	}
	ElektraKey * colAsParent = elektraKeysetLookupByName (config, "/columns/index", 0);
	ElektraKey * setNamesKey = elektraKeysetLookupByName (config, "/columns/names", 0);
	char * colNames = NULL;
	if (setNamesKey)
	{
		if (fixColumnCountKey)
		{
			ElektraKeyset * namesKS = elektraKeysetCut (config, setNamesKey);
			unsigned long nrNames = (unsigned long) elektraKeysetGetSize (namesKS) - 1;
			if (nrNames == fixColumnCount)
			{
				colNames = (char *) elektraMalloc (nrNames * sizeof (char *));
				ElektraKey * cur;
				char ** ptr = (char **) colNames;
				while ((cur = elektraKeysetNext (namesKS)) != NULL)
				{
					if (!strcmp (elektraKeyName (cur), elektraKeyName (setNamesKey))) continue;
					if (!strcmp (elektraKeyString (cur), ""))
						*ptr = NULL;
					else
						*ptr = (char *) elektraKeyString (cur);
					++ptr;
				}
			}
			elektraKeysetAppend (config, namesKS);
			elektraKeysetDel (namesKS);
		}
	}
	int nr_keys;
	nr_keys = csvRead (returned, parentKey, delim, colAsParent, useHeader, fixColumnCount, (const char **) colNames);
	if (colNames) elektraFree (colNames);
	if (nr_keys == -1) return -1;
	return 1;
}

static int isExportKey (const ElektraKey * key, const ElektraKey * parent, ElektraKeyset * ks)
{
	if (!ks) return 1;
	ElektraKey * lookupKey = elektraKeyNew ("/export", ELEKTRA_KEY_END);
	elektraKeyAddName (lookupKey, elektraKeyName (key) + strlen (elektraKeyName (parent)) + 1);
	if (!elektraKeysetLookupByName (ks, elektraKeyName (lookupKey), ELEKTRA_KDB_O_NONE))
	{
		elektraKeyDel (lookupKey);
		return 0;
	}
	else
	{
		elektraKeyDel (lookupKey);
		return 1;
	}
}

static int csvWrite (ElektraKeyset * returned, ElektraKey * parentKey, ElektraKeyset * exportKS, ElektraKey * colAsParent, char delim, short useHeader)
{
	FILE * fp;
	fp = fopen (elektraKeyString (parentKey), "w");
	if (!fp)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		return -1;
	}

	elektraKeyDel (elektraKeysetLookup (returned, parentKey, ELEKTRA_KDB_O_POP));

	unsigned long colCounter = 0;
	unsigned long columns = 0; // TODO: not needed?
	unsigned long lineCounter = 0;
	ElektraKey * cur;
	ElektraKeyset * toWriteKS;
	ElektraKey * toWrite;

	elektraKeysetRewind (returned);
	while ((cur = elektraKeysetNext (returned)) != NULL)
	{
		if (elektraKeyIsDirectlyBelow (parentKey, cur) != 1) continue;
		colCounter = 0;
		if (useHeader)
		{
			useHeader = 0;
			continue;
		}
		if (colAsParent)
		{
			ElektraKeyset * tmpKs = elektraKeysetDup (returned);
			elektraKeysetRewind (tmpKs);
			ElektraKeyset * headerKs = elektraKeysetCut (tmpKs, cur);
			elektraKeysetRewind (headerKs);
			elektraKeysetDel (tmpKs);
			elektraKeysetNext (headerKs);
			ElektraKey * tmp = elektraKeysetNext (headerKs);
			int printDelim = 0;
			if (isExportKey (tmp, cur, exportKS))
			{
				fprintf (fp, "%s", elektraKeyName (tmp) + strlen (elektraKeyName (cur)) + 1);
				printDelim = 1;
				++colCounter;
			}
			while ((tmp = elektraKeysetNext (headerKs)) != NULL)
			{
				if (!isExportKey (tmp, cur, exportKS)) continue;
				++colCounter;
				if (printDelim) fprintf (fp, "%c", delim);
				if ((strchr (elektraKeyName (tmp), '\n') != NULL) && (elektraKeyName (tmp)[0] != '"'))
				{
					fprintf (fp, "\"%s\"", elektraKeyName (tmp) + strlen (elektraKeyName (cur)) + 1);
				}
				else
				{
					fprintf (fp, "%s", elektraKeyName (tmp) + strlen (elektraKeyName (cur)) + 1);
				}
				printDelim = 1;
			}
			fprintf (fp, "\n");
			if (columns == 0)
			{
				columns = colCounter;
			}
			colAsParent = NULL;
			elektraKeysetDel (headerKs);
		}
		colCounter = 0;
		toWriteKS = elektraKeysetCut (returned, cur);
		elektraKeysetRewind (toWriteKS);
		int printDelim = 0;
		while (1)
		{
			toWrite = elektraKeysetNext (toWriteKS);
			if (!elektraKeyCmp (cur, toWrite)) continue;
			if (!toWrite) break;
			if (!isExportKey (toWrite, cur, exportKS))
			{
				continue;
			}
			if (printDelim) fprintf (fp, "%c", delim);
			++colCounter;
			if (elektraKeyGetMeta (toWrite, "internal/csvstorage/quoted"))
			{
				fprintf (fp, "\"%s\"", elektraKeyString (toWrite));
				printDelim = 1;
			}
			else if ((strchr (elektraKeyString (toWrite), '\n') != NULL) && (elektraKeyString (toWrite)[0] != '"'))
			{
				fprintf (fp, "\"%s\"", elektraKeyString (toWrite));
				printDelim = 1;
			}
			else
			{
				fprintf (fp, "%s", elektraKeyString (toWrite));
				printDelim = 1;
			}
		}
		elektraKeysetDel (toWriteKS);
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

int elektraCsvstorageSet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	ElektraKeyset * config = elektraPluginGetConfig (handle);
	ElektraKey * delimKey = elektraKeysetLookupByName (config, "/delimiter", 0);
	char outputDelim;
	if (delimKey)
	{
		const char * delimString = elektraKeyString (delimKey);
		outputDelim = delimString[0];
	}
	else
	{
		outputDelim = ',';
	}
	ElektraKey * colAsParent = elektraKeysetLookupByName (config, "/columns/index", 0);
	ElektraKey * useHeaderKey = elektraKeysetLookupByName (config, "/header", 0);
	ElektraKey * exportKey = elektraKeysetLookupByName (config, "/export", 0);
	ElektraKeyset * exportKS = NULL;
	if (exportKey)
	{
		exportKS = elektraKeysetCut (config, exportKey);
		elektraKeysetAppend (config, exportKS);
		elektraKeyDel (elektraKeysetLookup (exportKS, exportKey, ELEKTRA_KDB_O_POP));
		elektraKeysetRewind (exportKS);
	}
	short useHeader = 0;
	if (!strcmp (elektraKeyString (useHeaderKey), "skip")) useHeader = -1;
	int rc = csvWrite (returned, parentKey, exportKS, colAsParent, outputDelim, useHeader);
	elektraKeysetDel (exportKS);
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

