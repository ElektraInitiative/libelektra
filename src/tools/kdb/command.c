/**
 * @file
 *
 * @brief Implementation of things used everywhere in the kdb tool
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>

#include <kdberrors.h>
#include <stdio.h>
#include <string.h>

const char * expandKeyName (const char * name, bool * resolved)
{
	*resolved = false;
	const char * ret = NULL;
	if (name == NULL)
	{
		return ret;
	}

	KeySet * bookmarks = ksNew (0, KS_END);
	Key * bookmarkBase = keyNew (CLI_BASE_KEY "/bookmarks", KEY_END);
	KDB * handle = kdbOpen (NULL, bookmarkBase);
	kdbGet (handle, bookmarks, bookmarkBase);

	if (name[0] != '+')
	{
		ret = elektraStrDup (name);
	}
	else
	{
		int bookmarkEnd = strcspn (name, "/");
		char * bookmarkName = elektraMalloc (bookmarkEnd);
		strncpy (bookmarkName, name + 1, bookmarkEnd - 1);
		bookmarkName[bookmarkEnd - 1] = '\0';
		Key * bookmarkLookup = keyNew (CLI_BASE_KEY "/bookmarks", KEY_END);
		keyAddBaseName (bookmarkLookup, bookmarkName);
		Key * bookmarkKey = ksLookup (bookmarks, bookmarkLookup, KDB_O_DEL);
		elektraFree (bookmarkName);
		if (bookmarkKey != NULL)
		{
			Key * resolvedKey = keyNew (keyString (bookmarkKey), KEY_END);
			if (resolvedKey != NULL)
			{ // the bookmark's value may not be valid key name
				keyAddName (resolvedKey, name + bookmarkEnd);
				ret = elektraStrDup (keyName (resolvedKey));
				keyDel (resolvedKey);
				*resolved = true;
			}
		}
	}
	kdbClose (handle, bookmarkBase);
	keyDel (bookmarkBase);
	ksDel (bookmarks);
	return ret;
}


const char * getKeyNameFromOptions (const char * rawName, Key * errorKey, bool verbose)
{
	bool resolved = false;
	const char * result = expandKeyName (rawName, &resolved);
	if (result == NULL)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not resolve bookmark in '%s'", rawName);
		return NULL;
	}

	if (verbose && resolved)
	{
		printf ("resolved bookmark: \'%s\' -> \'%s\'\n", rawName, result);
	}

	Key * key = keyNew (result, KEY_END);
	if (key == NULL)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "'%s' is not valid key name.", result);
		elektraFree ((void *) result);
		return NULL;
	}
	keyDel (key);
	return result;
}

void cliPrint (int logLevel, int minLogLevel, const char * fmt, ...)
{
	if (logLevel < minLogLevel)
	{
		return;
	}

	va_list args;
	va_start (args, fmt);
	vprintf (fmt, args);
	va_end (args);
}

void cliErrorPrint (int logLevel, int minLogLevel, const char * fmt, ...)
{
	if (logLevel < minLogLevel)
	{
		return;
	}

	va_list args;
	va_start (args, fmt);
	vfprintf (stderr, fmt, args);
	va_end (args);
}