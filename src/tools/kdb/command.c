/**
 * @file
 *
 * @brief Implementation of things used everywhere in the kdb tool
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>

#include <kdberrors.h>
#include <kdbinternal.h>
#include <stdio.h>
#include <string.h>

Key * expandKeyName (const char * name, bool * resolved)
{
	*resolved = false;
	Key * ret = NULL;
	if (name == NULL)
	{
		return ret;
	}

	if (name[0] != '+')
	{
		ret = keyNew (name, KEY_END);
	}
	else
	{
		KeySet * bookmarks = ksNew (0, KS_END);
		Key * bookmarkBase = keyNew (CLI_BASE_KEY "/bookmarks", KEY_END);
		KDB * handle = kdbOpen (NULL, bookmarkBase);
		kdbGet (handle, bookmarks, bookmarkBase);
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
				ret = resolvedKey;
				*resolved = true;
			}
		}
		kdbClose (handle, bookmarkBase);
		keyDel (bookmarkBase);
		ksDel (bookmarks);
	}

	return ret;
}


Key * getKeyFromOptions (const char * rawName, Key * errorKey, bool verbose)
{
	bool resolved = false;
	Key * result = expandKeyName (rawName, &resolved);
	if (result == NULL)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not resolve bookmark in '%s'", rawName);
		return NULL;
	}

	if (verbose && resolved)
	{
		printf ("resolved bookmark: \'%s\' -> \'%s\'\n", rawName, keyName (result));
	}

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
