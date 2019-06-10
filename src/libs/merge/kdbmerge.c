#include "kdbmerge.h"
#include "kdb.h"
#include "kdbassert.h"
#include "kdblogger.h"
#include "kdbprivate.h"
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

char * strremove (char * str, const char * sub)
{
	size_t len = strlen (sub);
	if (len > 0)
	{
		char * p = str;
		while ((p = strstr (p, sub)) != NULL)
		{
			memmove (p, p + len, strlen (p + len) + 1);
		}
	}
	return str;
}

static KeySet * removeRoots (KeySet * original, Key * root)
{
	KeySet * result = ksNew (0, KS_END);
	ksRewind (original);
	const char * rootString = keyName (root);
	Key * k;
	while ((k = ksNext (original)) != NULL)
	{
		char * shortenedString = elektraMalloc (keyGetNameSize (k));
		if (keyGetName (k, shortenedString, keyGetNameSize (k)) < 0)
		{
			ELEKTRA_ASSERT (false, "This should not happen");
			return NULL;
		};
		ELEKTRA_ASSERT (keyIsBelow (root, k), "Removing root %s from beginning of key %s is not possible, as it is not below it",
				rootString, shortenedString);
		shortenedString = strremove (shortenedString, rootString);
		Key * keyCopy = keyDup (k);
		int retVal = keySetName (keyCopy, shortenedString);
		if (retVal > 0)
		{
			printf ("Setting new name was not possible: %d", retVal);
			return NULL;
		}
		elektraFree (shortenedString);
		ksAppendKey (result, keyCopy);
	}
	return result;
}

static int preprendAndAppend (KeySet * ks, Key * toAppend, const char * extension)
{
	char * newName = elektraMalloc (strlen (keyName (toAppend)) + strlen (extension) + 1);
	strcpy (newName, extension);
	strcat (newName, keyName (toAppend));
	int status = keySetName (toAppend, newName);
	elektraFree (newName);
	if (status < 0)
	{
		return -1;
	}
	status = ksAppendKey (ks, toAppend);
	if (status < 0)
	{
		return -1;
	}
	return 1;
}

KeySet * kdbMerge (KeySet * our, Key * ourRoot, KeySet * their, Key * theirRoot, KeySet * base, Key * baseRoot, Key * resultRoot)
{
	ELEKTRA_LOG ("cmerge starts");
	KeySet * result = ksNew (0, KS_END);
	KeySet * ourCropped = removeRoots (our, ourRoot);
	KeySet * theirCropped = removeRoots (their, theirRoot);
	KeySet * baseCropped = removeRoots (base, baseRoot);
	ksRewind (ourCropped);
	ksRewind (theirCropped);
	ksRewind (baseCropped);
	Key * base_key;
	while ((base_key = ksNext (baseCropped)) != NULL)
	{
		bool isInOur = ksLookup (ourCropped, base_key, 0) != NULL;
		bool isInTheir = ksLookup (theirCropped, base_key, 0) != NULL;
		if (isInOur && isInTheir)
		{
			if (preprendAndAppend (result, keyDup (base_key), keyName (resultRoot)) < 0)
			{
				return NULL;
			}
		}
	}
	ELEKTRA_LOG ("Resulting keyset of cmerge has size %ld", ksGetSize (result));
	return result;
}
