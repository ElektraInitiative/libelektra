/**
 * @file
 *
 * @brief Contains the set direction of the hosts plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "hosts.h"

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include <kdbextension.h>
#include <kdbproposal.h>

static int keyCmpOrderWrapper (const void * a, const void * b)
{
	return elektraKeyCmpOrder (*((const Key **) a), *((const Key **) b));
}

static void writeComment (const char * spaces, const char * start, const char * comment, FILE * fp)
{
	if (spaces)
	{
		char * endptr;
		long spaceValue = strtol (spaces, &endptr, 10);

		if (*endptr == '\0')
		{
			for (int i = 0; i < spaceValue; i++)
			{
				fprintf (fp, " ");
			}
		}
	}

	if (start)
	{
		fprintf (fp, "%s", start);
	}

	if (comment)
	{
		fprintf (fp, "%s", comment);
	}
}

static const char * getMetaValue (Key * key, const char * metaName)
{
	const Key * metaKey = keyGetMeta (key, metaName);

	if (metaKey) return keyString (metaKey);

	return 0;
}

static void writeLineComments (Key * key, FILE * fp)
{
	// TODO: this is really inefficient
	KeySet * metaKeys = elektraKeyGetMetaKeySet (key);
	Key * commentParent = keyNew ("comment", KEY_META_NAME, KEY_END);
	KeySet * comments = elektraArrayGet (commentParent, metaKeys);
	keyDel (commentParent);

	ksRewind (comments);
	Key * current;
	while ((current = ksNext (comments)))
	{
		if (strcmp (keyName (current), "comment/#0"))
		{
			Key * spaceKey = keyDup (current);
			keyAddBaseName (spaceKey, "space");
			Key * startKey = keyDup (current);
			keyAddBaseName (startKey, "start");
			const char * spaces = getMetaValue (key, keyName (spaceKey));
			const char * start = getMetaValue (key, keyName (startKey));
			const char * comment = getMetaValue (key, keyName (current));
			keyDel (spaceKey);
			keyDel (startKey);
			writeComment (spaces, start, comment, fp);
			fprintf (fp, "\n");
		}
	}

	ksDel (metaKeys);
	ksDel (comments);
}

static void writeInlineComment (Key * key, FILE * fp)
{

	const char * spaces = getMetaValue (key, "comment/#0/space");
	const char * start = getMetaValue (key, "comment/#0/start");
	const char * comment = getMetaValue (key, "comment/#0");

	writeComment (spaces, start, comment, fp);
}

static void writeHostsEntry (Key * key, KeySet * returned, FILE * fp)
{
	fprintf (fp, "%s\t%s", (char *) keyValue (key), (char *) keyBaseName (key));
	/* position the cursor at the current key and
	 * iterate over its subkeys
	 */
	ksLookup (returned, key, KDB_O_NONE);
	Key * alias;
	while ((alias = ksNext (returned)) != 0)
	{
		if (keyIsBelow (key, alias) != 1) break;

		fprintf (fp, " %s", (char *) keyBaseName (alias));
	}
}

int elektraHostsSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	int errnosave = errno;
	FILE * fp;

	fp = fopen (keyString (parentKey), "w");

	if (fp == 0)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		errno = errnosave;
		return -1;
	}

	/* build an array of entries and sort them according to their order metadata */
	Key ** keyArray;
	size_t arraySize = ksGetSize (returned);
	keyArray = calloc (arraySize, sizeof (Key *));

	ksRewind (returned);
	int ret = elektraKsToMemArray (returned, keyArray);

	if (ret < 0)
	{
		ELEKTRA_SET_RESOURCE_ERROR (parentKey, strerror (errno));
		fclose (fp);
		return -1;
	}

	qsort (keyArray, arraySize, sizeof (Key *), keyCmpOrderWrapper);

	Key * ipv4Base = keyDup (parentKey);
	keyAddBaseName (ipv4Base, "ipv4");
	Key * ipv6Base = keyDup (parentKey);
	keyAddBaseName (ipv6Base, "ipv6");

	/* now write the hosts file */
	for (size_t i = 0; i < arraySize; ++i)
	{
		Key * key = keyArray[i];

		/* only process canonical name keys */
		if (!keyIsDirectlyBelow (ipv4Base, key) && !keyIsDirectlyBelow (ipv6Base, key)) continue;

		writeLineComments (key, fp);
		writeHostsEntry (key, returned, fp);
		writeInlineComment (key, fp);
		fprintf (fp, "\n");
	}

	writeLineComments (parentKey, fp);

	keyDel (ipv4Base);
	keyDel (ipv6Base);

	fclose (fp);
	errno = errnosave;
	elektraFree (keyArray);
	return 1;
}
