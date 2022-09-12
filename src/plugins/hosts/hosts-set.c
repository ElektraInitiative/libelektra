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

static int keyCmpOrderWrapper (const void * a, const void * b)
{
	return elektraKeyCmpOrder (*((const ElektraKey **) a), *((const ElektraKey **) b));
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

static const char * getMetaValue (ElektraKey * key, const char * metaName)
{
	const ElektraKey * metaKey = elektraKeyGetMeta (key, metaName);

	if (metaKey) return elektraKeyString (metaKey);

	return 0;
}

static void writeLineComments (ElektraKey * key, FILE * fp)
{
	ElektraKeyset * metaKeys = elektraKeyMeta (key);
	ElektraKey * commentParent = elektraKeyNew ("meta:/comment", ELEKTRA_KEY_END);
	ElektraKeyset * comments = elektraArrayGet (commentParent, metaKeys);
	elektraKeyDel (commentParent);

	elektraKeysetRewind (comments);
	ElektraKey * current;
	while ((current = elektraKeysetNext (comments)))
	{
		if (strcmp (elektraKeyName (current), "meta:/comment/#0") != 0)
		{
			ElektraKey * spaceKey = elektraKeyDup (current, ELEKTRA_KEY_CP_ALL);
			elektraKeyAddBaseName (spaceKey, "space");
			ElektraKey * startKey = elektraKeyDup (current, ELEKTRA_KEY_CP_ALL);
			elektraKeyAddBaseName (startKey, "start");
			const char * spaces = getMetaValue (key, elektraKeyName (spaceKey));
			const char * start = getMetaValue (key, elektraKeyName (startKey));
			const char * comment = getMetaValue (key, elektraKeyName (current));
			elektraKeyDel (spaceKey);
			elektraKeyDel (startKey);
			writeComment (spaces, start, comment, fp);
			fprintf (fp, "\n");
		}
	}

	elektraKeysetDel (comments);
}

static void writeInlineComment (ElektraKey * key, FILE * fp)
{

	const char * spaces = getMetaValue (key, "comment/#0/space");
	const char * start = getMetaValue (key, "comment/#0/start");
	const char * comment = getMetaValue (key, "comment/#0");

	writeComment (spaces, start, comment, fp);
}

static void writeHostsEntry (ElektraKey * key, ElektraKeyset * returned, FILE * fp)
{
	fprintf (fp, "%s\t%s", (char *) elektraKeyValue (key), (char *) elektraKeyBaseName (key));
	/* position the cursor at the current key and
	 * iterate over its subkeys
	 */
	elektraKeysetLookup (returned, key, ELEKTRA_KDB_O_NONE);
	ElektraKey * alias;
	while ((alias = elektraKeysetNext (returned)) != 0)
	{
		if (elektraKeyIsBelow (key, alias) != 1) break;

		fprintf (fp, " %s", (char *) elektraKeyBaseName (alias));
	}
}

int elektraHostsSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	int errnosave = errno;
	FILE * fp;

	fp = fopen (elektraKeyString (parentKey), "w");

	if (fp == 0)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		errno = errnosave;
		return -1;
	}

	/* build an array of entries and sort them according to their order metadata */
	ElektraKey ** keyArray;
	size_t arraySize = elektraKeysetGetSize (returned);
	keyArray = calloc (arraySize, sizeof (ElektraKey *));

	elektraKeysetRewind (returned);
	int ret = elektraKsToMemArray (returned, keyArray);

	if (ret < 0)
	{
		ELEKTRA_SET_RESOURCE_ERROR (parentKey, strerror (errno));
		fclose (fp);
		return -1;
	}

	qsort (keyArray, arraySize, sizeof (ElektraKey *), keyCmpOrderWrapper);

	ElektraKey * ipv4Base = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	elektraKeyAddBaseName (ipv4Base, "ipv4");
	ElektraKey * ipv6Base = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	elektraKeyAddBaseName (ipv6Base, "ipv6");

	/* now write the hosts file */
	for (size_t i = 0; i < arraySize; ++i)
	{
		ElektraKey * key = keyArray[i];

		/* only process canonical name keys */
		if (!elektraKeyIsDirectlyBelow (ipv4Base, key) && !elektraKeyIsDirectlyBelow (ipv6Base, key)) continue;

		writeLineComments (key, fp);
		writeHostsEntry (key, returned, fp);
		writeInlineComment (key, fp);
		fprintf (fp, "\n");
	}

	writeLineComments (parentKey, fp);

	elektraKeyDel (ipv4Base);
	elektraKeyDel (ipv6Base);

	fclose (fp);
	errno = errnosave;
	elektraFree (keyArray);
	return 1;
}
