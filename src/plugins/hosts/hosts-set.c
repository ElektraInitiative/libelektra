/**
 * @file
 *
 * @brief Contains the set direction of the hosts plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./hosts.h"

#include <elektra/ease/array.h>
#include <elektra/ease/meta.h>
#include <elektra/ease/utils.h>
#include <internal/config.h>
#include <internal/macros/attributes.h>
#include <internal/macros/plugin_errors.h>
#include <internal/utility/alloc.h>

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
	KeySet * metaKeys = keyMeta (key);
	Key * commentParent = keyNew ("meta:/comment", KEY_END);
	KeySet * comments = elektraArrayGet (commentParent, metaKeys);
	keyDel (commentParent);

	Key * current;

	for (elektraCursor it = 0; it < ksGetSize (comments); ++it)
	{
		current = ksAtCursor (comments, it);
		if (strcmp (keyName (current), "meta:/comment/#0") != 0)
		{
			Key * spaceKey = keyDup (current, KEY_CP_ALL);
			keyAddBaseName (spaceKey, "space");
			Key * startKey = keyDup (current, KEY_CP_ALL);
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

	/* set the iterator to the current key and
	 * iterate over its subkeys
	 */
	for (elektraCursor it = ksSearch (returned, key) + 1; it < ksGetSize (returned); ++it)
	{
		Key * alias = ksAtCursor (returned, it);
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

	int ret = elektraKsToMemArray (returned, keyArray);

	if (ret < 0)
	{
		ELEKTRA_SET_RESOURCE_ERROR (parentKey, strerror (errno));
		fclose (fp);
		return -1;
	}

	qsort (keyArray, arraySize, sizeof (Key *), keyCmpOrderWrapper);

	Key * ipv4Base = keyDup (parentKey, KEY_CP_ALL);
	keyAddBaseName (ipv4Base, "ipv4");
	Key * ipv6Base = keyDup (parentKey, KEY_CP_ALL);
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
