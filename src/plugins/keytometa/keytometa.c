/**
 * \file
 *
 * \brief A plugin that converts keys to metakeys and vice versa
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "keytometa.h"

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif


#include <errno.h>
#include <stdlib.h>
#include <stdbool.h>

/*
 * Wrapper for the function comparing by order meta data. As
 * qsort is not stable returning 0 on missing order may
 * mess up the original order.
 */
int elektraKeyCmpOrderWrapper(const void *a, const void *b)
{
	const Key **ka = (const Key **) a;
	const Key **kb = (const Key **) b;

	int orderResult = elektraKeyCmpOrder(*ka, *kb);

	/* comparing the order meta could not order the keys
	 * revert to comparing the names instead
	 */
	if (orderResult == 0) return keyCmp(*ka, *kb);

	return orderResult;
}

/* The KeySet MUST be sorted alphabetically (or at least ascending
 * by the length of keynames) for this function to work
 */
static Key *findNearestParent(Key *key, KeySet *ks)
{

	Key *current;
	ksSetCursor(ks, ksGetSize(ks) - 1);
	while ((current = ksPrev(ks)) != 0)
	{
		if (keyIsBelow (current, key))
		{
			return current;
		}
	}


	return 0;
}

static int appendWithNewline(char **str1, const char *str2)
{
	int len1 = *str1 ? strlen (*str1) : 0;
	int len2 = str2 ? strlen (str2) : 0;

	char *dst = malloc (len1 + len2 + 2);

	if (!dst) return 0;

	if (*str1) strncpy (dst, *str1, len1 + 1);
	if (*str1) strcat (dst, "\n");
	if (*str1)
		strncat (dst, str2, len2);
	else
		strncpy (dst, str2, len2 + 1);

	free (*str1);
	*str1 = dst;

	return strlen (*str1);
}

static const char *getAppendMode (Key *key) {
	const Key *appendModeKey = keyGetMeta(key, "convert/append");
	const char *appendMode;

	/* append to the next key is the default */
	appendMode = appendModeKey != 0 ? keyString(appendModeKey) : "";
	return appendMode;
}

int elektraKeyToMetaGet(Plugin *handle, KeySet *returned, Key *parentKey ELEKTRA_UNUSED)
{
	int errnosave = errno;

	/* configuration only */
	if (!strcmp (keyName(parentKey), "system/elektra/modules/keytometa"))
	{
		KeySet *n;
		ksAppend (returned, n=ksNew (30,
			keyNew ("system/elektra/modules/keytometa",
				KEY_VALUE, "keytometa plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/keytometa/exports", KEY_END),
			keyNew ("system/elektra/modules/keytometa/exports/get",
				KEY_FUNC, elektraKeyToMetaGet,
				KEY_END),
			keyNew ("system/elektra/modules/keytometa/exports/set",
				KEY_FUNC, elektraKeyToMetaSet,
				KEY_END),
			keyNew ("system/elektra/modules/keytometa/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew ("system/elektra/modules/keytometa/infos/author",
				KEY_VALUE, "Felix Berlakovich <elektra@berlakovich.net>", KEY_END),
			keyNew ("system/elektra/modules/keytometa/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
			keyNew ("system/elektra/modules/keytometa/infos/description",
				KEY_VALUE, "Converts keys to meta keys and vice versa", KEY_END),
			keyNew ("system/elektra/modules/keytometa/infos/placements",
				KEY_VALUE, "presetstorage postgetstorage", KEY_END),
			keyNew ("system/elektra/modules/keytometa/infos/needs",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/keytometa/infos/provides",
				KEY_VALUE, "conversion", KEY_END),
			keyNew ("system/elektra/modules/keytometa/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END));
		ksDel (n);
	}

	Key **keyArray = calloc (ksGetSize(returned), sizeof (Key *));
	int ret = elektraKsToMemArray(returned, keyArray);

	if (ret < 0) {
		free (keyArray);
		ELEKTRA_SET_ERROR(87, parentKey, strerror(errno));
		errno = errnosave;
		return 0;
	}

	size_t numKeys = ksGetSize(returned);
	qsort (keyArray, numKeys, sizeof (Key *), elektraKeyCmpOrderWrapper);

	Key *current;
	KeySet *convertedKeys = ksNew(0);

	char *currentData = 0;
	Key *appendToKey;

	for (size_t index = 0; index < numKeys; index++)
	{
		appendToKey = 0;
		current = keyArray[index];
		const Key *metaTargetKey = keyGetMeta(current, "convert/metaname");

		if (!metaTargetKey) continue;


		const char *appendMode = getAppendMode(current);
		if (!strcmp (appendMode, "next"))
		{
			/* find the key we will append to */
			Key *following;
			for (size_t lastIndex = index; lastIndex < numKeys; lastIndex++)
			{
				following = keyArray[lastIndex];
				appendToKey = following;

				if (strcmp (appendMode, getAppendMode (following))) break;
			}

		}

		if (!strcmp (appendMode, "previous"))
		{
			appendToKey = keyArray[index - 1];
		}

		if (appendToKey)
		{
			/* check if the target key is on the same level */
			if (keyGetMeta (current, "convert/append/samelevel")
					&& keyRel (current, appendToKey) != 0)
			{
				/* we found a key to append, but it was not on the same level
				 * fall back to parent
				 */
				appendToKey = 0;
			}
		}

		if (!appendToKey || !strcmp (appendMode, "parent"))
		{
			appendToKey = findNearestParent(current, returned);
		}

		/* write the collected data */
		if (appendToKey)
		{
			/* collect the data from all the keys to be collapsed */
			size_t i;
			for (i = index; i < numKeys; i++)
			{
				Key *collapseKey = keyArray[i];

				if (strcmp (appendMode, getAppendMode (collapseKey))) break;

				appendWithNewline (&currentData, keyString (collapseKey));

				keySetMeta (collapseKey, "convert/to", keyName (appendToKey));
				keySetMeta (collapseKey, "convert/to/metaname",
						keyString (metaTargetKey));
				ksAppendKey (convertedKeys, collapseKey);
				ksLookupByName (returned, keyName (collapseKey), KDB_O_NONE);
				ksPopAtCursor (returned, ksGetCursor (returned));
			}

			keySetMeta (appendToKey, keyString (metaTargetKey), currentData);
			free (currentData);
			currentData = 0;
			index = i;
		}

	}

	free (keyArray);
	elektraPluginSetData(handle, convertedKeys);

	errno = errnosave;
	return 1; /* success */
}



int elektraKeyToMetaSet(Plugin *handle, KeySet *returned, Key *parentKey ELEKTRA_UNUSED)
{
	KeySet *converted = elektraPluginGetData(handle);

	/* nothing to do */
	if (converted == 0) return 1;

	ksRewind (converted);

	char *saveptr;
	char *value = 0;
	Key *current;
	Key *previous = 0;
	while ((current = ksNext (converted)) != 0)
	{
		const Key *targetName = keyGetMeta(current, "convert/to");
		const Key *metaName = keyGetMeta(current, "convert/to/metaname");

		/* they should always exist, just to be sure */
		if (targetName && metaName) {
			Key *target = ksLookupByName(returned, keyString(targetName), KDB_O_NONE);

			/* this might be NULL as the key might have been deleted */
			if (target) {

				char *result = 0;
				if (target != previous) {
					free (value);
					const Key *valueKey = keyGetMeta(target, keyString(metaName));
					size_t valueSize = keyGetValueSize(valueKey);
					value = malloc (valueSize);
					keyGetString(valueKey, value, valueSize);
					keySetMeta(target, keyString(metaName), 0);
					result = strtok_r (value, "\n", &saveptr);
				} else {
					result = strtok_r (NULL, "\n", &saveptr);
				}

				keySetString(current, result);

				previous = target;
			}
		}

		keySetMeta(current, "convert/to", 0);
		keySetMeta(current, "convert/to/metaname", 0);

		ksAppendKey(returned, current);
	}

	free (value);

	ksDel (converted);

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(keytometa)
{
	return elektraPluginExport("keytometa",
		ELEKTRA_PLUGIN_GET,	&elektraKeyToMetaGet,
		ELEKTRA_PLUGIN_SET,	&elektraKeyToMetaSet,
		ELEKTRA_PLUGIN_END);
}

