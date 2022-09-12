/**
 * @file
 *
 * @brief A plugin that converts keys to metakeys and vice versa
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "keytometa.h"

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif


#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>

static const char * CONVERT_METANAME = "convert/metaname";
static const char * CONVERT_TARGET = "convert/to";
static const char * CONVERT_APPEND_SAMELEVEL = "convert/append/samelevel";
static const char * CONVERT_APPENDMODE = "convert/append";

/*
 * Wrapper for the function comparing by order metadata. As
 * qsort is not stable returning 0 on missing order may
 * mess up the original order.
 */
int elektraKeyCmpOrderWrapper (const void * a, const void * b)
{
	const ElektraKey ** ka = (const ElektraKey **) a;
	const ElektraKey ** kb = (const ElektraKey **) b;

	int orderResult = elektraKeyCmpOrder (*ka, *kb);

	/* comparing the order meta could not order the keys
	 * revert to comparing the names instead
	 */
	if (orderResult == 0) return keyCmp (*ka, *kb);

	return orderResult;
}

/* The KeySet MUST be sorted alphabetically (or at least ascending
 * by the length of keynames) for this function to work
 */
static ElektraKey * findNearestParent (ElektraKey * key, ElektraKeyset * ks)
{
	ElektraKey * current;
	ksSetCursor (ks, ksGetSize (ks) - 1);

	for (elektraCursor cursor = ksGetCursor (ks) - 1; (current = ksAtCursor (ks, cursor)) != NULL; --cursor)
	{
		if (keyIsBelow (current, key))
		{
			return current;
		}
	}

	return 0;
}

/*
 * Appends a line to the MetaKey of the supplied Key
 * If no MetaKey with the given name exists yet, a new
 * one is created containing the supplied line. If
 * the MetaKey exists, the supplied line is added as
 * a new line to the value of the MetaKey (i.e. a newline
 * followed by the given line is appended to the metadata)
 *
 * @param	target the Key whose MetaKey is to be modified
 * @param	metaName the name of the MetaKey which is to be modified
 * @param	line the line to be appended to the matadata
 * @return	the new value size of the modified MetaKey
 * @retval 	-1 on NULL pointers or if a memory allocation error occurs
 *
 * @see keyGetValueSize(Key *key)
 *
 */
int elektraKeyAppendMetaLine (ElektraKey * target, const char * metaName, const char * line)
{
	if (!target) return 0;
	if (!metaName) return 0;
	if (!line) return 0;

	if (!keyGetMeta (target, metaName))
	{
		keySetMeta (target, metaName, line);
		return keyGetValueSize (keyGetMeta (target, metaName));
	}

	const ElektraKey * existingMeta = keyGetMeta (target, metaName);
	char * buffer = elektraMalloc (keyGetValueSize (existingMeta) + strlen (line) + 1);
	if (!buffer) return 0;

	keyGetString (existingMeta, buffer, keyGetValueSize (existingMeta));
	strcat (buffer, "\n");
	strncat (buffer, line, elektraStrLen (line));

	keySetMeta (target, metaName, buffer);
	elektraFree (buffer);
	return keyGetValueSize (keyGetMeta (target, metaName));
}

static const char * getAppendMode (ElektraKey * key)
{
	const ElektraKey * appendModeKey = keyGetMeta (key, CONVERT_APPENDMODE);
	const char * appendMode;

	/* append to the next key is the default */
	appendMode = appendModeKey != 0 ? keyString (appendModeKey) : "next";
	return appendMode;
}

void removeKeyFromResult (ElektraKey * convertKey, ElektraKey * target, ElektraKeyset * orig)
{
	/* remember which key this key was converted to
	 * before removing it from the result
	 */
	keySetMeta (convertKey, CONVERT_TARGET, keyName (target));
	keyDel (ksLookup (orig, convertKey, KDB_O_POP));
}

static void flushConvertedKeys (ElektraKey * target, ElektraKeyset * converted, ElektraKeyset * orig)
{
	if (ksGetSize (converted) == 0) return;

	ksRewind (converted);
	ElektraKey * current;

	while ((current = ksNext (converted)))
	{
		ElektraKey * appendTarget = target;
		const char * metaName = keyString (keyGetMeta (current, CONVERT_METANAME));

		ElektraKey * currentDup = keyDup (current, KEY_CP_ALL);
		ElektraKey * targetDup = keyDup (appendTarget, KEY_CP_ALL);
		keySetBaseName (currentDup, 0);
		keySetBaseName (targetDup, 0);

		/* the convert key request to be converted to a key
		 * on the same level, but the target is below or above
		 */
		if (keyGetMeta (current, CONVERT_APPEND_SAMELEVEL) && keyCmp (currentDup, targetDup))
		{
			appendTarget = 0;
		}

		keyDel (currentDup);
		keyDel (targetDup);

		/* no target key was found of the target
		 * was discarded for some reason. Revert to the parent
		 */
		if (!appendTarget)
		{
			appendTarget = findNearestParent (current, orig);
		}

		elektraKeyAppendMetaLine (appendTarget, metaName, keyString (current));
		removeKeyFromResult (current, target, orig);
	}

	ksClear (converted);
}

static ElektraKeyset * convertKeys (ElektraKey ** keyArray, size_t numKeys, ElektraKeyset * orig)
{
	ElektraKey * current = 0;
	ElektraKey * prevAppendTarget = 0;
	ElektraKeyset * prevConverted = ksNew (0, KS_END);
	ElektraKeyset * nextConverted = ksNew (0, KS_END);
	ElektraKeyset * result = ksNew (0, KS_END);

	for (size_t index = 0; index < numKeys; index++)
	{
		current = keyArray[index];

		if (!keyGetMeta (current, CONVERT_METANAME))
		{
			/* flush out "previous" and "next" keys which may have been collected
			 * because the current key serves as a new border
			 */
			ksAppend (result, prevConverted);
			flushConvertedKeys (prevAppendTarget, prevConverted, orig);
			prevAppendTarget = current;

			ksAppend (result, nextConverted);
			flushConvertedKeys (current, nextConverted, orig);
			continue;
		}

		const char * appendMode = getAppendMode (current);
		const char * metaName = keyString (keyGetMeta (current, CONVERT_METANAME));

		ElektraKey * bufferKey = 0;
		if (!strcmp (appendMode, "previous"))
		{
			ksAppendKey (prevConverted, current);
		}

		if (!strcmp (appendMode, "next"))
		{
			ksAppendKey (nextConverted, current);
		}

		if (!strcmp (appendMode, "parent"))
		{
			ElektraKey * parent = findNearestParent (current, orig);
			elektraKeyAppendMetaLine (parent, metaName, keyString (current));
			ksAppendKey (result, current);
			removeKeyFromResult (current, parent, orig);
		}

		if (bufferKey)
		{
			keySetString (bufferKey, keyName (current));
		}
	}

	ksAppend (result, prevConverted);
	flushConvertedKeys (prevAppendTarget, prevConverted, orig);

	ksAppend (result, nextConverted);
	flushConvertedKeys (0, nextConverted, orig);

	ksDel (nextConverted);
	ksDel (prevConverted);

	return result;
}

int elektraKeyToMetaGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	int errnosave = errno;

	/* configuration only */
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/keytometa"))
	{
		ElektraKeyset * info =
#include "contract.h"

			ksAppend (returned, info);
		ksDel (info);
		return 1;
	}

	ElektraKey ** keyArray = calloc (ksGetSize (returned), sizeof (ElektraKey *));
	int ret = elektraKsToMemArray (returned, keyArray);

	if (ret < 0)
	{
		elektraFree (keyArray);
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
		errno = errnosave;
		return 0;
	}

	size_t numKeys = ksGetSize (returned);
	qsort (keyArray, numKeys, sizeof (ElektraKey *), elektraKeyCmpOrderWrapper);

	ElektraKeyset * convertedKeys = convertKeys (keyArray, numKeys, returned);

	elektraFree (keyArray);

	/* cleanup what might have been left from a previous call */
	ElektraKeyset * old = elektraPluginGetData (handle);
	if (old)
	{
		ksDel (old);
	}

	elektraPluginSetData (handle, convertedKeys);

	errno = errnosave;
	return 1; /* success */
}


int elektraKeyToMetaSet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	ElektraKeyset * converted = elektraPluginGetData (handle);

	/* nothing to do */
	if (converted == 0) return 1;

	ksRewind (converted);

	char * saveptr = 0;
	char * value = 0;
	ElektraKey * current;
	ElektraKey * previous = 0;
	while ((current = ksNext (converted)) != 0)
	{
		const ElektraKey * targetName = keyGetMeta (current, CONVERT_TARGET);
		const ElektraKey * metaName = keyGetMeta (current, CONVERT_METANAME);

		/* they should always exist, just to be sure */
		if (targetName && metaName)
		{
			ElektraKey * target = ksLookupByName (returned, keyString (targetName), KDB_O_NONE);

			/* this might be NULL as the key might have been deleted */
			if (target)
			{

				char * result = 0;
				if (target != previous)
				{
					/* handle the first meta line this means initializing strtok and related buffers */
					elektraFree (value);
					const ElektraKey * valueKey = keyGetMeta (target, keyString (metaName));
					size_t valueSize = keyGetValueSize (valueKey);
					value = elektraMalloc (valueSize);
					keyGetString (valueKey, value, valueSize);
					keySetMeta (target, keyString (metaName), 0);
					result = strtok_r (value, "\n", &saveptr);
				}
				else
				{
					/* just continue splitting the metadata */
					result = strtok_r (NULL, "\n", &saveptr);
				}

				keySetString (current, result);

				previous = target;
			}
		}

		keySetMeta (current, CONVERT_TARGET, 0);
		keySetMeta (current, CONVERT_METANAME, 0);

		ksAppendKey (returned, current);
	}

	elektraFree (value);

	ksDel (converted);
	elektraPluginSetData (handle, 0);

	return 1; /* success */
}

int elektraKeyToMetaClose (Plugin * handle, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	ElektraKeyset * old = elektraPluginGetData (handle);

	if (old)
	{
		ksDel (old);
	}

	return 1;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("keytometa",
		ELEKTRA_PLUGIN_GET,	&elektraKeyToMetaGet,
		ELEKTRA_PLUGIN_SET,	&elektraKeyToMetaSet,
		ELEKTRA_PLUGIN_CLOSE, &elektraKeyToMetaClose,
		ELEKTRA_PLUGIN_END);
}

