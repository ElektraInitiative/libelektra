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
	if (orderResult == 0) return elektraKeyCmp (*ka, *kb);

	return orderResult;
}

/* The KeySet MUST be sorted alphabetically (or at least ascending
 * by the length of keynames) for this function to work
 */
static ElektraKey * findNearestParent (ElektraKey * key, ElektraKeyset * ks)
{
	ElektraKey * current;
	elektraKeysetSetCursor (ks, elektraKeysetGetSize (ks) - 1);

	for (elektraCursor cursor = elektraKeysetGetCursor (ks) - 1; (current = elektraKeysetAtCursor (ks, cursor)) != NULL; --cursor)
	{
		if (elektraKeyIsBelow (current, key))
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

	if (!elektraKeyGetMeta (target, metaName))
	{
		elektraKeySetMeta (target, metaName, line);
		return elektraKeyGetValueSize (elektraKeyGetMeta (target, metaName));
	}

	const ElektraKey * existingMeta = elektraKeyGetMeta (target, metaName);
	char * buffer = elektraMalloc (elektraKeyGetValueSize (existingMeta) + strlen (line) + 1);
	if (!buffer) return 0;

	elektraKeyGetString (existingMeta, buffer, elektraKeyGetValueSize (existingMeta));
	strcat (buffer, "\n");
	strncat (buffer, line, elektraStrLen (line));

	elektraKeySetMeta (target, metaName, buffer);
	elektraFree (buffer);
	return elektraKeyGetValueSize (elektraKeyGetMeta (target, metaName));
}

static const char * getAppendMode (ElektraKey * key)
{
	const ElektraKey * appendModeKey = elektraKeyGetMeta (key, CONVERT_APPENDMODE);
	const char * appendMode;

	/* append to the next key is the default */
	appendMode = appendModeKey != 0 ? elektraKeyString (appendModeKey) : "next";
	return appendMode;
}

void removeKeyFromResult (ElektraKey * convertKey, ElektraKey * target, ElektraKeyset * orig)
{
	/* remember which key this key was converted to
	 * before removing it from the result
	 */
	elektraKeySetMeta (convertKey, CONVERT_TARGET, elektraKeyName (target));
	elektraKeyDel (elektraKeysetLookup (orig, convertKey, ELEKTRA_KDB_O_POP));
}

static void flushConvertedKeys (ElektraKey * target, ElektraKeyset * converted, ElektraKeyset * orig)
{
	if (elektraKeysetGetSize (converted) == 0) return;

	elektraKeysetRewind (converted);
	ElektraKey * current;

	while ((current = elektraKeysetNext (converted)))
	{
		ElektraKey * appendTarget = target;
		const char * metaName = elektraKeyString (elektraKeyGetMeta (current, CONVERT_METANAME));

		ElektraKey * currentDup = elektraKeyDup (current, ELEKTRA_KEY_CP_ALL);
		ElektraKey * targetDup = elektraKeyDup (appendTarget, ELEKTRA_KEY_CP_ALL);
		elektraKeySetBaseName (currentDup, 0);
		elektraKeySetBaseName (targetDup, 0);

		/* the convert key request to be converted to a key
		 * on the same level, but the target is below or above
		 */
		if (elektraKeyGetMeta (current, CONVERT_APPEND_SAMELEVEL) && elektraKeyCmp (currentDup, targetDup))
		{
			appendTarget = 0;
		}

		elektraKeyDel (currentDup);
		elektraKeyDel (targetDup);

		/* no target key was found of the target
		 * was discarded for some reason. Revert to the parent
		 */
		if (!appendTarget)
		{
			appendTarget = findNearestParent (current, orig);
		}

		elektraKeyAppendMetaLine (appendTarget, metaName, elektraKeyString (current));
		removeKeyFromResult (current, target, orig);
	}

	elektraKeysetClear (converted);
}

static ElektraKeyset * convertKeys (ElektraKey ** keyArray, size_t numKeys, ElektraKeyset * orig)
{
	ElektraKey * current = 0;
	ElektraKey * prevAppendTarget = 0;
	ElektraKeyset * prevConverted = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * nextConverted = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * result = elektraKeysetNew (0, ELEKTRA_KS_END);

	for (size_t index = 0; index < numKeys; index++)
	{
		current = keyArray[index];

		if (!elektraKeyGetMeta (current, CONVERT_METANAME))
		{
			/* flush out "previous" and "next" keys which may have been collected
			 * because the current key serves as a new border
			 */
			elektraKeysetAppend (result, prevConverted);
			flushConvertedKeys (prevAppendTarget, prevConverted, orig);
			prevAppendTarget = current;

			elektraKeysetAppend (result, nextConverted);
			flushConvertedKeys (current, nextConverted, orig);
			continue;
		}

		const char * appendMode = getAppendMode (current);
		const char * metaName = elektraKeyString (elektraKeyGetMeta (current, CONVERT_METANAME));

		ElektraKey * bufferKey = 0;
		if (!strcmp (appendMode, "previous"))
		{
			elektraKeysetAppendKey (prevConverted, current);
		}

		if (!strcmp (appendMode, "next"))
		{
			elektraKeysetAppendKey (nextConverted, current);
		}

		if (!strcmp (appendMode, "parent"))
		{
			ElektraKey * parent = findNearestParent (current, orig);
			elektraKeyAppendMetaLine (parent, metaName, elektraKeyString (current));
			elektraKeysetAppendKey (result, current);
			removeKeyFromResult (current, parent, orig);
		}

		if (bufferKey)
		{
			elektraKeySetString (bufferKey, elektraKeyName (current));
		}
	}

	elektraKeysetAppend (result, prevConverted);
	flushConvertedKeys (prevAppendTarget, prevConverted, orig);

	elektraKeysetAppend (result, nextConverted);
	flushConvertedKeys (0, nextConverted, orig);

	elektraKeysetDel (nextConverted);
	elektraKeysetDel (prevConverted);

	return result;
}

int elektraKeyToMetaGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	int errnosave = errno;

	/* configuration only */
	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/keytometa"))
	{
		ElektraKeyset * info =
#include "contract.h"

			elektraKeysetAppend (returned, info);
		elektraKeysetDel (info);
		return 1;
	}

	ElektraKey ** keyArray = calloc (elektraKeysetGetSize (returned), sizeof (ElektraKey *));
	int ret = elektraKsToMemArray (returned, keyArray);

	if (ret < 0)
	{
		elektraFree (keyArray);
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
		errno = errnosave;
		return 0;
	}

	size_t numKeys = elektraKeysetGetSize (returned);
	qsort (keyArray, numKeys, sizeof (ElektraKey *), elektraKeyCmpOrderWrapper);

	ElektraKeyset * convertedKeys = convertKeys (keyArray, numKeys, returned);

	elektraFree (keyArray);

	/* cleanup what might have been left from a previous call */
	ElektraKeyset * old = elektraPluginGetData (handle);
	if (old)
	{
		elektraKeysetDel (old);
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

	elektraKeysetRewind (converted);

	char * saveptr = 0;
	char * value = 0;
	ElektraKey * current;
	ElektraKey * previous = 0;
	while ((current = elektraKeysetNext (converted)) != 0)
	{
		const ElektraKey * targetName = elektraKeyGetMeta (current, CONVERT_TARGET);
		const ElektraKey * metaName = elektraKeyGetMeta (current, CONVERT_METANAME);

		/* they should always exist, just to be sure */
		if (targetName && metaName)
		{
			ElektraKey * target = elektraKeysetLookupByName (returned, elektraKeyString (targetName), ELEKTRA_KDB_O_NONE);

			/* this might be NULL as the key might have been deleted */
			if (target)
			{

				char * result = 0;
				if (target != previous)
				{
					/* handle the first meta line this means initializing strtok and related buffers */
					elektraFree (value);
					const ElektraKey * valueKey = elektraKeyGetMeta (target, elektraKeyString (metaName));
					size_t valueSize = elektraKeyGetValueSize (valueKey);
					value = elektraMalloc (valueSize);
					elektraKeyGetString (valueKey, value, valueSize);
					elektraKeySetMeta (target, elektraKeyString (metaName), 0);
					result = strtok_r (value, "\n", &saveptr);
				}
				else
				{
					/* just continue splitting the metadata */
					result = strtok_r (NULL, "\n", &saveptr);
				}

				elektraKeySetString (current, result);

				previous = target;
			}
		}

		elektraKeySetMeta (current, CONVERT_TARGET, 0);
		elektraKeySetMeta (current, CONVERT_METANAME, 0);

		elektraKeysetAppendKey (returned, current);
	}

	elektraFree (value);

	elektraKeysetDel (converted);
	elektraPluginSetData (handle, 0);

	return 1; /* success */
}

int elektraKeyToMetaClose (Plugin * handle, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	ElektraKeyset * old = elektraPluginGetData (handle);

	if (old)
	{
		elektraKeysetDel (old);
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

