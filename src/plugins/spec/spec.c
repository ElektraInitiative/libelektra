/**
 * @file
 *
 * @brief Source for spec plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "spec.h"
#include <fnmatch.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <kdbmeta.h>
#include <kdbtypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_CHARS_IN_LONG 26

typedef enum
{
	IGNORE,
	ERROR,
	WARNING,
	INFO
} OnConflict;


// clang-format off
#define NO_CONFLICTS         "00000" // on char per conflict type below
#define CONFLICT_ARRAYMEMBER 0
#define CONFLICT_INVALID     1
#define CONFLICT_SUBCOUNT    2
#define CONFLICT_COLLISION   3
#define CONFLICT_OUTOFRANGE  4
// missing keys are handled directly
#define SIZE_CONFLICTS       sizeof(NO_CONFLICTS)
// clang-format on


typedef struct
{
	OnConflict member;
	OnConflict invalid;
	OnConflict count;
	OnConflict conflict;
	OnConflict range;
	OnConflict missing;
} ConflictHandling;

typedef struct
{
	KeySet * ks;
	int counter;
} PluginData;

static inline void safeFree (void * ptr)
{
	if (ptr != NULL)
	{
		elektraFree (ptr);
	}
}

/* region Key Globbing       */
/* ========================= */

// elektra wildcard syntax to fnmatch syntax
static char * keyNameToMatchingString (const Key * key)
{
	uint8_t arrayCount = 0;
	char * name = strchr (keyName (key), '/');
	if (!name) return elektraStrDup (keyName (key));
	for (char * ptr = name; *ptr != '\0'; ++ptr)
		if (*ptr == '#') ++arrayCount;
	char * pattern = elektraMalloc (elektraStrLen (name) + arrayCount);
	char * dst = pattern;
	for (char * src = (name + 1); *src != '\0'; ++src)
	{
		if (*src == '_' && *(src - 1) == '/' && (*(src + 1) == '/' || *(src + 1) == '\0'))
		{
			*dst++ = '*';
		}
		else if (*src == '#' && *(src - 1) == '/' && (*(src + 1) == '/' || *(src + 1) == '\0'))
		{
			*dst++ = '#';
			*dst++ = '*';
		}
		else
		{
			*dst++ = *src;
		}
	}
	*dst = '\0';
	return pattern;
}


// match pattern against keyname - ignore namespace
// @retval true if keyname matches pattern
// @retval false if keyname doesn't match pattern
static int matchPatternToKey (const char * pattern, const Key * key)
{
	const char * ptr = strchr (keyName (key), '/');
	if (ptr)
		return !fnmatch (pattern, ++ptr, FNM_PATHNAME);
	else
		return FNM_NOMATCH;
}

// endregion Globbing

/*  region Config parsing    */
/* ========================= */

static OnConflict parseOnConflictKey (const Key * key)
{
	const char * string = keyString (key);
	if (strcmp (string, "ERROR") == 0)
	{
		return ERROR;
	}
	else if (strcmp (string, "WARNING") == 0)
	{
		return WARNING;
	}
	else if (strcmp (string, "INFO") == 0)
	{
		return INFO;
	}
	else
	{
		return IGNORE;
	}
}

static void parseConfig (KeySet * config, ConflictHandling * ch, const char baseName[20])
{
	char nameBuffer[32];
	strcpy (nameBuffer, baseName);
	char * nameBufferEnd = nameBuffer + strlen (nameBuffer);

	Key * key = ksLookupByName (config, nameBuffer, 0);
	OnConflict base = parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/member");
	key = ksLookupByName (config, nameBuffer, 0);
	ch->member = key == NULL ? base : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/invalid");
	key = ksLookupByName (config, nameBuffer, 0);
	ch->invalid = key == NULL ? base : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/count");
	key = ksLookupByName (config, nameBuffer, 0);
	ch->count = key == NULL ? base : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/collision");
	key = ksLookupByName (config, nameBuffer, 0);
	ch->conflict = key == NULL ? base : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/range");
	key = ksLookupByName (config, nameBuffer, 0);
	ch->range = key == NULL ? base : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/missing");
	key = ksLookupByName (config, nameBuffer, 0);
	ch->missing = key == NULL ? base : parseOnConflictKey (key);
}

static void parseLocalConfig (Key * specKey, ConflictHandling * ch, const char baseName[20])
{
	char nameBuffer[32];
	strcpy (nameBuffer, baseName);
	char * nameBufferEnd = nameBuffer + strlen (nameBuffer);

	strcpy (nameBufferEnd, "/member");
	const Key * key = keyGetMeta (specKey, nameBuffer);
	ch->member = key == NULL ? ch->member : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/invalid");
	key = keyGetMeta (specKey, nameBuffer);
	ch->invalid = key == NULL ? ch->invalid : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/count");
	key = keyGetMeta (specKey, nameBuffer);
	ch->count = key == NULL ? ch->count : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/collision");
	key = keyGetMeta (specKey, nameBuffer);
	ch->conflict = key == NULL ? ch->conflict : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/range");
	key = keyGetMeta (specKey, nameBuffer);
	ch->range = key == NULL ? ch->range : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/missing");
	key = keyGetMeta (specKey, nameBuffer);
	ch->missing = key == NULL ? ch->missing : parseOnConflictKey (key);
}

// endregion Config parsing

/* region Conflict handling  */
/* ========================= */

static void addConflict (Key * key, int conflict)
{
	char conflicts[SIZE_CONFLICTS] = NO_CONFLICTS;
	keyGetString (key, conflicts, SIZE_CONFLICTS);
	conflicts[conflict] = '1';
	keySetMeta (key, "conflict", conflicts);
}

/**
 * Handle a single conflict.
 *
 * @param parentKey   The parent key (to store errors and warnings).
 * @param msg         The conflict message
 * @param onConflict  What to do with the conflict
 */
static void handleConflict (Key * parentKey, const char * msg, OnConflict onConflict)
{
	switch (onConflict)
	{
	case ERROR:
		ELEKTRA_SET_ERRORF (142, parentKey, "%s", msg);
		break;
	case WARNING:
		ELEKTRA_ADD_WARNINGF (143, parentKey, "%s", msg);
		break;
	case INFO:
		elektraMetaArrayAdd (parentKey, "logs/spec/info", msg);
		break;
	case IGNORE:
	default:
		break;
	}
}

/**
 * Handles all conflicts for the given key.
 *
 * @param key       The key whose conflicts we handle.
 * @param parentKey The parent key (for storing errors/warnings)
 * @param specKey   The spec Key causing the conflict (for additional information, e.g. max array size)
 * @param ch        How conflicts should be handled
 *
 * @retval  0 if no conflicts where found
 * @retval -1 otherwise
 */
static int handleConflicts (Key * key, Key * parentKey, Key * specKey, const ConflictHandling * ch)
{
	const Key * metaKey = keyGetMeta (key, "conflict");

	if (!metaKey)
	{
		return 0;
	}

	char conflicts[SIZE_CONFLICTS] = NO_CONFLICTS;
	keyGetString (key, conflicts, SIZE_CONFLICTS);

	if (conflicts[CONFLICT_INVALID] == '1' && ch->invalid != IGNORE)
	{
		char * msg = elektraFormat ("Invalidkey %s", keyName (key));
		handleConflict (parentKey, msg, ch->invalid);
		elektraFree (msg);
	}

	if (conflicts[CONFLICT_ARRAYMEMBER] == '1' && ch->member != IGNORE)
	{
		char * problemKeys = elektraMetaArrayToString (key, keyName (keyGetMeta (key, "conflict/arraymember")), ", ");
		char * msg = elektraFormat ("%s has invalid array key members: %s", keyName (key), problemKeys);
		handleConflict (parentKey, msg, ch->member);
		elektraFree (msg);
		safeFree (problemKeys);
	}

	if (conflicts[CONFLICT_SUBCOUNT] == '1' && ch->count != IGNORE)
	{
		Key * parent = keyNew (keyName (key), KEY_END);
		keySetBaseName (parent, NULL);
		char * msg = elektraFormat ("%s has invalid number of subkeys: %s. Expected: %s", keyName (parent),
					    keyString (keyGetMeta (key, "conflict/subcount")),
					    keyString (keyGetMeta (specKey, "require/count")));
		handleConflict (parentKey, msg, ch->count);
		elektraFree (msg);
		keyDel (parent);
	}

	if (conflicts[CONFLICT_COLLISION] == '1' && ch->conflict != IGNORE)
	{
		char * problemKeys = elektraMetaArrayToString (key, keyName (keyGetMeta (key, "conflict/collision")), ", ");
		char * msg = elektraFormat ("%s has conflicting metakeys: %s", keyName (key), problemKeys);
		handleConflict (parentKey, msg, ch->conflict);
		elektraFree (msg);
		safeFree (problemKeys);
	}

	if (conflicts[CONFLICT_OUTOFRANGE] == '1' && ch->range != IGNORE)
	{
		char * msg = elektraFormat ("%s has invalid number of members: %s. Expected: %s", keyName (key),
					    keyString (keyGetMeta (key, "conflict/outofrange")), keyString (keyGetMeta (specKey, "array")));
		handleConflict (parentKey, msg, ch->range);
		elektraFree (msg);
	}

	return -1;
}

/**
 * Handles all errors (conflicts) for the given key, including those stored in immediate parent.
 *
 * @param key            The key whose conflicts we handle.
 * @param parentKey      The parent key (for storing errors/warnings)
 * @param ks             The full KeySet
 * @param specKey        The spec Key causing the conflict (for additional information, e.g. max array size)
 * @param ch             How conflicts should be handled
 * @param configBaseName "/conflict/get" for kdbGet, "/conflict/set" for kdbSet
 *
 * @retval  0 if no conflicts where found
 * @retval -1 otherwise
 */
static int handleErrors (Key * key, Key * parentKey, KeySet * ks, Key * specKey, const ConflictHandling * ch, const char configBaseName[20])
{
	ConflictHandling localCh;
	memcpy (&localCh, ch, sizeof (ConflictHandling));

	parseLocalConfig (specKey, &localCh, configBaseName);

	Key * parentLookup = keyDup (key);
	keySetBaseName (parentLookup, NULL);

	cursor_t cursor = ksGetCursor (ks);
	Key * parent = ksLookup (ks, parentLookup, KDB_O_NONE);
	ksSetCursor (ks, cursor);

	keyDel (parentLookup);

	int ret = handleConflicts (parent, parentKey, specKey, &localCh) || handleConflicts (key, parentKey, specKey, &localCh);

	return ret;
}

// endregion Conflict handling

/**
 * Copies all metadata (except for internal/ and conflict/) from @p dest to @p src
 */
static void copyMeta (Key * dest, Key * src)
{
	keyRewindMeta (src);
	const Key * meta;
	while ((meta = keyNextMeta (src)) != NULL)
	{
		const char * name = keyName (meta);
		if (strncmp (name, "internal/", 9) != 0 && strncmp (name, "conflict/", 9) != 0)
		{
			const Key * oldMeta = keyGetMeta (dest, name);
			if (oldMeta != NULL && strcmp (keyString (oldMeta), keyString (meta)) != 0)
			{
				char * conflictName = elektraFormat ("conflict/%s", name);
				keySetMeta (dest, conflictName, keyString (oldMeta));
				elektraFree (conflictName);
				addConflict (dest, CONFLICT_COLLISION);
				elektraMetaArrayAdd (dest, "conflict/collision", name);
			}

			keyCopyMeta (dest, src, name);
		}
	}
}

/* region Array handling     */
/* ========================= */

/**
 * Checks whether the given key is an array spec,
 * i.e. it has a keyname part that is "#".
 *
 * @param key a spec key
 *
 * @retval #true  if @p key is an array spec
 * @retval #false otherwise
 */
static bool isArraySpec (const Key * key)
{
	size_t usize = keyGetUnescapedNameSize (key);
	const char * cur = keyUnescapedName (key);
	const char * end = cur + usize;

	while (cur < end)
	{
		size_t len = strlen (cur);

		if (len == 1 && cur[0] == '#')
		{
			return true;
		}

		cur += len;
	}

	return false;
}

static bool validateArraySize (Key * arrayParent, const char * arrayMin, const char * arrayMax)
{
	const Key * arrayActualKey = keyGetMeta (arrayParent, "array");
	const char * arrayActual = arrayActualKey == NULL ? "" : keyString (arrayActualKey);

	return strcmp (arrayMin, arrayActual) <= 0 && 0 <= strcmp (arrayActual, arrayMax);
}

static void instantiateArraySpec (KeySet * ks, Key * arraySpec);

// instantiates the array specified by arraySpec, e.g. abc/#
static void instantiateArray (KeySet * ks, Key * arraySpec, const char * callWithSuffix)
{
	Key * arrayParent = keyNew (strchr (keyName (arraySpec), '/'), KEY_END);
	keySetBaseName (arrayParent, 0);

	Key * parent = ksLookup (ks, arrayParent, 0);
	const Key * arrayKey = keyGetMeta (parent, "array");
	const char * arraySize = arrayKey == NULL ? "" : keyString (arrayKey);

	const Key * arrayMinKey = keyGetMeta (arrayParent, "array/min");
	const char * arrayMin = arrayMinKey == NULL ? "" : keyString (arrayMinKey);

	const Key * arrayMaxKey = keyGetMeta (arrayParent, "array/max");
	const char * arrayMax = arrayMaxKey == NULL ? "$" : keyString (arrayMaxKey); // $ > #, so "$" is bigger than array size

	if (!validateArraySize (parent, arrayMin, arrayMax))
	{
		// TODO: conflict range
	}

	kdb_long_long_t i = 0;
	char buf[ELEKTRA_MAX_ARRAY_SIZE];
	elektraWriteArrayNumber (buf, i);
	while (strcmp (buf, arraySize) <= 0)
	{
		Key * newKey = keyNew (keyName (arrayParent), KEY_CASCADING_NAME, KEY_END);
		keyAddBaseName (newKey, buf);
		copyMeta (newKey, arraySpec);
		ksAppendKey (ks, newKey);

		if (callWithSuffix != NULL)
		{
			Key * newArraySpec = keyDup (arraySpec);
			keySetBaseName (newArraySpec, buf);

			instantiateArraySpec (ks, newArraySpec);
		}

		++i;
		elektraWriteArrayNumber (buf, i);
	}

	keyDel (arrayParent);
}

// instantiates all array spec parts in an array spec key (e.g. abc/#/a/d/#/e)
static void instantiateArraySpec (KeySet * ks, Key * arraySpec)
{
	// FIXME: doesn't work with embedded slashes, needs keyEscape to rebuild key from unescaped version

	const char * spec = keyName (arraySpec);
	char * arrayPart = strstr (spec, "/#/");
	if (arrayPart != NULL)
	{
		char * newSpec = elektraStrDup (spec);
		newSpec[arrayPart - spec] = '\0';

		Key * newArraySpec = keyDup (arraySpec);
		keySetName (newArraySpec, newSpec);
		elektraFree (newSpec);

		char * suffix = arrayPart + 2;

		instantiateArray (ks, newArraySpec, suffix);
	}
	else if (strcmp (keyBaseName (arraySpec), "#") == 0)
	{
		instantiateArray (ks, arraySpec, NULL);
	}
}

static void validateArray (KeySet * ks, Key * arrayKey, Key * specKey)
{
	Key * arrayParent = keyDup (arrayKey);

	// TODO: [improvement] ksExtract?, like ksCut, but doesn't remove -> no need for ksDup
	KeySet * ksCopy = ksDup (ks);
	KeySet * subKeys = ksCut (ksCopy, arrayParent);

	Key * cur;
	while ((cur = ksNext (subKeys)) != NULL)
	{
		if (!keyIsDirectBelow (arrayParent, cur)) continue;
		if (keyBaseName (cur)[0] == '#')
		{
			if (elektraArrayValidateName (cur) < 0)
			{
				KeySet * invalidCutKS = ksCut (subKeys, cur);
				Key * toMark;
				while ((toMark = ksNext (invalidCutKS)) != NULL)
				{
					if (strcmp (keyName (cur), keyName (toMark)) != 0)
					{
						keySetMeta (toMark, "conflict/invalid", ""); // TODO ????, should skip later on
					}
					elektraMetaArrayAdd (arrayParent, "conflict/invalid/hasmember", keyName (toMark));
				}
				ksDel (invalidCutKS);
			}
		}
	}
	ksDel (subKeys);
	ksDel (ksCopy);
}

// endregion Array handling

/* region Wildcard (_) handling              */
/* ========================================= */

/**
 * Checks whether the given key is a wildcard spec,
 * i.e. it has a keyname part that is "_".
 *
 * @param key a spec key
 *
 * @retval #true  if @p key is a wildcard spec
 * @retval #false otherwise
 */
static bool isWildcardSpec (const Key * key)
{
	// FIXME: use code similar to isArraySpec, once validateWildcardSubs works for all wildcard specs

	return strcmp (keyBaseName (key), "_") == 0;
}

/**
 * Handles wildcard spec keys. A conflict will be added,
 * if the number of keys directly below
 * @param ks
 * @param key
 * @param specKey
 */
static void validateWildcardSubs (KeySet * ks, Key * key, Key * specKey)
{
	// FIXME: only works if '_' at end
	if (keyGetMeta (specKey, "require") != NULL)
	{
		return;
	}

	const Key * requireCount = keyGetMeta (specKey, "require/count");
	if (requireCount != NULL)
	{
		Key * parent = keyDup (key);
		keySetBaseName (parent, NULL);

		// TODO: [improvement] ksExtract?, like ksCut, but doesn't remove -> no need for ksDup
		KeySet * ksCopy = ksDup (ks);
		KeySet * subKeys = ksCut (ksCopy, parent);
		ksDel (ksCopy);

		Key * cur;
		long subCount = 0;
		while ((cur = ksNext (subKeys)) != NULL)
		{
			// FIXME: works with cascading? else need lookup?
			if (keyIsDirectBelow (parent, cur))
			{
				++subCount;
			}
		}
		ksDel (subKeys);

		// TODO: conversion library?

		long required = ELEKTRA_LONG_LONG_S (keyString (requireCount), NULL, 10);
		if (required != subCount)
		{
			char buffer[MAX_CHARS_IN_LONG + 1];
			snprintf (buffer, sizeof (buffer), "%ld", subCount);
			addConflict (key, CONFLICT_SUBCOUNT);
			keySetMeta (key, "conflict/subcount", buffer);
		}
	}
}

// endregion Wildcard (_) handling

static void removeMeta (Key * key, Key * specKey)
{ // TODO: correct??
	keyRewindMeta (specKey);
	const Key * meta;
	while ((meta = keyNextMeta (specKey)) != NULL)
	{
		const char * name = keyName (meta);
		if (strcmp (name, "array") != 0 && strcmp (name, "required") != 0 && strncmp (name, "conflict/", 9) != 0 &&
		    strcmp (name, "require") != 0)
		{
			keySetMeta (key, name, NULL);
		}
	}
}


static void cleanSpecKey (Key * specKey, KeySet * ks)
{ // TODO: what does it do? why does it exist? is it correct??
	char * pattern = keyNameToMatchingString (specKey);

	ksRewind (ks);
	Key * cur;
	while ((cur = ksNext (ks)) != NULL)
	{
		if (keyGetNamespace (cur) != KEY_NS_SPEC && matchPatternToKey (pattern, cur) != 0)
		{
			removeMeta (cur, specKey);
		}
	}

	elektraFree (pattern);
}

/**
 * Process exactly one key of the specification.
 *
 * @param specKey        The spec Key to process.
 * @param parentKey      The parent key (for errors)
 * @param ks	         The full KeySet
 * @param ch             How should conflicts be handled?
 * @param configBaseName "/conflict/get" for kdbGet, "/conflict/set" for kdbSet
 * @param isKdbGet       true in kdbGet, false otherwise
 *
 * @retval  0 on success
 * @retval -1 otherwise
 */
static int processSpecKey (Key * specKey, Key * parentKey, KeySet * ks, const ConflictHandling * ch, const char configBaseName[20],
			   bool isKdbGet)
{
	// TODO: use libglobbing
	char * pattern = keyNameToMatchingString (specKey);
	bool arraySpec = isArraySpec (specKey);
	bool wildcardSpec = isWildcardSpec (specKey);

	int found = 0;
	Key * cur;

	ksRewind (ks);
	ksNext (ks); // set cursor to first

	// externalize cursor to avoid having to reset after ksLookups
	cursor_t cursor = ksGetCursor (ks);
	for (; (cur = ksAtCursor (ks, cursor)) != NULL; ++cursor)
	{
		if (!matchPatternToKey (pattern, cur))
		{
			continue;
		}

		found = 1;

		if (arraySpec)
		{
			validateArray (ks, cur, specKey);
		}

		if (wildcardSpec)
		{
			validateWildcardSubs (ks, cur, specKey);
		}

		copyMeta (cur, specKey);
	}

	elektraFree (pattern);

	if (keyGetMeta (specKey, "require") != NULL)
	{
		// TODO: require + array
		char * msg = elektraFormat ("Required key %s is missing.", strchr (keyName (specKey), '/'));
		// pass parentKey as key, so we store the info message there,
		// because key is missing in KeySet
		handleConflict (parentKey, msg, ch->missing);
		elektraFree (msg);
	}

	if (!found && isKdbGet)
	{
		if (keyGetMeta (specKey, "assign/condition") != NULL)
		{
			Key * newKey = keyNew (strchr (keyName (specKey), '/'), KEY_CASCADING_NAME, KEY_END);
			keySetMeta (newKey, "assign/condition", keyString (keyGetMeta (specKey, "assign/condition")));
			ksAppendKey (ks, newKey);
		}
		else if (keyGetMeta (specKey, "default") != NULL)
		{
			Key * newKey = keyNew (strchr (keyName (specKey), '/'), KEY_CASCADING_NAME, KEY_VALUE,
					       keyString (keyGetMeta (specKey, "default")), KEY_END);
			copyMeta (newKey, specKey);
			ksAppendKey (ks, newKey);
		}
	}

	int ret = 0;
	while ((cur = ksNext (ks)) != NULL)
	{
		if (handleErrors (cur, parentKey, ks, specKey, ch, configBaseName) != 0)
		{
			ret = -1;
		}

		keySetMeta (cur, "conflict/invalid", NULL);
	}

	return ret;
}

int elektraSpecGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/spec"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/spec", KEY_VALUE, "spec plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/spec/exports", KEY_END),
			       keyNew ("system/elektra/modules/spec/exports/close", KEY_FUNC, elektraSpecClose, KEY_END),
			       keyNew ("system/elektra/modules/spec/exports/get", KEY_FUNC, elektraSpecGet, KEY_END),
			       keyNew ("system/elektra/modules/spec/exports/set", KEY_FUNC, elektraSpecSet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system/elektra/modules/spec/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS; // success
	}

	// parse configuration
	ConflictHandling ch;

	KeySet * config = elektraPluginGetConfig (handle);
	parseConfig (config, &ch, "/conflict/get");

	// create/update plugin data
	PluginData * pluginData = elektraPluginGetData (handle);

	if (pluginData)
	{
		++(pluginData->counter);
	}
	else
	{
		pluginData = elektraMalloc (sizeof (PluginData));
		pluginData->counter = 0;
		pluginData->ks = ksNew (0, KS_END);
	}

	int clean = 0;
	if (pluginData->counter == 1)
	{
		clean = 1;
		pluginData->counter = 0;
	}

	// extract spec namespace, and store in plugin data
	Key * specParent = keyNew ("spec", KEY_END);

	KeySet * specKS = ksCut (returned, specParent);
	ksAppend (pluginData->ks, specKS);

	elektraPluginSetData (handle, pluginData);

	keyDel (specParent);

	// extract other namespaces
	KeySet * ks = ksCut (returned, parentKey);

	// do actual work
	Key * specKey;
	ksRewind (specKS);
	int ret = ELEKTRA_PLUGIN_STATUS_SUCCESS;
	while ((specKey = ksNext (specKS)) != NULL)
	{
		if (clean)
		{
			cleanSpecKey (specKey, ks);
		}
		else if (processSpecKey (specKey, parentKey, ks, &ch, "/conflict/get", true) != 0)
		{
			ret = ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	// reconstruct KeySet
	ksAppend (returned, specKS);
	ksAppend (returned, ks);

	// cleanup
	ksDel (ks);
	ksDel (specKS);

	return ret;
}

int elektraSpecSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	// parse configuration
	ConflictHandling ch;

	KeySet * config = elektraPluginGetConfig (handle);
	parseConfig (config, &ch, "/conflict/set");

	// check for plugin data
	PluginData * pluginConfig = elektraPluginGetData (handle);
	if (!pluginConfig)
	{
		// get has to be called first
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}

	// update plugin data
	++(pluginConfig->counter);

	int clean = 0;
	if (pluginConfig->counter == 2)
	{
		clean = 1;
		pluginConfig->counter = 0;
	}
	elektraPluginSetData (handle, pluginConfig);

	// get spec namespace from plugin data
	KeySet * specKS = pluginConfig->ks;

	// remove spec keys from KeySet
	Key * specParent = keyNew ("spec", KEY_END);
	KeySet * specReturned = ksCut (returned, specParent);
	keyDel (specParent);

	// extract other namespaces
	KeySet * ks = ksCut (returned, parentKey);

	// do actual work
	Key * specKey;
	ksRewind (specKS);
	int ret = ELEKTRA_PLUGIN_STATUS_SUCCESS;
	while ((specKey = ksNext (specKS)) != NULL)
	{
		if (clean)
		{
			cleanSpecKey (specKey, ks);
		}
		else if (processSpecKey (specKey, parentKey, ks, &ch, "/conflict/set", false) != 0)
		{
			ret = ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	// reconstruct KeySet
	ksAppend (returned, specReturned);
	ksAppend (returned, ks);

	// cleanup
	ksDel (ks);

	return ret;
}

int elektraSpecClose (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	PluginData * pluginConfig = elektraPluginGetData (handle);

	if (pluginConfig != NULL)
	{
		ksDel (pluginConfig->ks);

		elektraFree (pluginConfig);
		elektraPluginSetData (handle, NULL);
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("spec",
			ELEKTRA_PLUGIN_CLOSE, &elektraSpecClose,
			ELEKTRA_PLUGIN_GET,	&elektraSpecGet,
			ELEKTRA_PLUGIN_SET,	&elektraSpecSet,
			ELEKTRA_PLUGIN_END);
}

