/**
 * @file
 *
 * @brief Source for spec plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "spec.h"

#include <elektra/ease/globbing.h>
#include <elektra/ease/meta.h>
#include <elektra/ease/old_ease.h>
#include <elektra/kdb/errors.h>
#include <elektra/type/types.h>
#include <internal/utility/assert.h>
#include <internal/utility/logger.h>
#include <internal/utility/old_helper.h>

#ifndef __MINGW32__
#include <fnmatch.h>
#endif

#include <stdlib.h>
#include <string.h>

typedef enum
{
	ON_CONFLICT_IGNORE,
	ON_CONFLICT_ERROR,
	ON_CONFLICT_WARNING,
	ON_CONFLICT_INFO
} OnConflict;


// clang-format off
#define NO_CONFLICTS         "00000" // on char per conflict type below
#define CONFLICT_ARRAYMEMBER     0
#define CONFLICT_INVALID         1
#define CONFLICT_COLLISION       2
#define CONFLICT_OUTOFRANGE      3
#define CONFLICT_WILDCARDMEMBER  4
// missing keys are handled directly
#define SIZE_CONFLICTS       sizeof(NO_CONFLICTS)
// clang-format on

#define CONFIG_BASE_NAME_GET "/conflict/get"
#define CONFIG_BASE_NAME_SET "/conflict/set"

typedef struct
{
	OnConflict member;
	OnConflict invalid;
	OnConflict count;
	OnConflict conflict;
	OnConflict range;
	OnConflict missing;
	int logMissing;
} ConflictHandling;

static void copyMeta (Key * dest, Key * src);

#ifdef __MINGW32__
static bool specMatches (Key * specKey, Key * otherKey)
{
	/**
	 * Known limitation: For MINGW builds fnmatch.h does not exist. Therefore, globbing can't be used.
	 * This means that there is no support for # and _ in key names.
	 * This function was copied from 68e9dff, doesn't use globbing and therefore doesn't require the globbing library which is not
	 * compatible with Windows:
	 */
	const char * spec = keyUnescapedName (specKey);
	size_t specNsLen = strlen (spec) + 1;
	spec += specNsLen; // skip namespace
	const char * other = keyUnescapedName (otherKey);
	size_t otherNsLen = strlen (other) + 1;
	other += otherNsLen; // skip namespace
	size_t const specSize = keyGetUnescapedNameSize (specKey) - specNsLen;
	size_t const otherSize = keyGetUnescapedNameSize (otherKey) - otherNsLen;

	return specSize == otherSize && memcmp (spec, other, specSize) == 0;
}
#else
static bool specMatches (Key * specKey, Key * otherKey)
{
	// ignore namespaces for globbing
	Key * globKey = keyNew (strchr (keyName (otherKey), '/'), KEY_END);
	bool matches = elektraKeyGlob (globKey, strchr (keyName (specKey), '/')) == 0;
	keyDel (globKey);
	return matches;
}
#endif


static inline void safeFree (void * ptr)
{
	if (ptr != NULL)
	{
		elektraFree (ptr);
	}
}

/*  region Config parsing    */
/* ========================= */

static OnConflict parseOnConflictKey (const Key * key)
{
	const char * string = keyString (key);
	if (strcmp (string, "ERROR") == 0)
	{
		return ON_CONFLICT_ERROR;
	}
	else if (strcmp (string, "WARNING") == 0)
	{
		return ON_CONFLICT_WARNING;
	}
	else if (strcmp (string, "INFO") == 0)
	{
		return ON_CONFLICT_INFO;
	}
	else
	{
		return ON_CONFLICT_IGNORE;
	}
}

static void parseConfig (KeySet * config, ConflictHandling * ch, const char baseName[14])
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

	strcpy (nameBufferEnd, "/collision");
	key = ksLookupByName (config, nameBuffer, 0);
	ch->conflict = key == NULL ? base : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/range");
	key = ksLookupByName (config, nameBuffer, 0);
	ch->range = key == NULL ? base : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/missing");
	key = ksLookupByName (config, nameBuffer, 0);
	ch->missing = key == NULL ? base : parseOnConflictKey (key);

	key = ksLookupByName (config, "/missing/log", 0);
	ch->logMissing = key != NULL && strcmp (keyString (key), "1") == 0;
}

static void parseLocalConfig (Key * specKey, ConflictHandling * ch, bool isKdbGet)
{
	char nameBuffer[32];
	strcpy (nameBuffer, isKdbGet ? CONFIG_BASE_NAME_GET : CONFIG_BASE_NAME_SET);
	char * nameBufferEnd = nameBuffer + strlen (nameBuffer);

	strcpy (nameBufferEnd, "/member");
	const Key * key = keyGetMeta (specKey, nameBuffer);
	ch->member = key == NULL ? ch->member : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/invalid");
	key = keyGetMeta (specKey, nameBuffer);
	ch->invalid = key == NULL ? ch->invalid : parseOnConflictKey (key);

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

	const Key * meta = keyGetMeta (key, "conflict");
	if (keyGetValueSize (meta) == SIZE_CONFLICTS)
	{
		keyGetString (meta, conflicts, SIZE_CONFLICTS);
	}

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
	ELEKTRA_LOG_DEBUG ("spec conflict: %s", msg);

	switch (onConflict)
	{
	case ON_CONFLICT_ERROR:
		keySetMeta (parentKey, "internal/spec/error", "1");
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "%s", msg);
		break;
	case ON_CONFLICT_WARNING:
		ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNINGF (parentKey, "%s", msg);
		break;
	case ON_CONFLICT_INFO:
		elektraMetaArrayAdd (parentKey, "logs/spec/info", msg);
		break;
	case ON_CONFLICT_IGNORE:
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
 * @retval  0 if no conflicts where found, or all found conflicts are ignored
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
	if (keyGetValueSize (metaKey) == SIZE_CONFLICTS)
	{
		keyGetString (metaKey, conflicts, SIZE_CONFLICTS);
	}

	int ret = 0;
	if (conflicts[CONFLICT_INVALID] == '1' && ch->invalid != ON_CONFLICT_IGNORE)
	{
		const Key * moreMsg = keyGetMeta (key, "conflict/invalid");
		char * msg;
		if (moreMsg != NULL)
		{
			msg = elektraFormat ("Invalid key %s: %s", keyName (key), keyString (moreMsg));
		}
		else
		{
			msg = elektraFormat ("Invalid key %s", keyName (key));
		}

		handleConflict (parentKey, msg, ch->invalid);
		elektraFree (msg);
		ret = -1;
	}

	if (conflicts[CONFLICT_ARRAYMEMBER] == '1' && ch->member != ON_CONFLICT_IGNORE)
	{
		char * problemKeys = elektraMetaArrayToString (key, keyName (keyGetMeta (key, "conflict/arraymember")), ", ");
		char * msg =
			elektraFormat ("Array key %s has invalid children (only array elements allowed): %s", keyName (key), problemKeys);
		handleConflict (parentKey, msg, ch->member);
		elektraFree (msg);
		safeFree (problemKeys);
		ret = -1;
	}

	if (conflicts[CONFLICT_WILDCARDMEMBER] == '1' && ch->member != ON_CONFLICT_IGNORE)
	{
		char * problemKeys = elektraMetaArrayToString (key, keyName (keyGetMeta (key, "conflict/wildcardmember")), ", ");
		char * msg =
			elektraFormat ("Widlcard key %s has invalid children (no array elements allowed): %s", keyName (key), problemKeys);
		handleConflict (parentKey, msg, ch->member);
		elektraFree (msg);
		safeFree (problemKeys);
		ret = -1;
	}

	if (conflicts[CONFLICT_COLLISION] == '1' && ch->conflict != ON_CONFLICT_IGNORE)
	{
		char * problemKeys = elektraMetaArrayToString (key, keyName (keyGetMeta (key, "conflict/collision")), ", ");
		char * msg = elektraFormat ("%s has conflicting metakeys: %s", keyName (key), problemKeys);
		handleConflict (parentKey, msg, ch->conflict);
		elektraFree (msg);
		safeFree (problemKeys);
		ret = -1;
	}

	if (conflicts[CONFLICT_OUTOFRANGE] == '1' && ch->range != ON_CONFLICT_IGNORE)
	{
		const Key * min = keyGetMeta (specKey, "array/min");
		const Key * max = keyGetMeta (specKey, "array/max");

		char * msg = elektraFormat ("%s has invalid number of members: %s. Expected: %s - %s", keyName (key),
					    keyString (keyGetMeta (key, "conflict/outofrange")), min == NULL ? "" : keyString (min),
					    max == NULL ? "" : keyString (max));
		handleConflict (parentKey, msg, ch->range);
		elektraFree (msg);
		ret = -1;
	}

	return ret;
}

/**
 * Handles all errors (conflicts) for the given key, including those stored in immediate parent.
 *
 * @param key            The key whose conflicts we handle.
 * @param parentKey      The parent key (for storing errors/warnings)
 * @param ks             The full KeySet
 * @param specKey        The spec Key causing the conflict (for additional information, e.g. max array size)
 * @param ch             How conflicts should be handled
 * @param isKdbGet       is this called from kdbGet?
 *
 * @retval  0 if no conflicts where found
 * @retval -1 otherwise
 */
static int handleErrors (Key * key, Key * parentKey, KeySet * ks, Key * specKey, const ConflictHandling * ch, bool isKdbGet)
{
	ConflictHandling localCh;
	memcpy (&localCh, ch, sizeof (ConflictHandling));

	parseLocalConfig (specKey, &localCh, isKdbGet);

	Key * parentLookup = keyDup (key, KEY_CP_ALL);
	keySetBaseName (parentLookup, NULL);

	Key * parent = ksLookup (ks, parentLookup, KDB_O_NONE);

	keyDel (parentLookup);

	int ret = handleConflicts (parent, parentKey, specKey, &localCh) || handleConflicts (key, parentKey, specKey, &localCh);

	return ret;
}

static int processAllConflicts (Key * specKey, KeySet * ks, Key * parentKey, const ConflictHandling * ch, bool isKdbGet)
{

	int ret = 0;

	for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
	{
		Key * cur = ksAtCursor (ks, it);
		if (handleErrors (cur, parentKey, ks, specKey, ch, isKdbGet) != 0)
		{
			ret = -1;
		}
	}
	return ret;
}

// endregion Conflict handling

/* region Array handling     */
/* ========================= */

/**
 * Checks whether the given key is an array spec,
 * i.e. it has at least one keyname part that is "#".
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

		cur += len + 1;
	}

	return false;
}

/**
 * Checks whether the given key is an instantiated array spec,
 * i.e. its last keyname part is "#" and no other keyname part is "#".
 *
 * @param key a spec key
 *
 * @retval #true  if @p key is an array spec
 * @retval #false otherwise
 */
static bool isInstantiatedArraySpec (const Key * key)
{
	size_t usize = keyGetUnescapedNameSize (key);
	const char * cur = keyUnescapedName (key);
	const char * end = cur + usize;

	while (cur < end)
	{
		size_t len = strlen (cur);

		if (len == 1 && cur[0] == '#')
		{
			return cur + len + 1 >= end;
		}

		cur += len + 1;
	}

	return false;
}

static bool validateArraySize (Key * arrayParent, Key * spec)
{
	const Key * arrayActualKey = keyGetMeta (arrayParent, "array");
	const char * arrayActual = arrayActualKey == NULL ? "" : keyString (arrayActualKey);

	const Key * arrayMinKey = keyGetMeta (spec, "array/min");
	const char * arrayMin = arrayMinKey == NULL ? NULL : keyString (arrayMinKey);

	const Key * arrayMaxKey = keyGetMeta (spec, "array/max");
	const char * arrayMax = arrayMaxKey == NULL ? NULL : keyString (arrayMaxKey);

	return (arrayMin == NULL || strcmp (arrayMin, arrayActual) <= 0) && (arrayMax == NULL || 0 <= strcmp (arrayActual, arrayMax));
}

static void validateEmptyArray (KeySet * ks, Key * arraySpecParent, Key * parentKey, OnConflict onConflict)
{
	Key * parentLookup = keyNew (strchr (keyName (arraySpecParent), '/'), KEY_END);

	// either existed already, or was added by processSpecKey because of KeySet order
	Key * arrayParent = ksLookup (ks, parentLookup, 0);
	if (keyGetMeta (arrayParent, "internal/spec/array/validated") != NULL)
	{
		keyDel (parentLookup);
		return;
	}

	bool immediate = arrayParent == NULL;
	if (immediate)
	{
		arrayParent = keyNew (keyName (parentLookup), KEY_END);
	}

	// TODO: [improvement] ksExtract?, like ksCut, but doesn't remove -> no need for ksDup
	KeySet * ksCopy = ksDup (ks);
	KeySet * subKeys = ksCut (ksCopy, parentLookup);
	ksDel (ksCopy);

	ssize_t parentLen = keyGetUnescapedNameSize (parentLookup);

	bool haveConflict = false;

	for (elektraCursor it = 0; it < ksGetSize (subKeys); ++it)
	{
		Key * cur = ksAtCursor (subKeys, it);
		if (keyIsBelow (parentLookup, cur) == 0 || keyGetNamespace (cur) == KEY_NS_SPEC)
		{
			continue;
		}

		const char * checkStr = strchr (keyName (cur), ':');
		checkStr += parentLen;

		if (elektraArrayValidateBaseNameString (checkStr) < 0)
		{
			haveConflict = true;
			addConflict (arrayParent, CONFLICT_ARRAYMEMBER);
			elektraMetaArrayAdd (arrayParent, "conflict/arraymember", keyName (cur));
		}
	}

	if (immediate)
	{
		if (haveConflict)
		{
			char * problemKeys =
				elektraMetaArrayToString (arrayParent, keyName (keyGetMeta (arrayParent, "conflict/arraymember")), ", ");
			char * msg = elektraFormat ("Array key %s has invalid children (only array elements allowed): %s",
						    keyName (arrayParent), problemKeys);
			handleConflict (parentKey, msg, onConflict);
			elektraFree (msg);
			safeFree (problemKeys);
		}
		keyDel (arrayParent);
	}

	ksDel (subKeys);
	keyDel (parentLookup);

	if (!immediate)
	{
		keySetMeta (arrayParent, "internal/spec/array/validated", "");
	}
}

static void validateArrayMembers (KeySet * ks, Key * arraySpec)
{
	Key * parentLookup = keyNew (strchr (keyName (arraySpec), '/'), KEY_END);
	keySetBaseName (parentLookup, NULL);

	// either existed already, or was added by processSpecKey because of KeySet order
	Key * arrayParent = ksLookup (ks, parentLookup, 0);
	if (keyGetMeta (arrayParent, "internal/spec/array/validated") != NULL)
	{
		keyDel (parentLookup);
		return;
	}

	// TODO: [improvement] ksExtract?, like ksCut, but doesn't remove -> no need for ksDup
	KeySet * ksCopy = ksDup (ks);
	KeySet * subKeys = ksCut (ksCopy, parentLookup);
	ksDel (ksCopy);

	ssize_t parentLen = keyGetUnescapedNameSize (parentLookup);

	for (elektraCursor it = 0; it < ksGetSize (subKeys); ++it)
	{
		Key * cur = ksAtCursor (subKeys, it);
		if (keyIsBelow (parentLookup, cur) == 0 || keyGetNamespace (cur) == KEY_NS_SPEC ||
		    keyGetNamespace (cur) == KEY_NS_CASCADING)
		{
			continue;
		}

		const char * checkStr = strchr (keyName (cur), ':');
		checkStr += parentLen;

		if (elektraArrayValidateBaseNameString (checkStr) < 0)
		{
			addConflict (arrayParent, CONFLICT_ARRAYMEMBER);
			elektraMetaArrayAdd (arrayParent, "conflict/arraymember", keyName (cur));
		}
	}

	ksDel (subKeys);
	keyDel (parentLookup);

	keySetMeta (arrayParent, "internal/spec/array/validated", "");
}

// instantiates all array spec parts in an array spec key (e.g. abc/#/a/d/#/e)
static KeySet * instantiateArraySpec (KeySet * ks, Key * arraySpec, Key * parentKey, OnConflict onConflict)
{
	size_t usize = keyGetUnescapedNameSize (arraySpec);
	const char * cur = keyUnescapedName (arraySpec);
	const char * end = cur + usize;

	cur += strlen (cur) + 1; // skip "spec:"

	KeySet * newKeys = ksNew (1, keyNew ("spec:/", KEY_END), KS_END);
	KeySet * parents = ksNew (0, KS_END);
	Key * specCur = keyNew ("spec:/", KEY_END);

	while (cur < end)
	{
		size_t len = strlen (cur);

		KeySet * curNew = ksNew (0, KS_END);
		if (len == 1 && cur[0] == '#')
		{
			for (elektraCursor it = 0; it < ksGetSize (newKeys); ++it)
			{
				Key * k = ksAtCursor (newKeys, it);
				Key * lookup = ksLookupByName (ks, strchr (keyName (k), '/'), 0);
				const Key * arrayMeta = lookup == NULL ? NULL : keyGetMeta (lookup, "array");
				Key * specLookup = ksLookup (ks, specCur, 0);
				if (arrayMeta != NULL)
				{
					if (!validateArraySize (lookup, specLookup))
					{
						addConflict (lookup, CONFLICT_OUTOFRANGE);
						keySetMeta (lookup, "conflict/outofrange", keyString (arrayMeta));
						continue;
					}
				}
				else
				{
					lookup = specLookup;
					arrayMeta = lookup == NULL ? NULL : keyGetMeta (lookup, "array");
				}

				const char * arraySize = arrayMeta == NULL ? "" : keyString (arrayMeta);

				if (strlen (arraySize) == 0)
				{
					// empty array
					validateEmptyArray (ks, k, parentKey, onConflict);
					continue;
				}

				if (elektraArrayValidateBaseNameString (arraySize) <= 0)
				{
					addConflict (lookup, CONFLICT_INVALID);
					keySetMeta (lookup, "conflict/invalid", "invalid array metadata");
					continue;
				}

				char elem[ELEKTRA_MAX_ARRAY_SIZE];
				kdb_long_long_t i = 0;
				elektraWriteArrayNumber (elem, i);

				while (strcmp (elem, arraySize) <= 0)
				{
					Key * new = keyDup (k, KEY_CP_ALL);
					keyAddBaseName (new, elem);
					ksAppendKey (curNew, new);

					++i;
					elektraWriteArrayNumber (elem, i);
				}

				Key * parent = keyNew (keyName (k), KEY_END);
				keyAddBaseName (parent, "#");
				ksAppendKey (parents, parent);
			}
		}
		else
		{
			for (elektraCursor it = 0; it < ksGetSize (newKeys); ++it)
			{
				Key * k = ksAtCursor (newKeys, it);
				Key * new = keyDup (k, KEY_CP_ALL);
				keyAddBaseName (new, cur);
				ksAppendKey (curNew, new);
			}
		}
		ksDel (newKeys);
		newKeys = curNew;

		keyAddBaseName (specCur, cur);
		cur += len + 1;
	}

	keyDel (specCur);

	ksAppend (newKeys, parents);
	ksDel (parents);

	for (elektraCursor it = 0; it < ksGetSize (newKeys); ++it)
	{
		Key * k = ksAtCursor (newKeys, it);
		keySetMeta (k, "internal/spec/array", "");
		copyMeta (k, arraySpec);
	}

	return newKeys;
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
	size_t usize = keyGetUnescapedNameSize (key);
	const char * cur = keyUnescapedName (key);
	const char * end = cur + usize;

	while (cur < end)
	{
		size_t len = strlen (cur);

		if (len == 1 && cur[0] == '_')
		{
			return true;
		}

		cur += len + 1;
	}

	return false;
}

/**
 * Handles wildcard spec keys. A conflict will be added,
 * if the number of keys directly below
 * @param ks
 * @param key
 * @param specKey
 */
static void validateWildcardSubs (KeySet * ks, Key * key)
{
	Key * parent = keyDup (key, KEY_CP_ALL);
	keySetBaseName (parent, NULL);

	// TODO: [improvement] ksExtract?, like ksCut, but doesn't remove -> no need for ksDup
	KeySet * ksCopy = ksDup (ks);
	KeySet * subKeys = ksCut (ksCopy, parent);
	ksDel (ksCopy);

	for (elektraCursor it = 0; it < ksGetSize (subKeys); ++it)
	{
		Key * cur = ksAtCursor (subKeys, it);

		if (keyIsDirectlyBelow (parent, cur))
		{
			if (elektraArrayValidateBaseNameString (keyBaseName (cur)) > 0)
			{
				addConflict (parent, CONFLICT_WILDCARDMEMBER);
				elektraMetaArrayAdd (parent, "conflict/wildcardmember", keyName (cur));
			}
		}
	}
	ksDel (subKeys);
	keyDel (parent);
}

// endregion Wildcard (_) handling

/**
 * Copies all metadata (except for internal/ and conflict/) from @p dest to @p src
 */
static void copyMeta (Key * dest, Key * src)
{
	KeySet * metaKS = ksDup (keyMeta (src));

	Key * cutpoint = keyNew ("meta:/internal", KEY_END);
	ksDel (ksCut (metaKS, cutpoint)); // don't care for internal stuff

	keySetName (cutpoint, "meta:/conflict");
	ksDel (ksCut (metaKS, cutpoint)); // don't care for conflict stuff

	keyDel (cutpoint);

	// TODO: could be optimized by iterating both meta key sets simultaneously
	for (elektraCursor cursor = 0; cursor < ksGetSize (metaKS); ++cursor)
	{
		Key * meta = ksAtCursor (metaKS, cursor);
		const char * name = keyName (meta);

		const Key * oldMeta = keyGetMeta (dest, name);
		if (oldMeta != NULL)
		{
			// don't overwrite metadata
			// array metadata is not a conflict
			if (strcmp (name, "meta:/array") != 0 && strcmp (keyString (oldMeta), keyString (meta)) != 0)
			{
				char * conflictName = elektraFormat ("conflict/%s", name);
				keySetMeta (dest, conflictName, keyString (oldMeta));
				elektraFree (conflictName);
				addConflict (dest, CONFLICT_COLLISION);
				elektraMetaArrayAdd (dest, "conflict/collision", name);
			}
		}
		else
		{
			keyCopyMeta (dest, src, name);
		}
	}

	ksDel (metaKS);
}

/**
 * Process exactly one key of the specification.
 *
 * @param specKey        The spec Key to process.
 * @param parentKey      The parent key (for errors)
 * @param ks	         The full KeySet
 * @param ch             How should conflicts be handled?
 * @param isKdbGet       is this the kdbGet call?
 *
 * @retval  0 on success
 * @retval -1 otherwise
 */
static int processSpecKey (Key * specKey, Key * parentKey, KeySet * ks, const ConflictHandling * ch, bool isKdbGet)
{
	bool require = keyGetMeta (specKey, "require") != NULL;
	bool wildcardSpec = isWildcardSpec (specKey);

	if (isInstantiatedArraySpec (specKey))
	{
		// check instantiated arrays
		validateArrayMembers (ks, specKey);
		return 0;
	}

	if (isArraySpec (specKey))
	{
		// ignore other arrays
		return 0;
	}

	int found = 0;
	for (elektraCursor cursor = 0; cursor < ksGetSize (ks); ++cursor)
	{
		Key * cur = ksAtCursor (ks, cursor);
		if (!specMatches (specKey, cur))
		{
			continue;
		}

		found = 1;

		if (wildcardSpec)
		{
			validateWildcardSubs (ks, cur);
		}

		copyMeta (cur, specKey);
	}


	int ret = 0;
	if (!found)
	{
		if (require)
		{
			const char * missing = strchr (keyName (specKey), '/');
			char * msg = elektraFormat ("Required key %s is missing.", missing);
			handleConflict (parentKey, msg, ch->missing);
			elektraFree (msg);
			if (ch->missing != ON_CONFLICT_IGNORE)
			{
				ret = -1;
			}

			if (ch->logMissing)
			{
				elektraMetaArrayAdd (parentKey, "logs/spec/missing", missing);
			}
		}

		if (isKdbGet)
		{
			if (keyGetMeta (specKey, "assign/condition") != NULL)
			{
				Key * newKey = keyNew ("default:/", KEY_END);
				keyAddName (newKey, strchr (keyName (specKey), '/'));
				copyMeta (newKey, specKey);
				ksAppendKey (ks, newKey);
			}
			else if (keyGetMeta (specKey, "default") != NULL)
			{
				Key * newKey = keyNew ("default:/", KEY_VALUE, keyString (keyGetMeta (specKey, "default")), KEY_END);
				keyAddName (newKey, strchr (keyName (specKey), '/'));
				copyMeta (newKey, specKey);
				ksAppendKey (ks, newKey);
			}
		}

		if (keyGetMeta (specKey, "array") != NULL)
		{
			Key * newKey = keyNew ("default:/", KEY_END);
			keyAddName (newKey, strchr (keyName (specKey), '/'));
			copyMeta (newKey, specKey);
			if (!isKdbGet)
			{
				keySetMeta (newKey, "internal/spec/remove", "");
			}
			ksAppendKey (ks, newKey);
		}
	}

	return ret;
}

int elektraSpecCopy (Plugin * handle, KeySet * returned, Key * parentKey, bool isKdbGet)
{
	// parse configuration
	ConflictHandling ch;
	KeySet * config = elektraPluginGetConfig (handle);
	parseConfig (config, &ch, isKdbGet ? CONFIG_BASE_NAME_GET : CONFIG_BASE_NAME_SET);

	// build spec
	KeySet * specKS = ksNew (0, KS_END);

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		Key * cur = ksAtCursor (returned, it);

		if (keyGetNamespace (cur) == KEY_NS_SPEC)
		{
			if (isArraySpec (cur))
			{
				KeySet * specs = instantiateArraySpec (returned, cur, parentKey, ch.member);
				ksAppend (specKS, specs);
				ksDel (specs);
			}

			ksAppendKey (specKS, cur);
		}
	}

	int ret = ELEKTRA_PLUGIN_STATUS_SUCCESS;
	if (keyGetMeta (parentKey, "internal/spec/error") != NULL)
	{
		ret = ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// remove spec namespace from returned
	Key * specParent = keyNew ("spec:/", KEY_END);
	ksDel (ksCut (returned, specParent));
	keyDel (specParent);

	// extract other namespaces
	KeySet * ks = ksCut (returned, parentKey);

	// do actual work
	Key * specKey = NULL;
	for (elektraCursor it = 0; it < ksGetSize (specKS); ++it)
	{
		specKey = ksAtCursor (specKS, it);
		if (processSpecKey (specKey, parentKey, ks, &ch, isKdbGet) != 0)
		{
			ret = ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (!isKdbGet)
		{
			keySetMeta (specKey, "internal/spec/array/validated", NULL);

			if (keyGetMeta (specKey, "internal/spec/array") == NULL && keyGetMeta (specKey, "internal/spec/remove") == NULL)
			{
				ksAppendKey (returned, specKey);
			}
		}
	}

	if (processAllConflicts (specKey, ks, parentKey, &ch, isKdbGet) != 0)
	{
		ret = ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// reconstruct KeySet
	ksAppend (returned, specKS);
	ksAppend (returned, ks);

	// cleanup
	ksDel (ks);
	ksDel (specKS);

	keySetMeta (parentKey, "internal/spec/error", NULL);

	return ret;
}

int elektraSpecRemove (ELEKTRA_UNUSED Plugin * handle, KeySet * returned, Key * parentKey)
{
	Key * specName = keyNew ("spec:/", KEY_END);

	for (elektraCursor i = 0; i < ksGetSize (returned); i++)
	{
		Key * cur = ksAtCursor (returned, i);

		if (keyGetNamespace (cur) == KEY_NS_SPEC)
		{
			continue;
		}

		// Find out if there is a spec:/ key for the current key
		keySetName (specName, "spec:/");
		keyAddName (specName, strchr (keyName (cur), '/'));
		Key * specKey = ksLookup (returned, specName, KDB_O_NONE);

		if (specKey != NULL)
		{
			// Found a spec:/ key!
			// Now remove all meta from the current key that is also contained in the spec:/ key

			KeySet * specMeta = keyMeta (specKey);
			KeySet * meta = keyMeta (cur);

			for (elektraCursor j = 0; j < ksGetSize (specMeta); j++)
			{
				Key * m = ksAtCursor (specMeta, j);
				if (ksLookup (meta, m, 0) == m)
				{
					keySetMeta (cur, keyName (m), NULL);
				}
			}
		}
	}

	keyDel (specName);

	keySetMeta (parentKey, "internal/spec/error", NULL);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraSpecGet (ELEKTRA_UNUSED Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/spec"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/spec", KEY_VALUE, "spec plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/spec/exports", KEY_END),
			       keyNew ("system:/elektra/modules/spec/exports/get", KEY_FUNC, elektraSpecGet, KEY_END),
			       keyNew ("system:/elektra/modules/spec/exports/hook/spec/copy", KEY_FUNC, elektraSpecCopy, KEY_END),
			       keyNew ("system:/elektra/modules/spec/exports/hook/spec/remove", KEY_FUNC, elektraSpecRemove, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/spec/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS; // success
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("spec",
			ELEKTRA_PLUGIN_GET,	&elektraSpecGet,
			ELEKTRA_PLUGIN_END);
}

