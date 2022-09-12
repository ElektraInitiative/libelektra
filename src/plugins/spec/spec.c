/**
 * @file
 *
 * @brief Source for spec plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "spec.h"

#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbglobbing.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <kdbmeta.h>
#include <kdbtypes.h>

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

static void copyMeta (ElektraKey * dest, ElektraKey * src);

#ifdef __MINGW32__
static bool specMatches (ElektraKey * specKey, ElektraKey * otherKey)
{
	/**
	 * Known limitation: For MINGW builds fnmatch.h does not exist. Therefore, globbing can't be used.
	 * This means that there is no support for # and _ in key names.
	 * This function was copied from 68e9dff, doesn't use globbing and therefore doesn't require the globbing library which is not
	 * compatible with Windows:
	 */
	const char * spec = elektraKeyUnescapedName (specKey);
	size_t specNsLen = strlen (spec) + 1;
	spec += specNsLen; // skip namespace
	const char * other = elektraKeyUnescapedName (otherKey);
	size_t otherNsLen = strlen (other) + 1;
	other += otherNsLen; // skip namespace
	size_t const specSize = elektraKeyGetUnescapedNameSize (specKey) - specNsLen;
	size_t const otherSize = elektraKeyGetUnescapedNameSize (otherKey) - otherNsLen;

	return specSize == otherSize && memcmp (spec, other, specSize) == 0;
}
#else
static bool specMatches (ElektraKey * specKey, ElektraKey * otherKey)
{
	// ignore namespaces for globbing
	ElektraKey * globKey = elektraKeyNew (strchr (elektraKeyName (otherKey), '/'), ELEKTRA_KEY_END);
	bool matches = elektraKeyGlob (globKey, strchr (elektraKeyName (specKey), '/')) == 0;
	elektraKeyDel (globKey);
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

static OnConflict parseOnConflictKey (const ElektraKey * key)
{
	const char * string = elektraKeyString (key);
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

static void parseConfig (ElektraKeyset * config, ConflictHandling * ch, const char baseName[14])
{
	char nameBuffer[32];
	strcpy (nameBuffer, baseName);
	char * nameBufferEnd = nameBuffer + strlen (nameBuffer);

	ElektraKey * key = elektraKeysetLookupByName (config, nameBuffer, 0);
	OnConflict base = parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/member");
	key = elektraKeysetLookupByName (config, nameBuffer, 0);
	ch->member = key == NULL ? base : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/invalid");
	key = elektraKeysetLookupByName (config, nameBuffer, 0);
	ch->invalid = key == NULL ? base : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/collision");
	key = elektraKeysetLookupByName (config, nameBuffer, 0);
	ch->conflict = key == NULL ? base : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/range");
	key = elektraKeysetLookupByName (config, nameBuffer, 0);
	ch->range = key == NULL ? base : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/missing");
	key = elektraKeysetLookupByName (config, nameBuffer, 0);
	ch->missing = key == NULL ? base : parseOnConflictKey (key);

	key = elektraKeysetLookupByName (config, "/missing/log", 0);
	ch->logMissing = key != NULL && strcmp (elektraKeyString (key), "1") == 0;
}

static void parseLocalConfig (ElektraKey * specKey, ConflictHandling * ch, bool isKdbGet)
{
	char nameBuffer[32];
	strcpy (nameBuffer, isKdbGet ? CONFIG_BASE_NAME_GET : CONFIG_BASE_NAME_SET);
	char * nameBufferEnd = nameBuffer + strlen (nameBuffer);

	strcpy (nameBufferEnd, "/member");
	const ElektraKey * key = elektraKeyGetMeta (specKey, nameBuffer);
	ch->member = key == NULL ? ch->member : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/invalid");
	key = elektraKeyGetMeta (specKey, nameBuffer);
	ch->invalid = key == NULL ? ch->invalid : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/collision");
	key = elektraKeyGetMeta (specKey, nameBuffer);
	ch->conflict = key == NULL ? ch->conflict : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/range");
	key = elektraKeyGetMeta (specKey, nameBuffer);
	ch->range = key == NULL ? ch->range : parseOnConflictKey (key);

	strcpy (nameBufferEnd, "/missing");
	key = elektraKeyGetMeta (specKey, nameBuffer);
	ch->missing = key == NULL ? ch->missing : parseOnConflictKey (key);
}

// endregion Config parsing

/* region Conflict handling  */
/* ========================= */

static void addConflict (ElektraKey * key, int conflict)
{
	char conflicts[SIZE_CONFLICTS] = NO_CONFLICTS;

	const ElektraKey * meta = elektraKeyGetMeta (key, "conflict");
	if (elektraKeyGetValueSize (meta) == SIZE_CONFLICTS)
	{
		elektraKeyGetString (meta, conflicts, SIZE_CONFLICTS);
	}

	conflicts[conflict] = '1';
	elektraKeySetMeta (key, "conflict", conflicts);
}

/**
 * Handle a single conflict.
 *
 * @param parentKey   The parent key (to store errors and warnings).
 * @param msg         The conflict message
 * @param onConflict  What to do with the conflict
 */
static void handleConflict (ElektraKey * parentKey, const char * msg, OnConflict onConflict)
{
	ELEKTRA_LOG_DEBUG ("spec conflict: %s", msg);

	switch (onConflict)
	{
	case ON_CONFLICT_ERROR:
		elektraKeySetMeta (parentKey, "internal/spec/error", "1");
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
static int handleConflicts (ElektraKey * key, ElektraKey * parentKey, ElektraKey * specKey, const ConflictHandling * ch)
{
	const ElektraKey * metaKey = elektraKeyGetMeta (key, "conflict");

	if (!metaKey)
	{
		return 0;
	}

	char conflicts[SIZE_CONFLICTS] = NO_CONFLICTS;
	if (elektraKeyGetValueSize (metaKey) == SIZE_CONFLICTS)
	{
		elektraKeyGetString (metaKey, conflicts, SIZE_CONFLICTS);
	}

	int ret = 0;
	if (conflicts[CONFLICT_INVALID] == '1' && ch->invalid != ON_CONFLICT_IGNORE)
	{
		const ElektraKey * moreMsg = elektraKeyGetMeta (key, "conflict/invalid");
		char * msg;
		if (moreMsg != NULL)
		{
			msg = elektraFormat ("Invalid key %s: %s", elektraKeyName (key), elektraKeyString (moreMsg));
		}
		else
		{
			msg = elektraFormat ("Invalid key %s", elektraKeyName (key));
		}

		handleConflict (parentKey, msg, ch->invalid);
		elektraFree (msg);
		ret = -1;
	}

	if (conflicts[CONFLICT_ARRAYMEMBER] == '1' && ch->member != ON_CONFLICT_IGNORE)
	{
		char * problemKeys = elektraMetaArrayToString (key, elektraKeyName (elektraKeyGetMeta (key, "conflict/arraymember")), ", ");
		char * msg =
			elektraFormat ("Array key %s has invalid children (only array elements allowed): %s", elektraKeyName (key), problemKeys);
		handleConflict (parentKey, msg, ch->member);
		elektraFree (msg);
		safeFree (problemKeys);
		ret = -1;
	}

	if (conflicts[CONFLICT_WILDCARDMEMBER] == '1' && ch->member != ON_CONFLICT_IGNORE)
	{
		char * problemKeys = elektraMetaArrayToString (key, elektraKeyName (elektraKeyGetMeta (key, "conflict/wildcardmember")), ", ");
		char * msg =
			elektraFormat ("Widlcard key %s has invalid children (no array elements allowed): %s", elektraKeyName (key), problemKeys);
		handleConflict (parentKey, msg, ch->member);
		elektraFree (msg);
		safeFree (problemKeys);
		ret = -1;
	}

	if (conflicts[CONFLICT_COLLISION] == '1' && ch->conflict != ON_CONFLICT_IGNORE)
	{
		char * problemKeys = elektraMetaArrayToString (key, elektraKeyName (elektraKeyGetMeta (key, "conflict/collision")), ", ");
		char * msg = elektraFormat ("%s has conflicting metakeys: %s", elektraKeyName (key), problemKeys);
		handleConflict (parentKey, msg, ch->conflict);
		elektraFree (msg);
		safeFree (problemKeys);
		ret = -1;
	}

	if (conflicts[CONFLICT_OUTOFRANGE] == '1' && ch->range != ON_CONFLICT_IGNORE)
	{
		const ElektraKey * min = elektraKeyGetMeta (specKey, "array/min");
		const ElektraKey * max = elektraKeyGetMeta (specKey, "array/max");

		char * msg = elektraFormat ("%s has invalid number of members: %s. Expected: %s - %s", elektraKeyName (key),
					    elektraKeyString (elektraKeyGetMeta (key, "conflict/outofrange")), min == NULL ? "" : elektraKeyString (min),
					    max == NULL ? "" : elektraKeyString (max));
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
static int handleErrors (ElektraKey * key, ElektraKey * parentKey, ElektraKeyset * ks, ElektraKey * specKey, const ConflictHandling * ch, bool isKdbGet)
{
	ConflictHandling localCh;
	memcpy (&localCh, ch, sizeof (ConflictHandling));

	parseLocalConfig (specKey, &localCh, isKdbGet);

	ElektraKey * parentLookup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
	elektraKeySetBaseName (parentLookup, NULL);

	elektraCursor cursor = elektraKeysetGetCursor (ks);
	ElektraKey * parent = elektraKeysetLookup (ks, parentLookup, ELEKTRA_KDB_O_NONE);
	elektraKeysetSetCursor (ks, cursor);

	elektraKeyDel (parentLookup);

	int ret = handleConflicts (parent, parentKey, specKey, &localCh) || handleConflicts (key, parentKey, specKey, &localCh);

	return ret;
}

static int processAllConflicts (ElektraKey * specKey, ElektraKeyset * ks, ElektraKey * parentKey, const ConflictHandling * ch, bool isKdbGet)
{
	ElektraKey * cur;
	int ret = 0;
	elektraKeysetRewind (ks);
	while ((cur = elektraKeysetNext (ks)) != NULL)
	{
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
static bool isArraySpec (const ElektraKey * key)
{
	size_t usize = elektraKeyGetUnescapedNameSize (key);
	const char * cur = elektraKeyUnescapedName (key);
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
static bool isInstantiatedArraySpec (const ElektraKey * key)
{
	size_t usize = elektraKeyGetUnescapedNameSize (key);
	const char * cur = elektraKeyUnescapedName (key);
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

static bool validateArraySize (ElektraKey * arrayParent, ElektraKey * spec)
{
	const ElektraKey * arrayActualKey = elektraKeyGetMeta (arrayParent, "array");
	const char * arrayActual = arrayActualKey == NULL ? "" : elektraKeyString (arrayActualKey);

	const ElektraKey * arrayMinKey = elektraKeyGetMeta (spec, "array/min");
	const char * arrayMin = arrayMinKey == NULL ? NULL : elektraKeyString (arrayMinKey);

	const ElektraKey * arrayMaxKey = elektraKeyGetMeta (spec, "array/max");
	const char * arrayMax = arrayMaxKey == NULL ? NULL : elektraKeyString (arrayMaxKey);

	return (arrayMin == NULL || strcmp (arrayMin, arrayActual) <= 0) && (arrayMax == NULL || 0 <= strcmp (arrayActual, arrayMax));
}

static void validateEmptyArray (ElektraKeyset * ks, ElektraKey * arraySpecParent, ElektraKey * parentKey, OnConflict onConflict)
{
	ElektraKey * parentLookup = elektraKeyNew (strchr (elektraKeyName (arraySpecParent), '/'), ELEKTRA_KEY_END);

	// either existed already, or was added by processSpecKey because of KeySet order
	ElektraKey * arrayParent = elektraKeysetLookup (ks, parentLookup, 0);
	if (elektraKeyGetMeta (arrayParent, "internal/spec/array/validated") != NULL)
	{
		elektraKeyDel (parentLookup);
		return;
	}

	bool immediate = arrayParent == NULL;
	if (immediate)
	{
		arrayParent = elektraKeyNew (elektraKeyName (parentLookup), ELEKTRA_KEY_END);
	}

	// TODO: [improvement] ksExtract?, like ksCut, but doesn't remove -> no need for ksDup
	ElektraKeyset * elektraKeysetCopy = elektraKeysetDup (ks);
	ElektraKeyset * subKeys = elektraKeysetCut (elektraKeysetCopy, parentLookup);
	elektraKeysetDel (elektraKeysetCopy);

	ssize_t parentLen = elektraKeyGetUnescapedNameSize (parentLookup);

	bool haveConflict = false;
	ElektraKey * cur;
	elektraKeysetRewind (subKeys);
	while ((cur = elektraKeysetNext (subKeys)) != NULL)
	{
		if (elektraKeyIsBelow (parentLookup, cur) == 0 || elektraKeyGetNamespace (cur) == ELEKTRA_NS_SPEC)
		{
			continue;
		}

		const char * checkStr = strchr (elektraKeyName (cur), ':');
		checkStr += parentLen;

		if (elektraArrayValidateBaseNameString (checkStr) < 0)
		{
			haveConflict = true;
			addConflict (arrayParent, CONFLICT_ARRAYMEMBER);
			elektraMetaArrayAdd (arrayParent, "conflict/arraymember", elektraKeyName (cur));
		}
	}

	if (immediate)
	{
		if (haveConflict)
		{
			char * problemKeys =
				elektraMetaArrayToString (arrayParent, elektraKeyName (elektraKeyGetMeta (arrayParent, "conflict/arraymember")), ", ");
			char * msg = elektraFormat ("Array key %s has invalid children (only array elements allowed): %s",
						    elektraKeyName (arrayParent), problemKeys);
			handleConflict (parentKey, msg, onConflict);
			elektraFree (msg);
			safeFree (problemKeys);
		}
		elektraKeyDel (arrayParent);
	}

	elektraKeysetDel (subKeys);
	elektraKeyDel (parentLookup);

	if (!immediate)
	{
		elektraKeySetMeta (arrayParent, "internal/spec/array/validated", "");
	}
}

static void validateArrayMembers (ElektraKeyset * ks, ElektraKey * arraySpec)
{
	ElektraKey * parentLookup = elektraKeyNew (strchr (elektraKeyName (arraySpec), '/'), ELEKTRA_KEY_END);
	elektraKeySetBaseName (parentLookup, NULL);

	// either existed already, or was added by processSpecKey because of KeySet order
	ElektraKey * arrayParent = elektraKeysetLookup (ks, parentLookup, 0);
	if (elektraKeyGetMeta (arrayParent, "internal/spec/array/validated") != NULL)
	{
		elektraKeyDel (parentLookup);
		return;
	}

	// TODO: [improvement] ksExtract?, like ksCut, but doesn't remove -> no need for ksDup
	ElektraKeyset * elektraKeysetCopy = elektraKeysetDup (ks);
	ElektraKeyset * subKeys = elektraKeysetCut (elektraKeysetCopy, parentLookup);
	elektraKeysetDel (elektraKeysetCopy);

	ssize_t parentLen = elektraKeyGetUnescapedNameSize (parentLookup);

	ElektraKey * cur;
	elektraKeysetRewind (subKeys);
	while ((cur = elektraKeysetNext (subKeys)) != NULL)
	{
		if (elektraKeyIsBelow (parentLookup, cur) == 0 || elektraKeyGetNamespace (cur) == ELEKTRA_NS_SPEC ||
		    elektraKeyGetNamespace (cur) == ELEKTRA_NS_CASCADING)
		{
			continue;
		}

		const char * checkStr = strchr (elektraKeyName (cur), ':');
		checkStr += parentLen;

		if (elektraArrayValidateBaseNameString (checkStr) < 0)
		{
			addConflict (arrayParent, CONFLICT_ARRAYMEMBER);
			elektraMetaArrayAdd (arrayParent, "conflict/arraymember", elektraKeyName (cur));
		}
	}

	elektraKeysetDel (subKeys);
	elektraKeyDel (parentLookup);

	elektraKeySetMeta (arrayParent, "internal/spec/array/validated", "");
}

// instantiates all array spec parts in an array spec key (e.g. abc/#/a/d/#/e)
static ElektraKeyset * instantiateArraySpec (ElektraKeyset * ks, ElektraKey * arraySpec, ElektraKey * parentKey, OnConflict onConflict)
{
	elektraCursor cursor = elektraKeysetGetCursor (ks);
	size_t usize = elektraKeyGetUnescapedNameSize (arraySpec);
	const char * cur = elektraKeyUnescapedName (arraySpec);
	const char * end = cur + usize;

	cur += strlen (cur) + 1; // skip "spec:"

	ElektraKeyset * newKeys = elektraKeysetNew (1, elektraKeyNew ("spec:/", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * parents = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKey * specCur = elektraKeyNew ("spec:/", ELEKTRA_KEY_END);

	while (cur < end)
	{
		size_t len = strlen (cur);

		ElektraKeyset * curNew = elektraKeysetNew (0, ELEKTRA_KS_END);
		if (len == 1 && cur[0] == '#')
		{

			ElektraKey * k;
			elektraKeysetRewind (newKeys);
			while ((k = elektraKeysetNext (newKeys)) != NULL)
			{
				ElektraKey * lookup = elektraKeysetLookupByName (ks, strchr (elektraKeyName (k), '/'), 0);
				const ElektraKey * arrayMeta = lookup == NULL ? NULL : elektraKeyGetMeta (lookup, "array");
				ElektraKey * specLookup = elektraKeysetLookup (ks, specCur, 0);
				if (arrayMeta != NULL)
				{
					if (!validateArraySize (lookup, specLookup))
					{
						addConflict (lookup, CONFLICT_OUTOFRANGE);
						elektraKeySetMeta (lookup, "conflict/outofrange", elektraKeyString (arrayMeta));
						continue;
					}
				}
				else
				{
					lookup = specLookup;
					arrayMeta = lookup == NULL ? NULL : elektraKeyGetMeta (lookup, "array");
				}

				const char * arraySize = arrayMeta == NULL ? "" : elektraKeyString (arrayMeta);

				if (strlen (arraySize) == 0)
				{
					// empty array
					validateEmptyArray (ks, k, parentKey, onConflict);
					continue;
				}

				if (elektraArrayValidateBaseNameString (arraySize) <= 0)
				{
					addConflict (lookup, CONFLICT_INVALID);
					elektraKeySetMeta (lookup, "conflict/invalid", "invalid array metadata");
					continue;
				}

				char elem[ELEKTRA_MAX_ARRAY_SIZE];
				kdb_long_long_t i = 0;
				elektraWriteArrayNumber (elem, i);

				while (strcmp (elem, arraySize) <= 0)
				{
					ElektraKey * new = elektraKeyDup (k, ELEKTRA_KEY_CP_ALL);
					elektraKeyAddBaseName (new, elem);
					elektraKeysetAppendKey (curNew, new);

					++i;
					elektraWriteArrayNumber (elem, i);
				}

				ElektraKey * parent = elektraKeyNew (elektraKeyName (k), ELEKTRA_KEY_END);
				elektraKeyAddBaseName (parent, "#");
				elektraKeysetAppendKey (parents, parent);
			}
		}
		else
		{
			ElektraKey * k;
			elektraKeysetRewind (newKeys);
			while ((k = elektraKeysetNext (newKeys)) != NULL)
			{
				ElektraKey * new = elektraKeyDup (k, ELEKTRA_KEY_CP_ALL);
				elektraKeyAddBaseName (new, cur);
				elektraKeysetAppendKey (curNew, new);
			}
		}
		elektraKeysetDel (newKeys);
		newKeys = curNew;

		elektraKeyAddBaseName (specCur, cur);
		cur += len + 1;
	}

	elektraKeyDel (specCur);

	elektraKeysetAppend (newKeys, parents);
	elektraKeysetDel (parents);

	ElektraKey * k;
	elektraKeysetRewind (newKeys);
	while ((k = elektraKeysetNext (newKeys)) != NULL)
	{
		elektraKeySetMeta (k, "internal/spec/array", "");
		copyMeta (k, arraySpec);
	}

	elektraKeysetSetCursor (ks, cursor);
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
static bool isWildcardSpec (const ElektraKey * key)
{
	size_t usize = elektraKeyGetUnescapedNameSize (key);
	const char * cur = elektraKeyUnescapedName (key);
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
static void validateWildcardSubs (ElektraKeyset * ks, ElektraKey * key)
{
	ElektraKey * parent = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
	elektraKeySetBaseName (parent, NULL);

	// TODO: [improvement] ksExtract?, like ksCut, but doesn't remove -> no need for ksDup
	ElektraKeyset * elektraKeysetCopy = elektraKeysetDup (ks);
	ElektraKeyset * subKeys = elektraKeysetCut (elektraKeysetCopy, parent);
	elektraKeysetDel (elektraKeysetCopy);

	ElektraKey * cur;
	while ((cur = elektraKeysetNext (subKeys)) != NULL)
	{
		if (elektraKeyIsDirectlyBelow (parent, cur))
		{
			if (elektraArrayValidateBaseNameString (elektraKeyBaseName (cur)) > 0)
			{
				addConflict (parent, CONFLICT_WILDCARDMEMBER);
				elektraMetaArrayAdd (parent, "conflict/wildcardmember", elektraKeyName (cur));
			}
		}
	}
	elektraKeysetDel (subKeys);
	elektraKeyDel (parent);
}

// endregion Wildcard (_) handling

/**
 * Copies all metadata (except for internal/ and conflict/) from @p dest to @p src
 */
static void copyMeta (ElektraKey * dest, ElektraKey * src)
{
	ElektraKeyset * metaKS = elektraKeysetDup (elektraKeyMeta (src));

	ElektraKey * cutpoint = elektraKeyNew ("meta:/internal", ELEKTRA_KEY_END);
	elektraKeysetDel (elektraKeysetCut (metaKS, cutpoint)); // don't care for internal stuff

	elektraKeySetName (cutpoint, "meta:/conflict");
	elektraKeysetDel (elektraKeysetCut (metaKS, cutpoint)); // don't care for conflict stuff

	elektraKeyDel (cutpoint);

	// TODO: could be optimized by iterating both meta key sets simultaneously
	for (elektraCursor cursor = 0; cursor < elektraKeysetGetSize (metaKS); ++cursor)
	{
		ElektraKey * meta = elektraKeysetAtCursor (metaKS, cursor);
		const char * name = elektraKeyName (meta);

		const ElektraKey * oldMeta = elektraKeyGetMeta (dest, name);
		if (oldMeta != NULL)
		{
			// don't overwrite metadata
			// array metadata is not a conflict
			if (strcmp (name, "meta:/array") != 0 && strcmp (elektraKeyString (oldMeta), elektraKeyString (meta)) != 0)
			{
				char * conflictName = elektraFormat ("conflict/%s", name);
				elektraKeySetMeta (dest, conflictName, elektraKeyString (oldMeta));
				elektraFree (conflictName);
				addConflict (dest, CONFLICT_COLLISION);
				elektraMetaArrayAdd (dest, "conflict/collision", name);
			}
		}
		else
		{
			elektraKeyCopyMeta (dest, src, name);
		}
	}

	elektraKeysetDel (metaKS);
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
static int processSpecKey (ElektraKey * specKey, ElektraKey * parentKey, ElektraKeyset * ks, const ConflictHandling * ch, bool isKdbGet)
{
	bool require = elektraKeyGetMeta (specKey, "require") != NULL;
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
	for (elektraCursor cursor = 0; cursor < elektraKeysetGetSize (ks); ++cursor)
	{
		ElektraKey * cur = elektraKeysetAtCursor (ks, cursor);
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
			const char * missing = strchr (elektraKeyName (specKey), '/');
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
			if (elektraKeyGetMeta (specKey, "assign/condition") != NULL)
			{
				ElektraKey * newKey = elektraKeyNew ("default:/", ELEKTRA_KEY_END);
				elektraKeyAddName (newKey, strchr (elektraKeyName (specKey), '/'));
				copyMeta (newKey, specKey);
				elektraKeysetAppendKey (ks, newKey);
			}
			else if (elektraKeyGetMeta (specKey, "default") != NULL)
			{
				ElektraKey * newKey = elektraKeyNew ("default:/", ELEKTRA_KEY_VALUE, elektraKeyString (elektraKeyGetMeta (specKey, "default")), ELEKTRA_KEY_END);
				elektraKeyAddName (newKey, strchr (elektraKeyName (specKey), '/'));
				copyMeta (newKey, specKey);
				elektraKeysetAppendKey (ks, newKey);
			}
		}

		if (elektraKeyGetMeta (specKey, "array") != NULL)
		{
			ElektraKey * newKey = elektraKeyNew ("default:/", ELEKTRA_KEY_END);
			elektraKeyAddName (newKey, strchr (elektraKeyName (specKey), '/'));
			copyMeta (newKey, specKey);
			if (!isKdbGet)
			{
				elektraKeySetMeta (newKey, "internal/spec/remove", "");
			}
			elektraKeysetAppendKey (ks, newKey);
		}
	}

	return ret;
}

int elektraSpecGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/spec"))
	{
		ElektraKeyset * contract =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/spec", ELEKTRA_KEY_VALUE, "spec plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/spec/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/spec/exports/get", ELEKTRA_KEY_FUNC, elektraSpecGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/spec/exports/set", ELEKTRA_KEY_FUNC, elektraSpecSet, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       elektraKeyNew ("system:/elektra/modules/spec/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS; // success
	}

	// parse configuration
	ConflictHandling ch;

	ElektraKeyset * config = elektraPluginGetConfig (handle);
	parseConfig (config, &ch, CONFIG_BASE_NAME_GET);

	// build spec
	ElektraKeyset * specKS = elektraKeysetNew (0, ELEKTRA_KS_END);

	ElektraKey * cur;
	elektraKeysetRewind (returned);
	while ((cur = elektraKeysetNext (returned)) != NULL)
	{
		if (elektraKeyGetNamespace (cur) == ELEKTRA_NS_SPEC)
		{
			if (isArraySpec (cur))
			{
				ElektraKeyset * specs = instantiateArraySpec (returned, cur, parentKey, ch.member);
				elektraKeysetAppend (specKS, specs);
				elektraKeysetDel (specs);
			}

			elektraKeysetAppendKey (specKS, cur);
		}
	}

	int ret = ELEKTRA_PLUGIN_STATUS_SUCCESS;
	if (elektraKeyGetMeta (parentKey, "internal/spec/error") != NULL)
	{
		ret = ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// remove spec namespace from returned
	ElektraKey * specParent = elektraKeyNew ("spec:/", ELEKTRA_KEY_END);
	elektraKeysetDel (elektraKeysetCut (returned, specParent));
	elektraKeyDel (specParent);

	// extract other namespaces
	ElektraKeyset * ks = elektraKeysetCut (returned, parentKey);

	// do actual work
	ElektraKey * specKey;
	elektraKeysetRewind (specKS);
	while ((specKey = elektraKeysetNext (specKS)) != NULL)
	{
		if (processSpecKey (specKey, parentKey, ks, &ch, true) != 0)
		{
			ret = ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	if (processAllConflicts (specKey, ks, parentKey, &ch, true) != 0)
	{
		ret = ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// reconstruct KeySet
	elektraKeysetAppend (returned, specKS);
	elektraKeysetAppend (returned, ks);

	// cleanup
	elektraKeysetDel (ks);
	elektraKeysetDel (specKS);

	elektraKeySetMeta (parentKey, "internal/spec/error", NULL);

	return ret;
}

int elektraSpecSet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	// parse configuration
	ConflictHandling ch;

	ElektraKeyset * config = elektraPluginGetConfig (handle);
	parseConfig (config, &ch, CONFIG_BASE_NAME_SET);

	// build spec
	ElektraKeyset * specKS = elektraKeysetNew (0, ELEKTRA_KS_END);

	ElektraKey * cur;
	elektraKeysetRewind (returned);
	while ((cur = elektraKeysetNext (returned)) != NULL)
	{
		if (elektraKeyGetNamespace (cur) == ELEKTRA_NS_SPEC)
		{
			if (isArraySpec (cur))
			{
				ElektraKeyset * specs = instantiateArraySpec (returned, cur, parentKey, ch.member);
				elektraKeysetAppend (specKS, specs);
				elektraKeysetDel (specs);
			}

			elektraKeysetAppendKey (specKS, cur);
		}
	}

	int ret = ELEKTRA_PLUGIN_STATUS_SUCCESS;
	if (elektraKeyGetMeta (parentKey, "internal/spec/error") != NULL)
	{
		ret = ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// remove spec namespace from returned
	ElektraKey * specParent = elektraKeyNew ("spec:/", ELEKTRA_KEY_END);
	elektraKeysetDel (elektraKeysetCut (returned, specParent));
	elektraKeyDel (specParent);

	// extract other namespaces
	ElektraKeyset * ks = elektraKeysetCut (returned, parentKey);

	// do actual work
	ElektraKey * specKey;
	elektraKeysetRewind (specKS);
	while ((specKey = elektraKeysetNext (specKS)) != NULL)
	{
		if (processSpecKey (specKey, parentKey, ks, &ch, false) != 0)
		{
			ret = ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		elektraKeySetMeta (specKey, "internal/spec/array/validated", NULL);

		if (elektraKeyGetMeta (specKey, "internal/spec/array") == NULL && elektraKeyGetMeta (specKey, "internal/spec/remove") == NULL)
		{
			elektraKeysetAppendKey (returned, specKey);
		}
	}

	if (processAllConflicts (specKey, ks, parentKey, &ch, false) != 0)
	{
		ret = ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// reconstruct KeySet
	elektraKeysetRewind (ks);
	while ((cur = elektraKeysetNext (ks)) != NULL)
	{
		if (elektraKeyGetNamespace (cur) == ELEKTRA_NS_SPEC)
		{
			continue;
		}

		elektraKeySetMeta (cur, "internal/spec/array/validated", NULL);

		if (elektraKeyGetMeta (cur, "internal/spec/array") == NULL && elektraKeyGetMeta (cur, "internal/spec/remove") == NULL)
		{
			elektraKeysetAppendKey (returned, cur);
		}
	}

	// cleanup
	elektraKeysetDel (ks);
	elektraKeysetDel (specKS);

	elektraKeySetMeta (parentKey, "internal/spec/error", NULL);

	return ret;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("spec",
			ELEKTRA_PLUGIN_GET,	&elektraSpecGet,
			ELEKTRA_PLUGIN_SET,	&elektraSpecSet,
			ELEKTRA_PLUGIN_END);
}

