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
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <kdbprivate.h> //elektraArrayValidateName
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define MAX_CHARS_IN_LONG 26

typedef enum { GET, SET } Direction;

typedef enum { ERROR, WARNING, INFO, IGNORE } OnConflict;

typedef enum {
	ARRAYMEMBER,
	INVALID,
	SUBCOUNT,
	CONFLICT,
	OUTOFRANGE,
	MISSING,
	NAC // Not A Conflict
} Conflict;

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
} PluginConfig;


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

static int isValidArrayKey (Key * key)
{
	Key * copy = keyDup (key);
	do
	{
		if (keyBaseName (copy)[0] == '#')
		{
			if (elektraArrayValidateName (copy) == -1)
			{
				keyDel (copy);
				return 0;
			}
		}
	} while (keySetBaseName (copy, 0) != -1);
	keyDel (copy);
	return 1;
}

static int hasArray (Key * key)
{
	if (!strstr (keyName (key), "/#"))
		return 0;
	else
		return 1;
}

static OnConflict getConfOption (Key * key)
{
	const char * string = keyString (key);
	if (!strcmp (string, "ERROR"))
	{
		return ERROR;
	}
	else if (!strcmp (string, "WARNING"))
	{
		return WARNING;
	}
	else if (!strcmp (string, "INFO"))
	{
		return INFO;
	}
	else
	{
		return IGNORE;
	}
}


static void validateArrayRange (Key * parent, long validCount, Key * specKey)
{
	const Key * arrayRange = keyGetMeta (specKey, "array");
	if (arrayRange != NULL)
	{
		char * rangeString = elektraMalloc (keyGetValueSize (arrayRange));
		keyGetString (arrayRange, rangeString, keyGetValueSize (arrayRange));
		char * delimPtr = strchr (rangeString, '-');
		long min = 0;
		long max = 0;
		if (delimPtr)
		{
			char * maxString = delimPtr + 1;
			*delimPtr = '\0';
			char * minString = rangeString;
			min = atoi (minString);
			max = atoi (maxString);
		}
		else
		{
			min = max = atoi (rangeString);
		}
		if (validCount < min || validCount > max)
		{
			char buffer[MAX_CHARS_IN_LONG + 1];
			snprintf (buffer, sizeof (buffer), "%ld", validCount);
			keySetMeta (parent, "conflict/range", buffer);
		}
		elektraFree (rangeString);
	}
}

static void validateArray (KeySet * ks, Key * arrayKey, Key * specKey)
{
	Key * tmpArrayParent = keyDup (arrayKey);
	keySetBaseName (tmpArrayParent, 0);
	Key * arrayParent = ksLookup (ks, tmpArrayParent, KDB_O_NONE);
	keyDel (tmpArrayParent);
	if (arrayParent == NULL) return;
	KeySet * ksCopy = ksDup (ks);
	KeySet * subKeys = ksCut (ksCopy, arrayParent);
	Key * cur;
	long validCount = 0;
	while ((cur = ksNext (subKeys)) != NULL)
	{
		if (!keyIsDirectBelow (arrayParent, cur)) continue;
		if (keyBaseName (cur)[0] == '#')
		{
			if (elektraArrayValidateName (cur) == 1)
			{
				++validCount;
				keySetMeta (cur, "spec/internal/valid", "");
			}
			else
			{
				KeySet * invalidCutKS = ksCut (subKeys, cur);
				Key * toMark;
				while ((toMark = ksNext (invalidCutKS)) != NULL)
				{
					if (strcmp (keyName (cur), keyName (toMark))) keySetMeta (toMark, "conflict/invalid", "");
					elektraMetaArrayAdd (arrayParent, "conflict/invalid/hasmember", keyName (toMark));
				}
				ksDel (invalidCutKS);
			}
		}
	}
	ksDel (subKeys);
	ksDel (ksCopy);
	validateArrayRange (arrayParent, validCount, specKey);
}
static void validateWildcardSubs (KeySet * ks, Key * key, Key * specKey)
{
	const Key * requiredMeta = keyGetMeta (specKey, "required");
	if (!requiredMeta) return;
	Key * tmpParent = keyDup (key);
	keySetBaseName (tmpParent, 0);
	Key * parent = ksLookup (ks, tmpParent, KDB_O_NONE);
	keyDel (tmpParent);
	if (parent == NULL) return;
	KeySet * ksCopy = ksDup (ks);
	KeySet * subKeys = ksCut (ksCopy, parent);
	Key * cur;
	long subCount = 0;
	while ((cur = ksNext (subKeys)) != NULL)
	{
		if (keyIsDirectBelow (parent, cur)) ++subCount;
	}
	long required = atol (keyString (requiredMeta));
	if (required != subCount)
	{
		char buffer[MAX_CHARS_IN_LONG + 1];
		snprintf (buffer, sizeof (buffer), "%ld", subCount);
		keySetMeta (parent, "conflict/invalid/subcount", buffer);
	}

	ksDel (subKeys);
	ksDel (ksCopy);
}


static void parseLocalConfig (Key * specKey, ConflictHandling * localCh, Direction dir)
{
	Key * localConflictMeta = NULL;
	switch (dir)
	{
	case GET:
		if ((localConflictMeta = (Key *) keyGetMeta (specKey, "conflict/get/member")) != NULL)
		{
			localCh->member = getConfOption (localConflictMeta);
		}
		else if ((localConflictMeta = (Key *) keyGetMeta (specKey, "conflict/get/invalid")) != NULL)
		{
			localCh->invalid = getConfOption (localConflictMeta);
		}
		else if ((localConflictMeta = (Key *) keyGetMeta (specKey, "conflict/get/count")) != NULL)
		{
			localCh->count = getConfOption (localConflictMeta);
		}
		else if ((localConflictMeta = (Key *) keyGetMeta (specKey, "conflict/get/collision")) != NULL)
		{
			localCh->conflict = getConfOption (localConflictMeta);
		}
		else if ((localConflictMeta = (Key *) keyGetMeta (specKey, "conflict/get/range")) != NULL)
		{
			localCh->range = getConfOption (localConflictMeta);
		}
		else if ((localConflictMeta = (Key *) keyGetMeta (specKey, "conflict/get/missing")) != NULL)
		{
			localCh->missing = getConfOption (localConflictMeta);
		}
		break;
	case SET:
		if ((localConflictMeta = (Key *) keyGetMeta (specKey, "conflict/set/member")) != NULL)
		{
			localCh->member = getConfOption (localConflictMeta);
		}
		else if ((localConflictMeta = (Key *) keyGetMeta (specKey, "conflict/set/invalid")) != NULL)
		{
			localCh->invalid = getConfOption (localConflictMeta);
		}
		else if ((localConflictMeta = (Key *) keyGetMeta (specKey, "conflict/set/count")) != NULL)
		{
			localCh->count = getConfOption (localConflictMeta);
		}
		else if ((localConflictMeta = (Key *) keyGetMeta (specKey, "conflict/set/collision")) != NULL)
		{
			localCh->conflict = getConfOption (localConflictMeta);
		}
		else if ((localConflictMeta = (Key *) keyGetMeta (specKey, "conflict/set/range")) != NULL)
		{
			localCh->range = getConfOption (localConflictMeta);
		}
		else if ((localConflictMeta = (Key *) keyGetMeta (specKey, "conflict/set/missing")) != NULL)
		{
			localCh->missing = getConfOption (localConflictMeta);
		}
		break;
	}
}

static Conflict getConflict (Key * conflictMeta)
{
	if (!strcmp (keyName (conflictMeta), "conflict/invalid"))
		return INVALID;
	else if (!strcmp (keyName (conflictMeta), "conflict/collision"))
		return CONFLICT;
	else if (!strcmp (keyName (conflictMeta), "conflict/invalid/hasmember"))
		return ARRAYMEMBER;
	else if (!strcmp (keyName (conflictMeta), "conflict/range"))
		return OUTOFRANGE;
	else if (!strcmp (keyName (conflictMeta), "conflict/invalid/subcount"))
		return SUBCOUNT;
	else if (!strcmp (keyName (conflictMeta), "conflict/missing"))
		return MISSING;
	else
		return NAC;
}

static int handleInvalidConflict (Key * parentKey, Key * key, OnConflict onConflict)
{
	int ret = 0;
	switch (onConflict)
	{
	case ERROR:
		ELEKTRA_SET_ERRORF (142, parentKey, "Invalid key %s\n", keyName (key));
		ret = -1;
		break;
	case WARNING:
		ELEKTRA_ADD_WARNINGF (143, parentKey, "Invalid key %s\n", keyName (key));
		break;
	case INFO:
	{
		const char * infoString = "Invalid key ";
		const size_t len = elektraStrLen (infoString) + elektraStrLen (keyName (key)) - 1;
		char * buffer = elektraMalloc (len);
		snprintf (buffer, len, "Invalid key %s\n", keyName (key));
		elektraMetaArrayAdd (key, "logs/spec/info", buffer);
		elektraFree (buffer);
	}
	break;
	case IGNORE:

		break;
	}
	return ret;
}

static int handleArrayConflict (Key * parentKey, Key * key, Key * conflictMeta, OnConflict onConflict)
{
	int ret = 0;
	const char * problemKeys = elektraMetaArrayToString (key, keyName (conflictMeta), ", ");
	switch (onConflict)
	{
	case ERROR:
		ELEKTRA_SET_ERRORF (142, parentKey, "%s has invalid array key members: %s\n", keyName (key), problemKeys);
		ret = -1;
		break;
	case WARNING:
		ELEKTRA_ADD_WARNINGF (143, parentKey, "%s has invalid array members: %s\n", keyName (key), problemKeys);
		break;
	case INFO:
	{
		const char * infoString = "invalid array members: ";
		const size_t len = elektraStrLen (infoString) + elektraStrLen (problemKeys) - 1;
		char * buffer = elektraMalloc (len);
		snprintf (buffer, len, "invalid array members: %s\n", problemKeys);
		elektraMetaArrayAdd (key, "logs/spec/info", buffer);
		elektraFree (buffer);
	}
	break;
	case IGNORE:

		break;
	}
	if (problemKeys)
	{
		elektraFree ((void *) problemKeys);
	}
	return ret;
}

static int handleSubCountConflict (Key * parentKey, Key * key, Key * specKey, Key * conflictMeta, OnConflict onConflict)
{
	int ret = 0;
	switch (onConflict)
	{
	case ERROR:
		ELEKTRA_SET_ERRORF (142, parentKey, "%s has a invalid number of subkeys: %s. Expected: %s\n", keyName (key),
				    keyString (conflictMeta), keyString (keyGetMeta (specKey, "required")));
		ret = -1;
		break;
	case WARNING:
		ELEKTRA_ADD_WARNINGF (143, parentKey, "%s has a invalid number of subkeys: %s. Expected: %s\n", keyName (key),
				      keyString (conflictMeta), keyString (keyGetMeta (specKey, "required")));
		break;
	case INFO:
	{
		const char * infoString = "invalid number of subkeys: %s. Expected %s";
		const size_t len = elektraStrLen (infoString) + MAX_CHARS_IN_LONG * 2;
		char * buffer = elektraMalloc (len);
		snprintf (buffer, len, infoString, keyString (conflictMeta), keyString (keyGetMeta (specKey, "required")));
		elektraMetaArrayAdd (key, "logs/spec/info", buffer);
		elektraFree (buffer);
	}
	break;
	case IGNORE:

		break;
	}
	return ret;
}

static int handleConflictConflict (Key * parentKey, Key * key, Key * conflictMeta, OnConflict onConflict)
{
	ELEKTRA_LOG ("handling conflict %s:%s\n", keyName (key), keyName (conflictMeta));

	int ret = 0;
	const char * problemKeys = elektraMetaArrayToString (key, keyName (conflictMeta), ", ");
	switch (onConflict)
	{
	case ERROR:
		ELEKTRA_SET_ERRORF (142, parentKey, "%s has conflicting metakeys: %s\n", keyName (key), problemKeys);
		ret = -1;
		break;
	case WARNING:
		ELEKTRA_ADD_WARNINGF (143, parentKey, "%s has conflicting metakeys: %s\n", keyName (key), problemKeys);
		break;
	case INFO:
	{
		const char * infoString = "has conflicting metakeys:";
		const size_t len = elektraStrLen (infoString) + elektraStrLen (problemKeys) + elektraStrLen (keyName (key));
		char * buffer = elektraMalloc (len);
		snprintf (buffer, len, "%s %s %s", keyName (key), infoString, problemKeys);
		elektraMetaArrayAdd (key, "logs/spec/info", buffer);
		elektraFree (buffer);
	}
	break;
	case IGNORE:

		break;
	}
	if (problemKeys)
	{
		elektraFree ((void *) problemKeys);
	}
	return ret;
}

static int handleOutOfRangeConflict (Key * parentKey, Key * key, Key * specKey, Key * conflictMeta, OnConflict onConflict)
{
	int ret = 0;
	switch (onConflict)
	{
	case ERROR:
		ELEKTRA_SET_ERRORF (142, parentKey, "%s has invalid number of members: %s. Expected: %s\n", keyName (key),
				    keyString (conflictMeta), keyString (keyGetMeta (specKey, "array")));
		ret = -1;
		break;
	case WARNING:
		ELEKTRA_ADD_WARNINGF (143, parentKey, "%s has invalid number of members: %s. Expected: %s\n", keyName (key),
				      keyString (conflictMeta), keyString (keyGetMeta (specKey, "array")));
		break;
	case INFO:
	{
		const char * infoString = "%s has invalid number of member: %s. Expected: %s";
		const size_t len = elektraStrLen (infoString) + elektraStrLen (keyName (key)) + MAX_CHARS_IN_LONG +
				   keyGetValueSize (keyGetMeta (specKey, "array")) - 2;
		char * buffer = elektraMalloc (len);
		snprintf (buffer, len, infoString, keyName (key), keyString (conflictMeta), keyString (keyGetMeta (specKey, "array")));
		elektraMetaArrayAdd (key, "logs/spec/info", buffer);
		elektraFree (buffer);
	}
	break;
	case IGNORE:

		break;
	}
	return ret;
}

static int handleMissingConflict (Key * parentKey, Key * key, Key * conflictMeta, OnConflict onConflict)
{
	int ret = 0;
	const char * problemKeys = elektraMetaArrayToString (key, keyName (conflictMeta), ", ");
	switch (onConflict)
	{
	case ERROR:
		ELEKTRA_SET_ERRORF (142, parentKey, "%s has missing subkeys: %s\n", keyName (key), problemKeys);
		ret = -1;
		break;
	case WARNING:
		ELEKTRA_ADD_WARNINGF (143, parentKey, "%s has missing subkeys: %s\n", keyName (key), problemKeys);
		break;
	case INFO:
	{
		const char * infoString = "has missing subkeys:";
		const size_t len = elektraStrLen (infoString) + elektraStrLen (problemKeys) + elektraStrLen (keyName (key));
		char * buffer = elektraMalloc (len);
		snprintf (buffer, len, "%s %s %s", keyName (key), infoString, problemKeys);
		elektraMetaArrayAdd (key, "logs/spec/info", buffer);
		elektraFree (buffer);
	}
	break;
	case IGNORE:

		break;
	}
	if (problemKeys)
	{
		elektraFree ((void *) problemKeys);
	}
	return ret;
}

static int handleError (Key * parentKey, Key * key, Key * specKey, Key * conflictMeta, Conflict conflict, ConflictHandling * ch)
{
	int ret = 0;
	switch (conflict)
	{
	case INVALID:
		ret = handleInvalidConflict (parentKey, key, ch->invalid);
		break;
	case ARRAYMEMBER:
		ret = handleArrayConflict (parentKey, key, conflictMeta, ch->member);
		break;
	case SUBCOUNT:
		ret = handleSubCountConflict (parentKey, key, specKey, conflictMeta, ch->count);
		break;
	case CONFLICT:
		ret = handleConflictConflict (parentKey, key, conflictMeta, ch->conflict);
		break;
	case OUTOFRANGE:
		ret = handleOutOfRangeConflict (parentKey, key, specKey, conflictMeta, ch->range);
		break;
	case MISSING:
		ret = handleMissingConflict (parentKey, key, conflictMeta, ch->missing);
	default:
		break;
	}
	return ret;
}

static int handleErrors (Key * parentKey, KeySet * ks, Key * key, Key * specKey, ConflictHandling * ch, Direction dir)
{
	cursor_t cursor = ksGetCursor (ks);
	int ret = 0;
	ConflictHandling * localCh = elektraMalloc (sizeof (ConflictHandling));
	memcpy (localCh, ch, sizeof (ConflictHandling));
	parseLocalConfig (specKey, localCh, dir);
	Key * parentLookup = keyDup (key);
	keySetBaseName (parentLookup, 0);
	Key * parent = ksLookup (ks, parentLookup, KDB_O_NONE);
	keyDel (parentLookup);
	keyRewindMeta (parent);
	Conflict conflict;
	Key * meta;
	while (keyNextMeta (parent) != NULL)
	{
		meta = (Key *) keyCurrentMeta (parent);
		conflict = getConflict (meta);
		if (conflict != NAC)
		{
			ret |= handleError (parentKey, parent, specKey, meta, conflict, localCh);
			keySetMeta (parent, keyName (meta), 0);
		}
		else if (!strncmp (keyName (meta), "conflict/#", 10) || !strncmp (keyName (meta), "conflict/invalid/hasmember/#", 28))
		{
			keySetMeta (parent, keyName (meta), 0);
		}
	}
	keyRewindMeta (key);
	while (keyNextMeta (key) != NULL)
	{
		meta = (Key *) keyCurrentMeta (key);
		conflict = getConflict (meta);
		if (conflict != NAC)
		{
			ret |= handleError (parentKey, key, specKey, meta, conflict, localCh);
			keySetMeta (key, keyName (meta), 0);
		}
		else if (!strncmp (keyName (meta), "conflict/#", 10) || !strncmp (keyName (meta), "conflict/invalid/hasmember/#", 28))
		{
			keySetMeta (key, keyName (meta), 0);
		}
	}
	elektraFree (localCh);
	ksSetCursor (ks, cursor);
	return ret;
}

static void removeMeta (Key * key, Key * specKey, Key * parentKey ELEKTRA_UNUSED)
{
	keyRewindMeta (specKey);
	while (keyNextMeta (specKey) != NULL)
	{
		const Key * meta = keyCurrentMeta (specKey);
		const char * name = keyName (meta);
		if (!(!strcmp (name, "array") || !strcmp (name, "required") || !strncmp (name, "conflict/", 9) ||
		      !strcmp (name, "require")))
		{
			const Key * oldMeta;
			if ((oldMeta = keyGetMeta (key, name)) != NULL)
			{
				keySetMeta (key, name, 0);
			}
		}
	}
}

static void copyMeta (Key * key, Key * specKey, Key * parentKey ELEKTRA_UNUSED)
{
	keyRewindMeta (specKey);
	while (keyNextMeta (specKey) != NULL)
	{
		const Key * meta = keyCurrentMeta (specKey);
		const char * name = keyName (meta);
		if (!(!strncmp (name, "internal/", 9) || !strncmp (name, "conflict/", 9)))
		{
			const Key * oldMeta;
			if ((oldMeta = keyGetMeta (key, name)) != NULL)
			{
				if (strcmp (keyString (oldMeta), keyString (meta)))
				{
					int conflictStringSize = elektraStrLen (name) + elektraStrLen ("conflict/collision/");
					char * conflictName = elektraMalloc (conflictStringSize);
					snprintf (conflictName, conflictStringSize, "conflict/%s", name);
					keySetMeta (key, conflictName, keyString (oldMeta));
					keyCopyMeta (key, specKey, name);
					elektraFree (conflictName);
					elektraMetaArrayAdd (key, "conflict/collision", name);
				}
			}
			else
			{
				keySetMeta (key, name, keyString (meta));
			}
		}
	}
	keySetMeta (key, "spec/internal/valid", 0);
}

static int hasRequired (Key * key, Key * specKey, KeySet * ks)
{
	Key * lookupKey = keyDup (key);
	keyAddBaseName (lookupKey, keyBaseName (specKey));
	Key * found = ksLookup (ks, lookupKey, KDB_O_NONE);
	keyDel (lookupKey);
	if (!found)
	{
		elektraMetaArrayAdd (key, "conflict/missing", keyBaseName (specKey));
		return 0;
	}
	return 1;
}

static int doGlobbing (Key * parentKey, KeySet * returned, KeySet * specKS, ConflictHandling * ch, Direction dir, int clean)
{
	Key * specKey;
	ksRewind (specKS);
	int ret = 1;
	while ((specKey = ksNext (specKS)) != NULL)
	{
		int require = 0;
		char * pattern;
		if (keyGetMeta (specKey, "require"))
		{
			require = 1;
			Key * matchKey = keyDup (specKey);
			keySetBaseName (matchKey, 0);
			pattern = keyNameToMatchingString (matchKey);
			keyDel (matchKey);
		}
		else
		{
			pattern = keyNameToMatchingString (specKey);
		}
		int found = 0;
		ksRewind (returned);
		Key * cur;
		while ((cur = ksNext (returned)) != NULL)
		{
			if (keyGetNamespace (cur) == KEY_NS_SPEC) continue;

			cursor_t cursor = ksGetCursor (returned);
			if (matchPatternToKey (pattern, cur))
			{
				if (!clean)
				{
					found = 1;
					if (require)
					{
						if (hasRequired (cur, specKey, returned)) copyMeta (cur, specKey, parentKey);
					}
					else if (keyGetMeta (cur, "conflict/invalid"))
					{
						copyMeta (cur, specKey, parentKey);
					}
					else if (keyGetMeta (cur, "spec/internal/valid"))
					{
						copyMeta (cur, specKey, parentKey);
					}
					else if (elektraArrayValidateName (cur) == 1)
					{
						validateArray (returned, cur, specKey);
						copyMeta (cur, specKey, parentKey);
					}
					else if (!(strcmp (keyBaseName (specKey), "_")))
					{
						validateWildcardSubs (returned, cur, specKey);
						copyMeta (cur, specKey, parentKey);
					}
					else
					{
						if (hasArray (cur))
						{
							if (isValidArrayKey (cur))
							{
								copyMeta (cur, specKey, parentKey);
							}
						}
						else
						{
							copyMeta (cur, specKey, parentKey);
						}
					}
				}
				else
				{
					removeMeta (cur, specKey, parentKey);
				}
			}
			ksSetCursor (returned, cursor);
		}
		ksRewind (returned);
		if (!found && dir == GET)
		{
			if (keyGetMeta (specKey, "assign/condition")) // hardcoded for now because only "assign/condition" from "assign/*"
								      // currently exists
			{
				Key * newKey = keyNew (strchr (keyName (specKey), '/'), KEY_CASCADING_NAME, KEY_END);
				keySetMeta (newKey, "assign/condition", keyString (keyGetMeta (specKey, "assign/condition")));
				ksAppendKey (returned, keyDup (newKey));
				keyDel (newKey);
			}
			else if (keyGetMeta (specKey,
					     "default")) // hardcoded for now because only "default" from "default/*" currently exists
			{
				Key * newKey = keyNew (strchr (keyName (specKey), '/'), KEY_CASCADING_NAME, KEY_VALUE,
						       keyString (keyGetMeta (specKey, "default")), KEY_END);
				copyMeta (newKey, specKey, parentKey);
				ksAppendKey (returned, keyDup (newKey));
				keyDel (newKey);
			}
		}
		while ((cur = ksNext (returned)) != NULL)
		{
			ret = handleErrors (parentKey, returned, cur, specKey, ch, dir);
			keySetMeta (cur, "conflict/invalid", 0);
		}

		elektraFree (pattern);
	}
	return ret;
}

static void parseConfig (KeySet * config, ConflictHandling * ch)
{
	Key * onConflictConf = NULL;
	while ((onConflictConf = ksNext (config)) != NULL)
	{
		const char * baseName = keyBaseName (onConflictConf);
		if (!strcmp (baseName, "member"))
		{
			ch->member = getConfOption (onConflictConf);
		}
		else if (!strcmp (baseName, "invalid"))
		{
			ch->invalid = getConfOption (onConflictConf);
		}
		else if (!strcmp (baseName, "count"))
		{
			ch->count = getConfOption (onConflictConf);
		}
		else if (!strcmp (baseName, "collision"))
		{
			ch->conflict = getConfOption (onConflictConf);
		}
		else if (!strcmp (baseName, "range"))
		{
			ch->range = getConfOption (onConflictConf);
		}
		else if (!strcmp (baseName, "missing"))
		{
			ch->missing = getConfOption (onConflictConf);
		}
	}
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
#include ELEKTRA_README (spec)
			       keyNew ("system/elektra/modules/spec/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	KeySet * config = elektraPluginGetConfig (handle);
	Key * onConflictConf = ksLookupByName (config, "/conflict/get", KDB_O_NONE);
	OnConflict onConflict = IGNORE;
	ConflictHandling * ch = elektraMalloc (sizeof (ConflictHandling));
	if (onConflictConf)
	{
		const char * onConflictString = keyString (onConflictConf);
		if (!strcmp (onConflictString, "ERROR"))
		{
			onConflict = ERROR;
		}
		else if (!strcmp (onConflictString, "WARNING"))
		{
			onConflict = WARNING;
		}
		else if (!strcmp (onConflictString, "INFO"))
		{
			onConflict = INFO;
		}
		else if (!strcmp (onConflictString, "IGNORE"))
		{
			onConflict = IGNORE;
		}
	}
	int clean = 0;
	PluginConfig * pluginConfig = elektraPluginGetData (handle);
	if (pluginConfig)
	{
		++(pluginConfig->counter);
	}
	else
	{
		pluginConfig = elektraMalloc (sizeof (PluginConfig));
		pluginConfig->counter = 0;
		pluginConfig->ks = NULL;
	}
	if (pluginConfig->counter == 1)
	{
		clean = 1;
		pluginConfig->counter = 0;
	}
	ch->member = onConflict;
	ch->invalid = onConflict;
	ch->count = onConflict;
	ch->conflict = onConflict;
	ch->range = onConflict;
	ch->missing = onConflict;
	KeySet * conflictCut = ksCut (config, onConflictConf);
	parseConfig (conflictCut, ch);
	ksAppend (config, conflictCut);
	ksDel (conflictCut);
	Key * specKey = keyNew ("spec", KEY_END);
	KeySet * specKS = ksCut (returned, specKey);
	if (pluginConfig->ks)
		ksAppend (pluginConfig->ks, specKS);
	else
		pluginConfig->ks = ksDup (specKS);
	elektraPluginSetData (handle, pluginConfig);
	keyDel (specKey);
	KeySet * ks = ksCut (returned, parentKey);
	ksRewind (ks);
	ksRewind (specKS);
	int ret = doGlobbing (parentKey, ks, specKS, ch, GET, clean);
	ksAppend (returned, specKS);
	ksAppend (returned, ks);
	ksDel (ks);
	ksDel (specKS);
	elektraFree (ch);
	ksRewind (returned);
	return ret;
}

int elektraSpecSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	KeySet * config = elektraPluginGetConfig (handle);
	Key * onConflictConf = ksLookupByName (config, "/conflict/set", KDB_O_NONE);
	OnConflict onConflict = IGNORE;
	if (onConflictConf)
	{
		const char * onConflictString = keyName (onConflictConf);
		if (!strcmp (onConflictString, "ERROR"))
		{
			onConflict = ERROR;
		}
		else if (!strcmp (onConflictString, "WARNING"))
		{
			onConflict = WARNING;
		}
		else if (!strcmp (onConflictString, "INFO"))
		{
			onConflict = INFO;
		}
	}
	int clean = 0;
	PluginConfig * pluginConfig = elektraPluginGetData (handle);
	if (pluginConfig)
	{
		++(pluginConfig->counter);
	}
	else
	{
		return 0;
	}
	if (pluginConfig->counter == 2)
	{
		clean = 1;
		pluginConfig->counter = 0;
	}
	ConflictHandling * ch = elektraMalloc (sizeof (ConflictHandling));
	ch->member = onConflict;
	ch->invalid = onConflict;
	ch->count = onConflict;
	ch->conflict = onConflict;
	ch->range = onConflict;
	ch->missing = onConflict;
	KeySet * conflictCut = ksCut (config, onConflictConf);
	parseConfig (conflictCut, ch);
	ksAppend (config, conflictCut);
	ksDel (conflictCut);
	KeySet * specKS = NULL;
	if (pluginConfig) specKS = pluginConfig->ks;
	KeySet * ks = ksCut (returned, parentKey);
	ksRewind (ks);
	int ret = 0;
	if (specKS)
	{
		ksRewind (specKS);
		ret = doGlobbing (parentKey, ks, specKS, ch, SET, clean);
	}
	ksAppend (returned, ks);
	ksDel (ks);
	elektraFree (ch);
	ksRewind (returned);
	elektraPluginSetData (handle, pluginConfig);
	return ret;
}

int elektraSpecClose (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	PluginConfig * pluginConfig = elektraPluginGetData (handle);
	if (!pluginConfig) return 0;
	if (pluginConfig->ks) ksDel (pluginConfig->ks);
	elektraFree (pluginConfig);
	elektraPluginSetData (handle, 0);
	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (spec)
{
	// clang-format off
	return elektraPluginExport ("spec",
			ELEKTRA_PLUGIN_CLOSE, &elektraSpecClose,
			ELEKTRA_PLUGIN_GET,	&elektraSpecGet,
			ELEKTRA_PLUGIN_SET,	&elektraSpecSet,
			ELEKTRA_PLUGIN_END);
}

