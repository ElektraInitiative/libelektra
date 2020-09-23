/**
 * @file utility.h
 *
 * @brief Contains functionality for handling Keys used throughout writing and reading.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "utility.h"

#include <kdbassert.h>
#include <kdbhelper.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void dumpKS (KeySet * keys)
{
	printf ("DUMPING KS, size = %lu\n", ksGetSize (keys));
	FILE * f = stdout; // fopen ("keys.txt", "w");
	ksRewind (keys);
	for (Key * key = ksNext (keys); key != NULL; key = ksNext (keys))
	{
		fprintf (f, "KEY = %s, VALUE = %s\n", keyName (key), keyString (key));
		keyRewindMeta (key);
		for (const Key * meta = keyNextMeta (key); meta != NULL; meta = keyNextMeta (key))
		{
			fprintf (f, "\tMETA KEY = %s, VALUE = %s\n", keyName (meta), keyString (meta));
		}
	}
	ksRewind (keys);
}

void dumpMemKS (Key ** keys, size_t size)
{
	printf ("DUMPING KS, size = %lu\n", size);
	FILE * f = stdout; // fopen ("keys_mem.txt", "w");
	for (size_t i = 0; i < size; i++)
	{
		Key * key = keys[i];
		fprintf (f, "KEY = %s, VALUE = %s\n", keyName (key), keyString (key));
		keyRewindMeta (key);
		for (const Key * meta = keyNextMeta (key); meta != NULL; meta = keyNextMeta (key))
		{
			fprintf (f, "\tMETA KEY = %s, VALUE = %s\n", keyName (meta), keyString (meta));
		}
	}
}

Key * keyAppendIndex (size_t index, const Key * parent)
{
	// Key * indexKey = keyDup (parent);
	Key * indexKey = keyNew (keyName (parent), KEY_END);

	char * indexStr = indexToArrayString (index);
	keyAddBaseName (indexKey, indexStr);
	elektraFree (indexStr);
	return indexKey;
}

void keyUpdateArrayMetakey (Key * key, size_t newIndex)
{
	char * indexStr = indexToArrayString (newIndex);
	keySetMeta (key, "array", indexStr);
	elektraFree (indexStr);
}

char * indexToArrayString (size_t index)
{
	size_t digits = 1;
	for (size_t value = index; value > 9; digits++)
	{
		value /= 10;
	}
	int strLen = 1 +	    //  '#'
		     (digits - 1) + // underscores
		     digits +	    // actual digits
		     1;		    // '\0'
	char * str = (char *) elektraCalloc (sizeof (char) * strLen);
	memset (str, '_', sizeof (char) * strLen);
	str[0] = '#';
	str[strLen - 1] = 0;
	snprintf (str + 1 + (digits - 1), strLen, "%lu", index);
	return str;
}

size_t arrayStringToIndex (const char * indexStr)
{
	if (*indexStr++ != '#')
	{
		return 0;
	}
	while (*indexStr == '_')
	{
		indexStr++;
	}
	size_t val = 0;
	if (sscanf (indexStr, "%lu", &val) == EOF)
	{
		ELEKTRA_ASSERT (0, "Could not parse array index");
		return 0;
	}
	return val;
}

bool isArrayIndex (const char * basename)
{
	if (*basename++ != '#')
	{
		return false;
	}
	while (*basename == '_')
	{
		basename++;
	}
	while (*basename >= '0' && *basename <= '9')
	{
		basename++;
	}
	return *basename == 0;
}

bool isArrayElement (const Key * key)
{
	const char * part = (const char *) keyUnescapedName (key);
	const char * stop = part + keyGetUnescapedNameSize (key);
	while (part < stop)
	{
		if (isArrayIndex (part))
		{
			return true;
		}
		part += elektraStrLen (part);
	}
	return false;
}

bool isEmptyArray (Key * key)
{
	const Key * meta = keyGetMeta (key, "array");
	ELEKTRA_ASSERT (meta != NULL, "Supplied key must have array meta key, but hadn't");
	const char * sizeStr = keyString (meta);
	return elektraStrLen (sizeStr) == 1;
}

size_t getArrayMax (Key * key)
{
	const Key * meta = keyGetMeta (key, "array");
	ELEKTRA_ASSERT (meta != NULL, "Supplied key must have array meta key, but hadn't");

	return arrayStringToIndex (keyString (meta));
}

void setPlainIntMeta (Key * key, const char * metaKeyName, size_t value)
{
	char * str = intToStr (value);
	keySetMeta (key, metaKeyName, str);
	elektraFree (str);
}

char * intToStr (size_t i)
{
	char * str = (char *) elektraCalloc (sizeof (char) * 40);
	snprintf (str, 40, "%lu", i);
	return str;
}

void setOrderForKey (Key * key, size_t order)
{
	setPlainIntMeta (key, "order", order);
}

bool isArray (Key * key)
{
	return keyGetMeta (key, "array") != NULL;
}

bool isInlineTable (Key * key)
{
	return isTomlType (key, "inlinetable");
}

bool isSimpleTable (Key * key)
{
	return isTomlType (key, "simpletable");
}

bool isTableArray (Key * key)
{
	return isTomlType (key, "tablearray");
}

bool isTomlType (Key * key, const char * type)
{
	const Key * meta = keyGetMeta (key, "tomltype");
	if (meta == NULL)
	{
		return false;
	}
	return elektraStrCmp (keyString (meta), type) == 0;
}

bool isBareString (const char * str)
{
	while (*str != 0)
	{
		if (!((*str >= '0' && *str <= '9') || (*str >= 'a' && *str <= 'z') || (*str >= 'A' && *str <= 'Z') || *str == '-' ||
		      *str == '_'))
		{
			return false;
		}
		str++;
	}
	return true;
}

char * getRelativeName (Key * parent, Key * key)
{
	size_t nameSize = 64;
	size_t pos = 0;
	char * name = (char *) elektraCalloc (sizeof (char) * nameSize);
	bool placeDot = false;
	const char * keyPart = ((const char *) keyUnescapedName (key)) + keyGetUnescapedNameSize (parent);
	const char * keyStop = ((const char *) keyUnescapedName (key)) + keyGetUnescapedNameSize (key);

	if (isTableArray (parent))
	{ // skip array index
		keyPart += elektraStrLen (keyPart);
	}
	while (keyPart < keyStop)
	{
		if (placeDot)
		{
			if (pos == nameSize)
			{
				nameSize *= 2;
				if (elektraRealloc ((void **) &name, nameSize) < 0)
				{
					elektraFree (name);
					return NULL;
				}
			}
			name[pos++] = '.';
		}
		else
		{
			placeDot = true;
		}
		// Consider empty strings (only consisting of a zero terminator) non-bare, so that empty quotes get emitted
		bool bare = *keyPart != 0 && isBareString (keyPart);
		if (!bare)
		{
			if (pos == nameSize)
			{
				nameSize *= 2;
				if (elektraRealloc ((void **) &name, nameSize) < 0)
				{
					elektraFree (name);
					return NULL;
				}
			}
			name[pos++] = '"';
		}
		if (pos + elektraStrLen (keyPart) >= nameSize)
		{
			nameSize *= 2;
			if (elektraRealloc ((void **) &name, nameSize) < 0)
			{
				elektraFree (name);
				return NULL;
			}
		}
		strncat (&name[pos], keyPart, nameSize - pos);
		pos += elektraStrLen (keyPart) - 1;
		if (!bare)
		{
			if (pos == nameSize)
			{
				nameSize *= 2;
				if (elektraRealloc ((void **) &name, nameSize) < 0)
				{
					elektraFree (name);
					return NULL;
				}
			}
			name[pos++] = '"';
		}
		keyPart += elektraStrLen (keyPart);
	}
	name[pos] = 0;
	return name;
}

char * getDirectSubKeyName (const Key * parent, const Key * key)
{
	if (keyIsBelow (parent, key) <= 0)
	{
		return NULL;
	}
	const char * keyPart = ((const char *) keyUnescapedName (key)) + keyGetUnescapedNameSize (parent);
	return elektraStrDup (keyPart);
}

void keySetDiff (KeySet * whole, KeySet * part)
{
	if (whole == NULL || part == NULL)
	{
		return;
	}
	ksRewind (part);
	Key * key;
	while ((key = ksNext (part)) != NULL)
	{
		ksLookup (whole, key, KDB_O_POP);
	}
}

KeySet * keysByPredicate (KeySet * ks, bool (*pred) (Key *))
{
	KeySet * predicateKeys = ksNew (0, KS_END);
	if (predicateKeys == NULL)
	{
		return NULL;
	}
	ksRewind (ks);
	Key * key;
	while ((key = ksNext (ks)) != NULL)
	{
		if ((*pred) (key))
		{
			ksAppendKey (predicateKeys, key);
		}
	}
	return predicateKeys;
}

KeySet * collectSubKeys (KeySet * ks, Key * parent)
{
	KeySet * subKeys = ksNew (0, KS_END);
	ksRewind (ks);
	Key * key;
	while ((key = ksNext (ks)) != NULL)
	{
		if (keyIsBelow (parent, key) == 1)
		{
			ksAppendKey (subKeys, key);
		}
	}
	return subKeys;
}

KeySet * extractSubKeys (KeySet * ks, Key * parent)
{
	KeySet * sub = collectSubKeys (ks, parent);
	keySetDiff (ks, sub);
	return sub;
}

bool isLeaf (Key * leafCandidate, KeySet * ks)
{
	elektraCursor cursor = ksGetCursor (ks);
	ksRewind (ks);
	Key * key;
	while ((key = ksNext (ks)) != NULL)
	{
		if (keyIsBelow (leafCandidate, key) == 1)
		{
			ksSetCursor (ks, cursor);
			return false;
		}
	}
	ksSetCursor (ks, cursor);
	return true;
}

bool isBase64String (const char * str)
{
	const char * prefix = "@BASE64";
	if (elektraStrLen (str) < elektraStrLen (prefix))
	{
		return false;
	}
	for (size_t i = 0; i < elektraStrLen (prefix) - 1; i++)
	{
		if (str[i] != prefix[i])
		{
			return false;
		}
	}
	return true;
}

bool isNullString (const char * str)
{
	const char * nullIndicator = "@NULL";
	return elektraStrCmp (str, nullIndicator) == 0;
}
