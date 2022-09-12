/**
 * @file utility.h
 *
 * @brief Contains functionality for handling Keys used throughout writing and reading.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "utility.h"

#include <kdbassert.h>
#include <kdbease.h>
#include <kdbhelper.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

ElektraKey * keyAppendIndex (size_t index, const ElektraKey * parent)
{
	// Key * indexKey = keyDup (parent);
	ElektraKey * indexKey = keyNew (keyName (parent), KEY_END);

	char * indexStr = indexToArrayString (index);
	keyAddBaseName (indexKey, indexStr);
	elektraFree (indexStr);
	return indexKey;
}

void keyUpdateArrayMetakey (ElektraKey * key, size_t newIndex)
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
	return elektraArrayValidateBaseNameString (basename) == 1;
}

bool isArrayElement (const ElektraKey * key)
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

bool isEmptyArray (ElektraKey * key)
{
	const ElektraKey * meta = keyGetMeta (key, "array");
	ELEKTRA_ASSERT (meta != NULL, "Supplied key must have array meta key, but hadn't");
	const char * sizeStr = keyString (meta);
	return elektraStrLen (sizeStr) == 1;
}

size_t getArrayMax (ElektraKey * key)
{
	const ElektraKey * meta = keyGetMeta (key, "array");
	ELEKTRA_ASSERT (meta != NULL, "Supplied key must have array meta key, but hadn't");

	return arrayStringToIndex (keyString (meta));
}

void setPlainIntMeta (ElektraKey * key, const char * metaKeyName, size_t value)
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

void setOrderForKey (ElektraKey * key, size_t order)
{
	setPlainIntMeta (key, "order", order);
}

bool isArray (ElektraKey * key)
{
	return keyGetMeta (key, "array") != NULL;
}

bool isInlineTable (ElektraKey * key)
{
	return isTomlType (key, "inlinetable");
}

bool isSimpleTable (ElektraKey * key)
{
	return isTomlType (key, "simpletable");
}

bool isTableArray (ElektraKey * key)
{
	return isTomlType (key, "tablearray");
}

bool isTomlType (ElektraKey * key, const char * type)
{
	const ElektraKey * meta = keyGetMeta (key, "tomltype");
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

char * getRelativeName (ElektraKey * parent, ElektraKey * key)
{
	size_t nameSize = 64;
	size_t pos = 0;
	char * name = (char *) elektraCalloc (sizeof (char) * nameSize);
	bool placeDot = false;
	const char * keyPart;
	const char * keyStop;

	if (keyGetUnescapedNameSize (parent) == 3)
	{
		// root key -> needs special treatment
		keyPart = ((const char *) keyUnescapedName (key)) + 2;
		keyStop = ((const char *) keyUnescapedName (key)) + keyGetUnescapedNameSize (key);
	}
	else
	{
		keyPart = ((const char *) keyUnescapedName (key)) + keyGetUnescapedNameSize (parent);
		keyStop = ((const char *) keyUnescapedName (key)) + keyGetUnescapedNameSize (key);
	}


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

char * getDirectSubKeyName (const ElektraKey * parent, const ElektraKey * key)
{
	if (keyIsBelow (parent, key) <= 0)
	{
		return NULL;
	}
	const char * keyPart = ((const char *) keyUnescapedName (key)) + keyGetUnescapedNameSize (parent);
	return elektraStrDup (keyPart);
}

void keySetDiff (ElektraKeyset * whole, ElektraKeyset * part)
{
	if (whole == NULL || part == NULL)
	{
		return;
	}
	ksRewind (part);
	ElektraKey * key;
	while ((key = ksNext (part)) != NULL)
	{
		ksLookup (whole, key, KDB_O_POP);
	}
}

ElektraKeyset * keysByPredicate (ElektraKeyset * ks, bool (*pred) (ElektraKey *))
{
	ElektraKeyset * predicateKeys = ksNew (0, KS_END);
	if (predicateKeys == NULL)
	{
		return NULL;
	}
	ksRewind (ks);
	ElektraKey * key;
	while ((key = ksNext (ks)) != NULL)
	{
		if ((*pred) (key))
		{
			ksAppendKey (predicateKeys, key);
		}
	}
	return predicateKeys;
}

ElektraKeyset * collectSubKeys (ElektraKeyset * ks, ElektraKey * parent)
{
	ElektraKeyset * subKeys = ksNew (0, KS_END);
	ksRewind (ks);
	ElektraKey * key;
	while ((key = ksNext (ks)) != NULL)
	{
		if (keyIsBelow (parent, key) == 1)
		{
			ksAppendKey (subKeys, key);
		}
	}
	return subKeys;
}

ElektraKeyset * extractSubKeys (ElektraKeyset * ks, ElektraKey * parent)
{
	ElektraKeyset * sub = collectSubKeys (ks, parent);
	keySetDiff (ks, sub);
	return sub;
}

bool isLeaf (ElektraKey * leafCandidate, ElektraKeyset * ks)
{
	elektraCursor cursor = ksGetCursor (ks);
	ksRewind (ks);
	ElektraKey * key;
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
