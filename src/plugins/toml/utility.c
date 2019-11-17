#include "utility.h"

#include <kdbhelper.h>
#include <kdbassert.h>
#include <stdio.h>
#include <string.h>

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
	if (sscanf (indexStr, "%llu", &val) == EOF)
	{
		ELEKTRA_ASSERT (0, "Could not parse array index");
		return 0;
	}
	return val;
}

bool isEmptyArray(Key * key) {
	Key * meta = findMetaKey (key, "array");
	ELEKTRA_ASSERT (meta != NULL, "Supplied key must have array meta key, but hadn't");
	const char * sizeStr = keyString(meta);
	return elektraStrLen(sizeStr) == 1;
}

size_t getArrayMax (Key * key)
{
	const Key * meta = findMetaKey (key, "array");
	ELEKTRA_ASSERT (meta != NULL, "Supplied key must have array meta key, but hadn't");

	return arrayStringToIndex (keyString(meta));
}

const Key * findMetaKey (Key * key, const char * metakeyName)
{
	keyRewindMeta (key);
	for (const Key * meta = keyNextMeta (key); meta != NULL; meta = keyNextMeta (key))
	{
		if (elektraStrCmp (keyName (meta), metakeyName) == 0)
		{
			return meta;
		}
	}
	return NULL;
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
	return findMetaKey (key, "array") != NULL;
}

bool isInlineTable (Key * key)
{
	return isType (key, "inlinetable");
}

bool isTableArray (Key * key)
{
	return isType (key, "tablearray");
}

bool isType (Key * key, const char * type)
{
	const Key * meta = findMetaKey (key, "type");
	if (meta == NULL)
	{
		return false;
	}
	return elektraStrCmp (keyString (meta), type) == 0;
}

char * getRelativeKeyName (const Key * parent, const Key * key)
{
	if (keyIsBelow (parent, key) <= 0)
	{
		return NULL;
	}
	size_t len = keyGetUnescapedNameSize (key) - keyGetUnescapedNameSize (parent);
	size_t pos = 0;
	char * name = elektraCalloc (sizeof (char) * len);
	const char * keyPart = ((const char *) keyUnescapedName (key)) + keyGetUnescapedNameSize (parent);
	const char * keyStop = ((const char *) keyUnescapedName (key)) + keyGetUnescapedNameSize (key);
	while (keyPart < keyStop)
	{
		strncat (name + pos, keyPart, len);
		pos += elektraStrLen (keyPart) - 1;
		name[pos++] = '.';
		keyPart += elektraStrLen (keyPart);
	}
	if (pos > 0)
	{
		name[pos - 1] = '\0';
	}

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
