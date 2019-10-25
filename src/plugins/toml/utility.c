#include "utility.h"

#include <kdbhelper.h>
#include <stdio.h>

Key * keyAppendIndex (size_t index, const Key * parent)
{
	// Key * indexKey = keyDup (parent);
	Key * indexKey = keyNew (keyName (parent), KEY_END);

	char * indexStr = indexToArrayString (index);
	keyAddBaseName (indexKey, indexStr);
	elektraFree (indexStr);
	return indexKey;
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
	char * str = (char*) elektraCalloc (sizeof (char) * strLen);
	memset (str, '_', sizeof (char) * strLen);
	str[0] = '#';
	str[strLen - 1] = 0;
	snprintf (str + 1 + (digits - 1), strLen, "%lu", index);
	return str;
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
