/**
 * @file table_array.c
 *
 * @brief Contains functionaly for handling table arrays on reading.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "table_array.h"

#include <internal/utility/old_helper.h>
#include <stdio.h>
#include <string.h>

#include "utility.h"

static char * getChildFraction (const Key * parent, const Key * child);

TableArrayList * pushTableArray (TableArrayList * top, Key * key)
{
	TableArrayList * ta = elektraCalloc (sizeof (TableArrayList));
	ta->key = key;
	keyIncRef (key);
	if (top != NULL)
	{
		ta->keyStr = getChildFraction (top->key, key);
		if (ta->keyStr == NULL)
		{
			return NULL;
		}
	}
	if (ta->keyStr == NULL)
	{
		ta->keyStr = elektraStrDup (keyName (key));
	}
	ta->currIndex = 0;
	ta->next = top;

	return ta;
}

TableArrayList * popTableArray (TableArrayList * top)
{
	TableArrayList * newTop = top->next;
	keyDecRef (top->key);
	keyDel (top->key);
	elektraFree (top->keyStr);
	elektraFree (top);
	return newTop;
}

Key * buildTableArrayKeyName (const TableArrayList * ta)
{
	if (ta->next == NULL || !keyIsBelow (ta->next->key, ta->key))
	{
		return keyAppendIndex (ta->currIndex, ta->key);
	}
	else
	{
		Key * key = buildTableArrayKeyName (ta->next);
		keyAddName (key, ta->keyStr);
		char * index = indexToArrayString (ta->currIndex);
		keyAddBaseName (key, index);
		elektraFree (index);
		return key;
	}
}

static char * getChildFraction (const Key * parent, const Key * child)
{
	if (!keyIsBelow (parent, child))
	{
		return NULL;
	}
	else
	{
		Key * childDup = keyDup (child, KEY_CP_ALL);
		size_t fracSize = 256;
		char * fraction = elektraCalloc (sizeof (char) * fracSize);
		if (fraction == NULL)
		{
			return NULL;
		}
		do
		{
			const char * baseName = keyBaseName (childDup);
			if (elektraStrLen (fraction) + elektraStrLen (baseName) - 1 >= fracSize)
			{
				fracSize *= 2;
				size_t oldLen = elektraStrLen (fraction);
				if (elektraRealloc ((void **) &fraction, fracSize) < 0)
				{
					return NULL;
				}
				memset (fraction + oldLen, 0, fracSize - oldLen);
			}
			char * fracDup = elektraStrDup (fraction); // TODO: avoid allocation
			snprintf (fraction, fracSize, "%s/%s", baseName, fracDup);
			elektraFree (fracDup);
			keyAddName (childDup, "..");
		} while (keyCmp (parent, childDup) != 0);
		fraction[elektraStrLen (fraction) - 2] = 0;
		keyDel (childDup);
		return fraction;
	}
}
