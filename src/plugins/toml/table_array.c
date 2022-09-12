/**
 * @file table_array.c
 *
 * @brief Contains functionaly for handling table arrays on reading.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "table_array.h"

#include <kdbhelper.h>
#include <stdio.h>
#include <string.h>

#include "utility.h"

static char * getChildFraction (const ElektraKey * parent, const ElektraKey * child);

TableArrayList * pushTableArray (TableArrayList * top, ElektraKey * key)
{
	TableArrayList * ta = elektraCalloc (sizeof (TableArrayList));
	ta->key = key;
	elektraKeyIncRef (key);
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
		ta->keyStr = elektraStrDup (elektraKeyName (key));
	}
	ta->currIndex = 0;
	ta->next = top;

	return ta;
}

TableArrayList * popTableArray (TableArrayList * top)
{
	TableArrayList * newTop = top->next;
	elektraKeyDecRef (top->key);
	elektraKeyDel (top->key);
	elektraFree (top->keyStr);
	elektraFree (top);
	return newTop;
}

ElektraKey * buildTableArrayKeyName (const TableArrayList * ta)
{
	if (ta->next == NULL || !elektraKeyIsBelow (ta->next->key, ta->key))
	{
		return keyAppendIndex (ta->currIndex, ta->key);
	}
	else
	{
		ElektraKey * key = buildTableArrayKeyName (ta->next);
		elektraKeyAddName (key, ta->keyStr);
		char * index = indexToArrayString (ta->currIndex);
		elektraKeyAddBaseName (key, index);
		elektraFree (index);
		return key;
	}
}

static char * getChildFraction (const ElektraKey * parent, const ElektraKey * child)
{
	if (!elektraKeyIsBelow (parent, child))
	{
		return NULL;
	}
	else
	{
		ElektraKey * childDup = elektraKeyDup (child, ELEKTRA_KEY_CP_ALL);
		size_t fracSize = 256;
		char * fraction = elektraCalloc (sizeof (char) * fracSize);
		if (fraction == NULL)
		{
			return NULL;
		}
		do
		{
			const char * baseName = elektraKeyBaseName (childDup);
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
			elektraKeyAddName (childDup, "..");
		} while (elektraKeyCmp (parent, childDup) != 0);
		fraction[elektraStrLen (fraction) - 2] = 0;
		elektraKeyDel (childDup);
		return fraction;
	}
}
