#include "table_array.h"

#include <kdbhelper.h>

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
	}
	if (ta->keyStr == NULL)
	{
		ta->keyStr = strdup (keyName (key));
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
	// printf ("Determining child fraction of:\n\t%s\n\t%s\n", keyName (parent), keyName (child));
	if (!keyIsBelow (parent, child))
	{
		return NULL;
	}
	else
	{
		Key * childDup = keyDup (child);
		size_t fracSize = 256;
		char * fraction = elektraCalloc (sizeof (char) * fracSize);
		do
		{
			const char * baseName = keyBaseName (childDup);
			if (strlen (fraction) + strlen (baseName) >= fracSize)
			{
				fracSize *= 2;
				elektraRealloc ((void **) &fraction, fracSize);
			}
			char * fracDup = strdup (fraction); // TODO: avoid allocation
			snprintf (fraction, fracSize, "%s/%s", baseName, fracDup);
			elektraFree (fracDup);
			keyAddName (childDup, "..");
		} while (keyRel (parent, childDup) != 0);
		fraction[strlen (fraction) - 1] = 0;
		elektraRealloc ((void **) &fraction, strlen (fraction) + 1);
		keyDel (childDup);
		// printf ("got fraction: %s\n", fraction);
		return fraction;
	}
}
