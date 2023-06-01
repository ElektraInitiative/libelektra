/**
 * @file prepare.c
 *
 * @brief Contains functionality for preparing a keyset to be written.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include "./prepare.h"
#include "./utility.h"
#include <internal/utility/assert.h>
#include <internal/utility/alloc.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct ArrayInfo_
{
	Key * name;
	size_t maxIndex;
	struct ArrayInfo_ * next;
} ArrayInfo;

static void completeKeySetComments (KeySet * keys);
static void completeKeyComments (Key * key);
static void addMissingArrayKeys (KeySet * keys, Key * parent);
static void pruneInvalidArrayKeys (KeySet * keys);
static bool orderUnorderedKeys (KeySet * keys);
static KeySet * collectUnorderedKeys (KeySet * ks);
static int getMaxOrder (KeySet * ks);
static void assignContinuousOrder (KeySet * ksUnordered, int startOrder);
static ArrayInfo * updateArrayInfo (ArrayInfo * root, Key * name, size_t index);
static bool predNeedsOrder (Key * key);

bool prepareKeySet (KeySet * keys, Key * parent)
{
	addMissingArrayKeys (keys, parent);
	pruneInvalidArrayKeys (keys);
	if (!orderUnorderedKeys (keys))
	{
		return false;
	}
	completeKeySetComments (keys);
	return false;
}

static void completeKeySetComments (KeySet * keys)
{
	for (elektraCursor it = 0; it < ksGetSize (keys); ++it)
	{
		Key * key = ksAtCursor (keys, it);
		completeKeyComments (key);
	}
}

static void completeKeyComments (Key * key)
{
	for (size_t index = 0;; index++)
	{
		char * indexStr = indexToArrayString (index);
		char name[48];
		snprintf (name, 48, "meta:/comment/%s", indexStr);
		elektraFree (indexStr);
		if (keyGetMeta (key, name) == NULL)
		{ // Inline comment with index 0 is optional, so don't stop if not present
			if (index == 0)
			{
				continue;
			}
			else
			{
				char nameStart[64];
				snprintf (nameStart, 64, "%s/start", name);
				if (keyGetMeta (key, nameStart) == NULL)
				{
					break;
				}
			}
		}
		else
		{
			char nameExt[64];
			snprintf (nameExt, 64, "%s/start", name);
			if (keyGetMeta (key, nameExt) == NULL)
			{
				keySetMeta (key, nameExt, "# ");
			}
		}
	}
}

static bool orderUnorderedKeys (KeySet * keys)
{
	KeySet * unordered = collectUnorderedKeys (keys);
	if (unordered == NULL)
	{
		return false;
	}
	int maxOrder = getMaxOrder (keys);
	assignContinuousOrder (unordered, maxOrder + 1);
	ksDel (unordered);
	return true;
}

static void addMissingArrayKeys (KeySet * keys, Key * parent)
{
	ArrayInfo * arrays = NULL;

	for (elektraCursor it = 0; it < ksGetSize (keys); ++it)
	{
		Key * key = ksAtCursor (keys, it);
		if (keyCmp (key, parent) == 0)
		{
			continue;
		}
		if (isTableArray (key) && !isArray (key))
		{
			arrays = updateArrayInfo (arrays, key, 0);
		}
		Key * name = keyNew (keyName (key), KEY_END);
		if (name == NULL)
		{
			return;
		}
		do
		{
			if (isArrayIndex (keyBaseName (name)))
			{
				size_t index = arrayStringToIndex (keyBaseName (name));
				keyAddName (name, "..");
				arrays = updateArrayInfo (arrays, name, index);
			}
			else
			{
				keyAddName (name, "..");
			}
		} while (keyCmp (parent, name) != 0);
		keyDel (name);
	}
	while (arrays != NULL)
	{
		Key * arrayRoot = ksLookup (keys, arrays->name, 0);
		if (arrayRoot == NULL)
		{
			keyUpdateArrayMetakey (arrays->name, arrays->maxIndex);
			ksAppendKey (keys, arrays->name);
		}
		else
		{
			keyUpdateArrayMetakey (arrayRoot, arrays->maxIndex);
			keyDel (arrays->name);
		}
		ArrayInfo * next = arrays->next;
		elektraFree (arrays);
		arrays = next;
	}
}

static void pruneInvalidArrayKeys (KeySet * keys)
{
	KeySet * pruneSet = ksNew (8, KS_END);

	for (elektraCursor it = 0; it < ksGetSize (keys);)
	{
		Key * key = ksAtCursor (keys, it);
		const Key * meta = keyGetMeta (key, "array");
		if (meta)
		{
			Key * sub = ksAtCursor (keys, ++it);
			while (sub && keyIsBelow (key, sub) == 1)
			{
				char * directSub = getDirectSubKeyName (key, sub);
				if (!isArrayIndex (directSub))
				{
					ksAppendKey (pruneSet, key);
					break;
				}
				elektraFree (directSub);

				sub = ksAtCursor (keys, ++it);
			}
		}
		else
		{
			++it;
		}
	}

	for (elektraCursor it = 0; it < ksGetSize (pruneSet); ++it)
	{
		Key * key = ksAtCursor (pruneSet, it);
		Key * prune = ksLookup (keys, key, KDB_O_POP);
		ELEKTRA_ASSERT (prune != NULL, "Key must exist in keyset");
		keyDel (prune);
	}
	ksDel (pruneSet);
}

static ArrayInfo * updateArrayInfo (ArrayInfo * root, Key * name, size_t index)
{
	ArrayInfo * ptr = root;
	while (ptr != NULL)
	{
		if (keyCmp (ptr->name, name) == 0)
		{
			if (index > ptr->maxIndex)
			{
				ptr->maxIndex = index;
			}
			return root;
		}
		ptr = ptr->next;
	}
	ArrayInfo * element = elektraCalloc (sizeof (ArrayInfo));
	if (element == NULL)
	{
		return NULL;
	}
	element->name = keyDup (name, KEY_CP_ALL);
	element->maxIndex = index;
	element->next = root;
	return element;
}

static KeySet * collectUnorderedKeys (KeySet * ks)
{
	return keysByPredicate (ks, predNeedsOrder);
}

static void assignContinuousOrder (KeySet * ksUnordered, int startOrder)
{
	for (elektraCursor it = 0; it < ksGetSize (ksUnordered); ++it)
	{
		Key * key = ksAtCursor (ksUnordered, it);
		setOrderForKey (key, startOrder++);
	}
}

static int getMaxOrder (KeySet * ks)
{
	int maxOrder = 0;

	for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
	{
		Key * key = ksAtCursor (ks, it);
		const Key * meta = keyGetMeta (key, "order");
		if (meta != NULL)
		{
			int order = atoi (keyString (meta));
			ELEKTRA_ASSERT (order >= 0, "Only positive order values allowed, but found order %d", order);
			if (order > maxOrder)
			{
				maxOrder = order;
			}
		}
	}
	return maxOrder;
}

static bool predNeedsOrder (Key * key)
{
	return keyGetMeta (key, "order") == NULL && !isArrayIndex (keyBaseName (key));
}
