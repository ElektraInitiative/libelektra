/**
 * @file prepare.c
 *
 * @brief Contains functionality for preparing a keyset to be written.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include "prepare.h"
#include "utility.h"
#include <kdbassert.h>
#include <kdbhelper.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct ArrayInfo_
{
	ElektraKey * name;
	size_t maxIndex;
	struct ArrayInfo_ * next;
} ArrayInfo;

static void completeKeySetComments (ElektraKeyset * keys);
static void completeKeyComments (ElektraKey * key);
static void addMissingArrayKeys (ElektraKeyset * keys, ElektraKey * parent);
static void pruneInvalidArrayKeys (ElektraKeyset * keys);
static bool orderUnorderedKeys (ElektraKeyset * keys);
static ElektraKeyset * collectUnorderedKeys (ElektraKeyset * ks);
static int getMaxOrder (ElektraKeyset * ks);
static void assignContinuousOrder (ElektraKeyset * ksUnordered, int startOrder);
static ArrayInfo * updateArrayInfo (ArrayInfo * root, ElektraKey * name, size_t index);
static bool predNeedsOrder (ElektraKey * key);

bool prepareKeySet (ElektraKeyset * keys, ElektraKey * parent)
{
	elektraCursor cursor = ksGetCursor (keys);
	addMissingArrayKeys (keys, parent);
	pruneInvalidArrayKeys (keys);
	if (!orderUnorderedKeys (keys))
	{
		ksSetCursor (keys, cursor);
		return false;
	}
	completeKeySetComments (keys);
	ksSetCursor (keys, cursor);
	return false;
}

static void completeKeySetComments (ElektraKeyset * keys)
{
	ksRewind (keys);
	ElektraKey * key;
	while ((key = ksNext (keys)) != NULL)
	{
		completeKeyComments (key);
	}
}

static void completeKeyComments (ElektraKey * key)
{
	keyRewindMeta (key);
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

static bool orderUnorderedKeys (ElektraKeyset * keys)
{
	ksRewind (keys);
	ElektraKeyset * unordered = collectUnorderedKeys (keys);
	if (unordered == NULL)
	{
		return false;
	}
	int maxOrder = getMaxOrder (keys);
	assignContinuousOrder (unordered, maxOrder + 1);
	ksDel (unordered);
	return true;
}

static void addMissingArrayKeys (ElektraKeyset * keys, ElektraKey * parent)
{
	ArrayInfo * arrays = NULL;
	ksRewind (keys);
	ElektraKey * key;
	while ((key = ksNext (keys)) != NULL)
	{
		if (keyCmp (key, parent) == 0)
		{
			continue;
		}
		if (isTableArray (key) && !isArray (key))
		{
			arrays = updateArrayInfo (arrays, key, 0);
		}
		ElektraKey * name = keyNew (keyName (key), KEY_END);
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
		ElektraKey * arrayRoot = ksLookup (keys, arrays->name, 0);
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

static void pruneInvalidArrayKeys (ElektraKeyset * keys)
{
	ksRewind (keys);
	ElektraKeyset * pruneSet = ksNew (8, KS_END);
	ElektraKey * key = ksNext (keys);
	while (key != NULL)
	{
		const ElektraKey * meta = keyGetMeta (key, "array");
		if (meta != NULL)
		{
			ElektraKey * sub;
			while ((sub = ksNext (keys)) != NULL && keyIsBelow (key, sub) == 1)
			{
				char * directSub = getDirectSubKeyName (key, sub);
				if (!isArrayIndex (directSub))
				{
					ksAppendKey (pruneSet, key);
					break;
				}
				elektraFree (directSub);
			}
			key = ksCurrent (keys);
		}
		else
		{
			key = ksNext (keys);
		}
	}
	ksRewind (pruneSet);
	while ((key = ksNext (pruneSet)) != NULL)
	{
		ElektraKey * prune = ksLookup (keys, key, KDB_O_POP);
		ELEKTRA_ASSERT (prune != NULL, "Key must exist in keyset");
		keyDel (prune);
	}
	ksDel (pruneSet);
}

static ArrayInfo * updateArrayInfo (ArrayInfo * root, ElektraKey * name, size_t index)
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

static ElektraKeyset * collectUnorderedKeys (ElektraKeyset * ks)
{
	return keysByPredicate (ks, predNeedsOrder);
}

static void assignContinuousOrder (ElektraKeyset * ksUnordered, int startOrder)
{
	ksRewind (ksUnordered);
	ElektraKey * key;
	while ((key = ksNext (ksUnordered)) != NULL)
	{
		setOrderForKey (key, startOrder++);
	}
}

static int getMaxOrder (ElektraKeyset * ks)
{
	ksRewind (ks);
	int maxOrder = 0;
	ElektraKey * key;
	while ((key = ksNext (ks)) != NULL)
	{
		const ElektraKey * meta = keyGetMeta (key, "order");
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

static bool predNeedsOrder (ElektraKey * key)
{
	return keyGetMeta (key, "order") == NULL && !isArrayIndex (keyBaseName (key));
}
