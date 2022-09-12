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
	elektraCursor cursor = elektraKeysetGetCursor (keys);
	addMissingArrayKeys (keys, parent);
	pruneInvalidArrayKeys (keys);
	if (!orderUnorderedKeys (keys))
	{
		elektraKeysetSetCursor (keys, cursor);
		return false;
	}
	completeKeySetComments (keys);
	elektraKeysetSetCursor (keys, cursor);
	return false;
}

static void completeKeySetComments (ElektraKeyset * keys)
{
	elektraKeysetRewind (keys);
	ElektraKey * key;
	while ((key = elektraKeysetNext (keys)) != NULL)
	{
		completeKeyComments (key);
	}
}

static void completeKeyComments (ElektraKey * key)
{
	elektraKeyRewindMeta (key);
	for (size_t index = 0;; index++)
	{
		char * indexStr = indexToArrayString (index);
		char name[48];
		snprintf (name, 48, "meta:/comment/%s", indexStr);
		elektraFree (indexStr);
		if (elektraKeyGetMeta (key, name) == NULL)
		{ // Inline comment with index 0 is optional, so don't stop if not present
			if (index == 0)
			{
				continue;
			}
			else
			{
				char nameStart[64];
				snprintf (nameStart, 64, "%s/start", name);
				if (elektraKeyGetMeta (key, nameStart) == NULL)
				{
					break;
				}
			}
		}
		else
		{
			char nameExt[64];
			snprintf (nameExt, 64, "%s/start", name);
			if (elektraKeyGetMeta (key, nameExt) == NULL)
			{
				elektraKeySetMeta (key, nameExt, "# ");
			}
		}
	}
}

static bool orderUnorderedKeys (ElektraKeyset * keys)
{
	elektraKeysetRewind (keys);
	ElektraKeyset * unordered = collectUnorderedKeys (keys);
	if (unordered == NULL)
	{
		return false;
	}
	int maxOrder = getMaxOrder (keys);
	assignContinuousOrder (unordered, maxOrder + 1);
	elektraKeysetDel (unordered);
	return true;
}

static void addMissingArrayKeys (ElektraKeyset * keys, ElektraKey * parent)
{
	ArrayInfo * arrays = NULL;
	elektraKeysetRewind (keys);
	ElektraKey * key;
	while ((key = elektraKeysetNext (keys)) != NULL)
	{
		if (elektraKeyCmp (key, parent) == 0)
		{
			continue;
		}
		if (isTableArray (key) && !isArray (key))
		{
			arrays = updateArrayInfo (arrays, key, 0);
		}
		ElektraKey * name = elektraKeyNew (elektraKeyName (key), ELEKTRA_KEY_END);
		if (name == NULL)
		{
			return;
		}
		do
		{
			if (isArrayIndex (elektraKeyBaseName (name)))
			{
				size_t index = arrayStringToIndex (elektraKeyBaseName (name));
				elektraKeyAddName (name, "..");
				arrays = updateArrayInfo (arrays, name, index);
			}
			else
			{
				elektraKeyAddName (name, "..");
			}
		} while (elektraKeyCmp (parent, name) != 0);
		elektraKeyDel (name);
	}
	while (arrays != NULL)
	{
		ElektraKey * arrayRoot = elektraKeysetLookup (keys, arrays->name, 0);
		if (arrayRoot == NULL)
		{
			keyUpdateArrayMetakey (arrays->name, arrays->maxIndex);
			elektraKeysetAppendKey (keys, arrays->name);
		}
		else
		{
			keyUpdateArrayMetakey (arrayRoot, arrays->maxIndex);
			elektraKeyDel (arrays->name);
		}
		ArrayInfo * next = arrays->next;
		elektraFree (arrays);
		arrays = next;
	}
}

static void pruneInvalidArrayKeys (ElektraKeyset * keys)
{
	elektraKeysetRewind (keys);
	ElektraKeyset * pruneSet = elektraKeysetNew (8, ELEKTRA_KS_END);
	ElektraKey * key = elektraKeysetNext (keys);
	while (key != NULL)
	{
		const ElektraKey * meta = elektraKeyGetMeta (key, "array");
		if (meta != NULL)
		{
			ElektraKey * sub;
			while ((sub = elektraKeysetNext (keys)) != NULL && elektraKeyIsBelow (key, sub) == 1)
			{
				char * directSub = getDirectSubKeyName (key, sub);
				if (!isArrayIndex (directSub))
				{
					elektraKeysetAppendKey (pruneSet, key);
					break;
				}
				elektraFree (directSub);
			}
			key = elektraKeysetCurrent (keys);
		}
		else
		{
			key = elektraKeysetNext (keys);
		}
	}
	elektraKeysetRewind (pruneSet);
	while ((key = elektraKeysetNext (pruneSet)) != NULL)
	{
		ElektraKey * prune = elektraKeysetLookup (keys, key, ELEKTRA_KDB_O_POP);
		ELEKTRA_ASSERT (prune != NULL, "Key must exist in keyset");
		elektraKeyDel (prune);
	}
	elektraKeysetDel (pruneSet);
}

static ArrayInfo * updateArrayInfo (ArrayInfo * root, ElektraKey * name, size_t index)
{
	ArrayInfo * ptr = root;
	while (ptr != NULL)
	{
		if (elektraKeyCmp (ptr->name, name) == 0)
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
	element->name = elektraKeyDup (name, ELEKTRA_KEY_CP_ALL);
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
	elektraKeysetRewind (ksUnordered);
	ElektraKey * key;
	while ((key = elektraKeysetNext (ksUnordered)) != NULL)
	{
		setOrderForKey (key, startOrder++);
	}
}

static int getMaxOrder (ElektraKeyset * ks)
{
	elektraKeysetRewind (ks);
	int maxOrder = 0;
	ElektraKey * key;
	while ((key = elektraKeysetNext (ks)) != NULL)
	{
		const ElektraKey * meta = elektraKeyGetMeta (key, "order");
		if (meta != NULL)
		{
			int order = atoi (elektraKeyString (meta));
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
	return elektraKeyGetMeta (key, "order") == NULL && !isArrayIndex (elektraKeyBaseName (key));
}
