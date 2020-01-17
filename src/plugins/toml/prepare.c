#include "prepare.h"
#include "utility.h"
#include <kdbassert.h>
#include <kdbhelper.h>
#include <stdlib.h>

typedef struct ArrayInfo_
{
	Key * name;
	size_t maxIndex;
	struct ArrayInfo_ * next;
} ArrayInfo;

static void addMissingArrayKeys (KeySet * keys, Key * parent);
static void pruneInvalidArrayKeys (KeySet * keys);
static bool orderUnorderedKeys(KeySet * keys);
static KeySet * collectUnorderedKeys (KeySet * ks);
static int getMaxOrder (KeySet * ks);
static void assignContinuousOrder (KeySet * ksUnordered, int startOrder);
static ArrayInfo * updateArrayInfo (ArrayInfo * root, Key * name, size_t index);
static bool predNeedsOrder (Key * key);

bool prepareKeySet (KeySet * keys, Key * parent)
{
	cursor_t cursor = ksGetCursor (keys);
	addMissingArrayKeys (keys, parent);
	pruneInvalidArrayKeys (keys);
	if (!orderUnorderedKeys(keys)) {
		ksSetCursor (keys, cursor);
		return false;
	}
	ksSetCursor (keys, cursor);
	return false;
}

static bool orderUnorderedKeys(KeySet * keys) {
	ksRewind(keys);
	KeySet * unordered = collectUnorderedKeys(keys);
	if (unordered == NULL) {
		return false;
	}
	int maxOrder = getMaxOrder(keys);
	assignContinuousOrder (unordered, maxOrder + 1);
	ksDel (unordered);
	return true;
}

static void addMissingArrayKeys (KeySet * keys, Key * parent)
{
	ArrayInfo * arrays = NULL;
	ksRewind (keys);
	Key * key;
	while ((key = ksNext (keys)) != NULL)
	{
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
			const Key * meta = findMetaKey (arrayRoot, "array");
			if (meta == NULL)
			{
				keyUpdateArrayMetakey (arrayRoot, arrays->maxIndex);
			}
			keyDel (arrays->name);
		}
		ArrayInfo * next = arrays->next;
		elektraFree (arrays);
		arrays = next;
	}
}

static void pruneInvalidArrayKeys (KeySet * keys)
{
	ksRewind (keys);
	KeySet * pruneSet = ksNew (8, KS_END);
	Key * key = ksNext (keys);
	while (key != NULL)
	{
		const Key * meta = findMetaKey (key, "array");
		if (meta != NULL)
		{
			Key * sub;
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
		Key * prune = ksLookup (keys, key, KDB_O_POP);
		ELEKTRA_ASSERT (prune != NULL, "Key must exist in keyset");
		keyDel (prune);
	}
	ksDel(pruneSet);
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
	element->name = keyDup (name);
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
	ksRewind (ksUnordered);
	Key * key;
	while ((key = ksNext (ksUnordered)) != NULL)
	{
		setOrderForKey (key, startOrder++);
	}
}

static int getMaxOrder (KeySet * ks)
{
	ksRewind (ks);
	int maxOrder = 0;
	Key * key;
	while ((key = ksNext (ks)) != NULL)
	{
		const Key * meta = findMetaKey (key, "order");
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
	return findMetaKey (key, "order") == NULL && !isArrayIndex (keyBaseName (key));
}
