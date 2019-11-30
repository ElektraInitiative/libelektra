#include "sort.h"
#include "utility.h"
#include <kdbassert.h>
#include <kdbhelper.h>
#include <stdlib.h>

typedef struct ArrayStack_
{
	Key * root;
	int order;
	struct ArrayStack_ * next;
} ArrayStack;


static KeySet * collectUnorderedKeys (KeySet * ks);
static KeySet * extractUnorderedArrayElements (KeySet * unorderedKs);
static void assignContinuousOrder (KeySet * ksUnordered, int startOrder);
static void assignElementsOrder (KeySet * ksElements, KeySet * ks);
static int getMaxOrder (KeySet * ks);

Key ** sortKeySet (KeySet * ks)
{
	KeySet * unordered = collectUnorderedKeys (ks);
	if (unordered == NULL)
	{
		return NULL;
	}
	KeySet * unorderedArrayElements = extractUnorderedArrayElements (unordered);
	if (unorderedArrayElements == NULL)
	{
		return NULL;
	}
	int maxOrder = getMaxOrder (ks);
	assignContinuousOrder (unordered, maxOrder + 1);
	ksDel (unordered);
}

static void assignElementsOrder (KeySet * ksElements, KeySet * ks)
{
	ksRewind (ks);
	int currOrder = 0;
	Key * key;
	Key * arrayParent = NULL;
	while ((key = ksNext (ks)) != NULL)
	{
		if (arrayParent == NULL || keyIsDirectlyBelow (arrayParent, key) == 0)
		{
			Key * parentName = keyDup (key);
			keyAddName (parentName, "..");
			arrayParent = ksLookup (ks, parentName, 0);
			if (arrayParent == NULL)
			{
				arrayParent = ksLookup (ksElements, parentName, 0); // -> nested arrays
			}
			ELEKTRA_ASSERT (arrayParent != NULL, "Array parent must be existent in full ks or element ks, but not found");
			const Key * meta = findMetaKey (arrayParent, "order");
			ELEKTRA_ASSERT (meta != NULL, "Array parent key must have an order meta key, but none found");
			currOrder = atoi (keyString (meta));
		}
	}
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

static KeySet * collectUnorderedKeys (KeySet * ks)
{
	ksRewind (ks);
	KeySet * unordered = ksNew (0, KS_END);
	if (unordered == NULL)
	{
		return NULL;
	}
	Key * key;
	while ((key = ksNext (ks)) != NULL)
	{
		if (findMetaKey (key, "order") == NULL)
		{
			ksAppendKey (unordered, key);
		}
	}
	return unordered;
}

static KeySet * extractUnorderedArrayElements (KeySet * unorderedKs)
{
	ksRewind (unorderedKs);
	KeySet * elements = ksNew (0, KS_END);
	if (elements == NULL)
	{
		return NULL;
	}
	Key * key;
	while ((key = ksNext (unorderedKs)) != NULL)
	{
		if (isArrayIndex (keyBaseName (key)))
		{
			ksAppendKey (elements, key);
		}
	}
	ksRewind (elements);
	while ((key = ksNext (elements)) != NULL)
	{
		ksLookup (unorderedKs, key, KDB_O_POP);
	}
	return elements;
}
