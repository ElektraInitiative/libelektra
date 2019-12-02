#include "sort.h"
#include "utility.h"
#include <kdbassert.h>
#include <kdbease.h>
#include <kdbhelper.h>
#include <kdbmeta.h>
#include <stdlib.h>

typedef struct ArrayElement_
{
	Key * element;
	Key * parent;
	struct ArrayElement_ * next;
} ArrayElement;

Key ** mergeWithArrayElements (Key ** ksArray, size_t ksSize, ArrayElement * elementList, size_t elementSize);
static KeySet * collectUnorderedKeys (KeySet * ks);
static void assignContinuousOrder (KeySet * ksUnordered, int startOrder);
static KeySet * extractArrayElements (KeySet * ks);
static int getMaxOrder (KeySet * ks);
static ArrayElement * buildArrayElementList (KeySet * ks, KeySet * elements);
static ArrayElement * appendArrayElement(Key * parent, Key * element, ArrayElement * back);
static void freeArrayElementList(ArrayElement * root);
static Key * getArrayElementParent (KeySet * ks, Key * element);
static int keyCmpOrderWrapper (const void * va, const void * vb);
static bool predNeedsOrder (Key * key);
static bool predIsArray (Key * key);
static bool predIsArrayElement (Key * key);

Key ** sortKeySet (KeySet * ks)
{
	KeySet * unordered = collectUnorderedKeys (ks);
	if (unordered == NULL)
	{
		return NULL;
	}
	int maxOrder = getMaxOrder (ks);
	assignContinuousOrder (unordered, maxOrder + 1);
	ksDel (unordered);

	KeySet * arrayElements = extractArrayElements (ks);
	ArrayElement * elementList = buildArrayElementList(ks, arrayElements);

	size_t ksSize = ksGetSize (ks);
	Key ** ksArray = elektraCalloc (sizeof (Key *) * ksSize);
	if (ksArray == NULL)
	{
		ksDel (arrayElements);
		return NULL;
	}
	if (elektraKsToMemArray (ks, ksArray) < 0)
	{
		ksDel (arrayElements);
		elektraFree (ksArray);
		return NULL;
	}
	qsort (ksArray, ksSize, sizeof (Key *), keyCmpOrderWrapper);
	size_t elementSize = ksGetSize(arrayElements);
	Key ** fullKsArray = mergeWithArrayElements (ksArray, ksSize, elementList, elementSize);
	elektraFree (ksArray);
	ksAppend(ks, arrayElements);
	ksDel (arrayElements);
	freeArrayElementList(elementList);
	return fullKsArray;
}

Key ** mergeWithArrayElements (Key ** ksArray, size_t ksSize, ArrayElement * elementList, size_t elementSize)
{
	size_t fullSize = ksSize + elementSize;
	Key ** fullArray = (Key **) elektraCalloc (sizeof (Key *) * fullSize);
	if (fullArray == NULL)
	{
		return NULL;
	}
	for (size_t i = 0, j = 0; i < ksSize; i++, j++)
	{
		if (!isArray (ksArray[i]))
		{
			fullArray[j] = ksArray[i];
		}
		else
		{
			fullArray[j] = ksArray[i];
			for (ArrayElement * ptr = elementList; ptr != NULL; ptr = ptr->next) {
				if (keyCmp(ksArray[i], ptr->parent) == 0) {
					fullArray[++j] = ptr->element;
					// TODO unlink used elements from list
				}
			}
		}
	}
	return fullArray;
}

static ArrayElement * buildArrayElementList (KeySet * ks, KeySet * elements)
{
	ArrayElement * root = NULL;
	ArrayElement * back = NULL;
	
	ksRewind (elements);
	Key * key;
	while ((key = ksNext (elements)) != NULL)
	{
		Key * parent = getArrayElementParent(ks, key);
		back = appendArrayElement(parent, key, back);
		if (back == NULL) {
			freeArrayElementList(root);
		} else if (root == NULL) {
			root = back;
		}
	}
	return root;
}

static ArrayElement * appendArrayElement(Key * parent, Key * element, ArrayElement * back) {
	ArrayElement * listElement = elektraCalloc (sizeof (ArrayElement));
	if (listElement == NULL) {
		return NULL;
	}
	listElement->element = element;
	listElement->parent = parent;
	if (back != NULL) {
		back->next = listElement;
	}
	return listElement;
}

static void freeArrayElementList(ArrayElement * root) {
	while (root != NULL) {
		ArrayElement * next = root->next;
		elektraFree(root);
		root = next;
	}
}

static Key * getArrayElementParent (KeySet * ks, Key * element)
{
	Key * parentName = keyDup (element);
	Key * parent;
	do
	{
		keyAddName (parentName, "..");
		parent = ksLookup (ks, parentName, 0);
	} while(parent == NULL || !isArray(parent));
	keyDel(parentName);
	return parent;
}
static KeySet * extractArrayKeys (KeySet * ks)
{
	KeySet * arrays = keysByPredicate (ks, predIsArray);
	keySetDiff (ks, arrays);
	return arrays;
}

static KeySet * extractArrayElements (KeySet * ks)
{
	KeySet * arrays = keysByPredicate (ks, predIsArrayElement);
	keySetDiff (ks, arrays);
	return arrays;
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
	return keysByPredicate (ks, predNeedsOrder);
}

static int keyCmpOrderWrapper (const void * va, const void * vb)
{
	return elektraKeyCmpOrder (*((const Key **) va), *((const Key **) vb));
}

static bool predNeedsOrder (Key * key)
{
	return findMetaKey (key, "order") == NULL && !isArrayIndex (keyBaseName (key));
}

static bool predIsArray (Key * key)
{
	return findMetaKey (key, "array") != NULL;
}

static bool predIsArrayElement (Key * key)
{
	return isArrayElement (key);
}
