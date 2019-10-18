#include <stdint.h>
#include <stdlib.h>

#include <kdb.h>
#include <kdbhelper.h>

#include "driver.h"
#include "parser.h"

extern int yyparse (Driver * driver);
extern FILE * yyin;

static void pushCurrKey (Driver * driver);
static void setCurrKey (Driver * driver, const Key * parent);
static void resetCurrKey (Driver * driver);
static void extendCurrKey (Driver * driver, const char * name);
static char * getChildFraction (const Key * parent, const Key * child);
static Key * buildTableArrayKeyName (const TableArray * ta);
static TableArray * pushTableArray (TableArray * top, Key * key);
static TableArray * popTableArray (TableArray * top);
static ParentList * pushParent (ParentList * top, Key * key);
static ParentList * popParent (ParentList * top);
static IndexList * pushIndex (IndexList * top, int value);
static IndexList * popIndex (IndexList * top);
static Key * indexToKey (size_t index, const Key * parent);
static char * indexToArrayString (size_t index);

Driver * createDriver (const Key * parent)
{
	Driver * driver = elektraCalloc (sizeof (Driver));
    memset (driver, 0, sizeof(Driver));
	driver->root = keyDup (parent);
	driver->parentStack = pushParent (NULL, keyDup (parent));
	driver->filename = keyString (parent);
	driver->tableActive = 0;
	return driver;
}

int driverParse (Driver * driver, KeySet * returned)
{
	driver->keys = returned;
	driver->file = fopen (driver->filename, "rb");
	if (driver->file == NULL)
	{
		driverError (driver, 0, "Could not open file");
		return 1;
	}
	yyin = driver->file;
	ksAppendKey (driver->keys, driver->root);
	return yyparse (driver);
}

void driverError (Driver * driver, int lineno, const char * msg)
{
	// TODO: proper error handling
	if (lineno > 0)
	{
		printf ("[ERROR] @ %d: %s\n", lineno, msg);
	}
	else
	{
		printf ("[ERROR] %s\n", msg);
	}
}

void driverEnterKey (Driver * driver)
{
	resetCurrKey (driver);
}

void driverExitKey (Driver * driver)
{
	pushCurrKey (driver);
}

void driverExitKeyValue (Driver * driver)
{
	driver->parentStack = popParent (driver->parentStack);
}

void driverExitSimpleKey (Driver * driver, const Scalar * name)
{
	extendCurrKey (driver, name->str);
}

void driverExitScalar (Driver * driver, Scalar * scalar)
{
	keySetString (driver->parentStack->key, scalar->str);
	ksAppendKey (driver->keys, driver->parentStack->key);
	printf ("Added Scalar: %s -> %s (%s)\n", keyName (driver->parentStack->key), keyString (driver->parentStack->key), scalar->str);
}

void driverEnterSimpleTable (Driver * driver)
{
	resetCurrKey (driver);
	if (driver->tableActive == 0)
	{
		driver->tableActive = 1;
	}
}

void driverExitSimpleTable (Driver * driver)
{
	pushCurrKey (driver);
	keySetMeta (driver->parentStack->key, "simpletable", "");
	ksAppendKey (driver->keys, driver->parentStack->key);
	printf ("Setting metakey for %s: 'simpletable'\n", keyName (driver->parentStack->key));
}


void driverEnterTableArray (Driver * driver)
{
	if (driver->tableActive != 0)
	{
		driver->parentStack = popParent (driver->parentStack);
		driver->tableActive = 0;
	}
	if (driver->tableArrayStack != NULL)
	{
		driver->parentStack = popParent (driver->parentStack); // pop old table array key
	}
	setCurrKey (driver, driver->root);
}

void driverExitTableArray (Driver * driver)
{
	if (driver->tableArrayStack == NULL)
	{
		driver->tableArrayStack = pushTableArray (driver->tableArrayStack, driver->currKey);

		char * indexStr = indexToArrayString (driver->tableArrayStack->currIndex);
		keySetMeta (driver->tableArrayStack->key, "array", indexStr);
		elektraFree (indexStr);

		Key * key = buildTableArrayKeyName (driver->tableArrayStack);

		driver->parentStack = pushParent (driver->parentStack, key);
	}
	else
	{
		int rel = keyRel (driver->tableArrayStack->key, driver->currKey);
		printf ("Table array existing, relation:\n\t%s\n\t%s\n\trel = %d\n", keyName (driver->tableArrayStack->key),
			keyName (driver->currKey), rel);
		if (rel == 0) // same table array name -> next element
		{
			driver->tableArrayStack->currIndex++;
		}
		else if (rel > 0) // below top name -> push new sub table array
		{
			driver->tableArrayStack = pushTableArray (driver->tableArrayStack, driver->currKey);
		}
		else if (rel < 0) // no relation, pop table array stack until some relation exists (or NULL)
		{
			while (driver->tableArrayStack != NULL && keyRel (driver->tableArrayStack->key, driver->currKey) < 0)
			{
				driver->tableArrayStack = popTableArray (driver->tableArrayStack);
			}
			if (driver->tableArrayStack == NULL)
			{
				driver->tableArrayStack = pushTableArray (driver->tableArrayStack, driver->currKey);
			}
			else
			{
				driver->tableArrayStack->currIndex++;
			}
		}

		Key * key = buildTableArrayKeyName (driver->tableArrayStack);

		char * indexStr = indexToArrayString (driver->tableArrayStack->currIndex);
        Key * arrayRoot = keyDup(key);
        keyAddName (arrayRoot, "..");
        keySetMeta (arrayRoot, "array", indexStr);
		elektraFree (indexStr);
        ksAppendKey (driver->keys, arrayRoot);

		driver->parentStack = pushParent (driver->parentStack, key);
		// ksAppendKey (driver->keys, key);
	}
}

void driverEnterArray (Driver * driver)
{
	printf ("entering array\n");
	driver->indexStack = pushIndex (driver->indexStack, 0);
	keySetMeta (driver->parentStack->key, "array", "");
}

void driverExitArray (Driver * driver)
{
	printf ("exiting array: %s,  max_index = %s\n", keyName (driver->parentStack->key),
		keyString (keyGetMeta (driver->parentStack->key, "array")));
	driver->indexStack = popIndex (driver->indexStack);
	ksAppendKey (driver->keys, driver->parentStack->key);
}

void driverEnterArrayElement (Driver * driver)
{
	if (driver->indexStack->value == SIZE_MAX)
	{
		driverError (driver, 0, "Array index at maximum range of size_t: SIZE_MAX");
		return;
	}

	Key * key = keyNew (keyName (driver->parentStack->key), KEY_END);

	char * indexStr = indexToArrayString (driver->indexStack->value);
	keyAddBaseName (key, indexStr);
	elektraFree (indexStr);

	keySetMeta (driver->parentStack->key, "array", keyBaseName (key));
	driver->parentStack = pushParent (driver->parentStack, key);

	driver->indexStack->value++;
}

void driverExitArrayElement (Driver * driver)
{
	driver->parentStack = popParent (driver->parentStack);
}

static void pushCurrKey (Driver * driver)
{
	driver->parentStack = pushParent (driver->parentStack, driver->currKey);
}

static void setCurrKey (Driver * driver, const Key * parent)
{
    assert (parent != NULL);
	if (driver->currKey != NULL)
	{
        keyDecRef (driver->currKey);
		keyDel (driver->currKey);
	}
	driver->currKey = keyNew (keyName (parent), KEY_END);
    keyIncRef(driver->currKey);
	// keyClear (driver->currKey);
}

static void resetCurrKey (Driver * driver)
{
	setCurrKey (driver, driver->parentStack->key);
}

static void extendCurrKey (Driver * driver, const char * name)
{
	assert (driver->currKey != NULL);
	keyAddBaseName (driver->currKey, name);
}

static char * getChildFraction (const Key * parent, const Key * child)
{
	printf ("Determining child fraction of:\n\t%s\n\t%s\n", keyName (parent), keyName (child));
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
		printf ("got fraction: %s\n", fraction);
		return fraction;
	}
}

static TableArray * pushTableArray (TableArray * top, Key * key)
{
	TableArray * ta = elektraCalloc (sizeof (TableArray));
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

static TableArray * popTableArray (TableArray * top)
{
	TableArray * newTop = top->next;
	keyDecRef (top->key);
	keyDel (top->key);
	elektraFree (top->keyStr);
	elektraFree (top);
	return newTop;
}

static Key * buildTableArrayKeyName (const TableArray * ta)
{
	if (ta->next == NULL || !keyIsBelow (ta->next->key, ta->key))
	{
		return indexToKey (ta->currIndex, ta->key);
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

static ParentList * pushParent (ParentList * top, Key * key)
{
	printf ("PUSH %s\n", keyName (key));
	ParentList * parent = elektraCalloc (sizeof (ParentList));
	parent->key = key;
	keyIncRef (key);
	parent->next = top;
	return parent;
}

static ParentList * popParent (ParentList * top)
{
	printf ("POP %s\n", keyName (top->key));
	ParentList * newTop = top->next;
	keyDecRef (top->key);
	keyDel (top->key);
	elektraFree (top);
	return newTop;
}

static IndexList * pushIndex (IndexList * top, int value)
{
	IndexList * newIndex = elektraCalloc (sizeof (IndexList));
	newIndex->value = value;
	newIndex->next = top;
	return newIndex;
}

static IndexList * popIndex (IndexList * top)
{
	IndexList * newTop = top->next;
	elektraFree (top);
	return newTop;
}

static Key * indexToKey (size_t index, const Key * parent)
{
	Key * indexKey = keyDup (parent);
	// Key * indexKey = keyNew (keyName (parent), KEY_END);

	char * indexStr = indexToArrayString (index);
	keyAddBaseName (indexKey, indexStr);
	elektraFree (indexStr);
	return indexKey;
}

static char * indexToArrayString (size_t index)
{
	size_t digits = 1;
	for (size_t value = index; value > 9; digits++)
	{
		value /= 10;
	}
	int strLen = 1 +	    //  '#'
		     (digits - 1) + // underscores
		     digits +       // actual digits
		     1;		    // '\0'
	char * str = elektraCalloc (sizeof (char) * strLen);
	memset (str, '_', sizeof (char) * strLen);
	str[0] = '#';
	str[strLen - 1] = 0;
	snprintf (str + 1 + (digits - 1), strLen, "%lu", index);
	return str;
}
