#include <stdint.h>
#include <stdlib.h>

#include <kdb.h>
#include <kdbhelper.h>

#include "driver.h"
#include "parser.h"

extern int yyparse (Driver * driver);
extern FILE * yyin;

static void popParentStack (Driver * driver);
static void topParentToKeySet (Driver * driver);
static void newTableArray (Driver * driver);
static void nextTableArrayElement (Driver * driver);
static ParentList * pushParent (ParentList * top, Key * key);
static ParentList * popParent (ParentList * top);
static IndexList * pushIndex (IndexList * top, int value);
static IndexList * popIndex (IndexList * top);
static TableArrayList * pushTableArray (TableArrayList * top, Key * key);
static TableArrayList * popTableArray (TableArrayList * top);
static char * indexToArrayString (size_t index);

Driver * createDriver (const Key * parent)
{
	Driver * driver = elektraCalloc (sizeof (Driver));
    driver->keys = ksNew(0, KS_END);
	driver->parentStack = pushParent (NULL, keyDup (parent));
    driver->tableArrayStack = NULL;
	driver->filename = keyString (parent);
    driver->file = NULL;
	driver->tableActive = 0;
	return driver;
}

int driverParse (Driver * driver)
{
	driver->file = fopen (driver->filename, "rb");
	if (driver->file == NULL)
	{
		driverError (driver, 0, "Could not open file");
		return 1;
	}
	yyin = driver->file;
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

void driverEnterKeyValue (Driver * driver)
{
	driver->keyDepthStack = pushIndex (driver->keyDepthStack, 0);
}

void driverExitKeyValue (Driver * driver)
{
	topParentToKeySet (driver);
	popParentStack (driver);
}

void driverExitSimpleKey (Driver * driver, const Scalar * name)
{
	Key * child = keyNew (keyName (driver->parentStack->key), KEY_END);
	keyAddBaseName (child, name->str);
	driver->parentStack = pushParent (driver->parentStack, child);
	driver->keyDepthStack->value++;
	printf ("pushed to '%s', key depth = %lu\n", keyName (driver->parentStack->key), driver->keyDepthStack->value);
}

void driverExitScalar (Driver * driver, Scalar * scalar)
{
	keySetString (driver->parentStack->key, scalar->str);
	printf ("ExitScalar: %s -> %s\n", keyName (driver->parentStack->key), keyString (driver->parentStack->key));
}

void driverEnterSimpleTable (Driver * driver)
{
	if (driver->tableActive == 0)
	{
		driver->tableActive = 1;
		driver->keyDepthStack = pushIndex (driver->keyDepthStack, 0);
		printf ("first time entering simple table!\n");
	}
	else
	{
		printf ("entering new simple table, clearing old one...\n");
		popParentStack (driver);
		driver->keyDepthStack = pushIndex (driver->keyDepthStack, 0);
	}
}

void driverExitSimpleTable (Driver * driver)
{
	keySetMeta (driver->parentStack->key, "simpletable", "");
	printf ("Setting metakey for %s: 'simpletable'\n", keyName (driver->parentStack->key));
}

void driverEnterTableArray (Driver * driver)
{
	if (driver->tableActive != 0)
	{
		popParentStack (driver);
		driver->tableActive = 0;
		printf ("Clearing simple table state before entering table array element\n");
	}
    if (driver->tableArrayStack != NULL) {
        popParentStack(driver);
    }
	driver->keyDepthStack = pushIndex (driver->keyDepthStack, 0);
}

void driverExitTableArray (Driver * driver)
{
	if (driver->tableArrayStack == NULL)
	{
        printf("table array stack NULL, make new table\n");
		newTableArray (driver);
	}
	else
	{
        printf("table array stack not null, decide\n");
		int rel = keyRel (driver->tableArrayStack->key, driver->parentStack->key);
		if (rel == 0) // same table array name -> next element
		{
			nextTableArrayElement (driver);
		}
		else if (rel > 0) // below top name -> push new sub table array
		{
			newTableArray (driver);
		}
		else if (rel < -1) // not below top name -> pop old table array, push new table array
		{
            driver->tableArrayStack = popTableArray (driver->tableArrayStack);
			newTableArray (driver);
		}
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
	topParentToKeySet (driver);
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

static void newTableArray (Driver * driver)
{
	driver->tableArrayStack = pushTableArray (driver->tableArrayStack, driver->parentStack->key);
	popParent (driver);
	nextTableArrayElement (driver);
}

static void nextTableArrayElement (Driver * driver)
{
	Key * key = keyNew (keyName (driver->tableArrayStack->key), KEY_END);
	char * indexStr = indexToArrayString (driver->tableArrayStack->index);
	keyAddBaseName (key, indexStr);
	elektraFree (indexStr);
	keySetMeta (driver->tableArrayStack->key, "array", keyBaseName (key));
	driver->parentStack = pushParent (driver->parentStack, key);

	driver->tableArrayStack->index++;
}

static void topParentToKeySet (Driver * driver)
{
	ksAppendKey (driver->keys, driver->parentStack->key);
}
static void popParentStack (Driver * driver)
{
	int keyDepth = driver->keyDepthStack->value;
	while (keyDepth-- > 0)
	{
		driver->parentStack = popParent (driver->parentStack);
	}
}

static ParentList * pushParent (ParentList * top, Key * key)
{
	ParentList * parent = elektraCalloc (sizeof (ParentList));
	parent->key = key;
	keyIncRef (key);
	parent->next = top;
	return parent;
}

static ParentList * popParent (ParentList * top)
{
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

static TableArrayList * pushTableArray (TableArrayList * top, Key * key)
{
	TableArrayList * newTop = elektraCalloc (sizeof (TableArrayList));
	newTop->key = key;
	keyIncRef (key);
	newTop->index = 0;
	newTop->next = top;
	return newTop;
}
static TableArrayList * popTableArray (TableArrayList * top)
{
	TableArrayList * newTop = top->next;
	keyDecRef (top->key);
	keyDel (top->key);
	elektraFree (top);
	return newTop;
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
