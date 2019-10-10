#include <stdlib.h>

#include <kdb.h>
#include <kdbhelper.h>

#include "driver.h"
#include "parser.h"

extern int yyparse (Driver * driver);
extern FILE * yyin;

static ParentList * pushParent (ParentList * top, Key * key);
static ParentList * popParent (ParentList * top);
static IndexList * pushIndex (IndexList * top, int value);
static IndexList * popIndex (IndexList * top);

Driver * createDriver (const Key * parent)
{
	Driver * driver = elektraCalloc (sizeof (Driver));
	driver->parentStack = pushParent (NULL, keyDup (parent));
	driver->filename = keyString (parent);
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
	printf ("K/V: %s -> %s\n", keyName (driver->parentStack->key), keyString (driver->parentStack->key));
	int keyDepth = driver->keyDepthStack->value;
	while (keyDepth-- > 0)
	{
		driver->parentStack = popParent (driver->parentStack);
	}
	driver->keyDepthStack = popIndex (driver->keyDepthStack);
}

void driverExitSimpleKey (Driver * driver, const char * text)
{
	Key * child = keyNew (keyName (driver->parentStack->key), KEY_END);
	keyAddBaseName (child, text);
	driver->parentStack = pushParent (driver->parentStack, child);
	driver->keyDepthStack->value++;
	printf ("pushed to '%s', key depth = %d\n", keyName (driver->parentStack->key), driver->keyDepthStack->value);
}

void driverExitScalar (Driver * driver, Scalar * scalar)
{
	keySetString (driver->parentStack->key, scalar->str);
}

void driverEnterArray(Driver * driver) {
    driver->indexStack = pushIndex (driver->indexStack, 1);
}

void driverExitArray(Driver * driver) {
    driver->indexStack = popIndex(driver->indexStack);
}

static ParentList * pushParent (ParentList * top, Key * key)
{
	ParentList * parent = elektraCalloc (sizeof (ParentList));
	parent->key = key;
	parent->next = top;
	return parent;
}

static ParentList * popParent (ParentList * top)
{
	ParentList * newTop = top->next;
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
