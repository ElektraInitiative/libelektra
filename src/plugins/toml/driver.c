#include <stdlib.h>

#include <kdb.h>
#include <kdbhelper.h>

#include "driver.h"
#include "parser.h"

extern int yyparse (Driver * driver);
extern FILE * yyin;

static ParentList * pushParent (const ParentList * top, Key * key);
static ParentList * popParent (ParentList * top);

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

void driverExitKey (Driver * driver, const char * text)
{
	Key * child = keyNew (keyName (driver->parentStack->key), KEY_END);
	keyAddBaseName (child, text);
	driver->parentStack = pushParent (driver->parentStack, child);
	printf ("pushed %s\n", keyName (driver->parentStack->key));
}

static ParentList * pushParent (const ParentList * top, Key * key)
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
