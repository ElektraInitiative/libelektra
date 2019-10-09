#ifndef ELEKTRA_PLUGIN_TOML_DRIVER_H
#define ELEKTRA_PLUGIN_TOML_DRIVER_H

#include <kdb.h>
#include <stdio.h>

typedef struct _ParentList
{
	Key * key;
	struct _ParentList * next;
} ParentList;

typedef struct _IndexList
{
	int value;
	struct _IndexList * next;
} IndexList;

typedef struct
{
	KeySet * keys;
	ParentList * parentStack;
	IndexList * indexStack;
	const char * filename;
	FILE * file;
} Driver;


Driver * createDriver (const Key * parent);
int driverParse (Driver * driver);
void driverError (Driver * driver, int lineno, const char * msg);

void driverExitKey (Driver * driver, const char * text);

#endif // ELEKTRA_PLUGIN_TOML_DRIVER_H
