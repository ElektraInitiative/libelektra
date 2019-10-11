#ifndef ELEKTRA_PLUGIN_TOML_DRIVER_H
#define ELEKTRA_PLUGIN_TOML_DRIVER_H

#include <kdb.h>
#include <stdio.h>

#include "scalar.h"

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
	IndexList * keyDepthStack;
	IndexList * indexStack;
	const char * filename;
	FILE * file;
    int tableActive;
} Driver;


Driver * createDriver (const Key * parent);
int driverParse (Driver * driver);
void driverError (Driver * driver, int lineno, const char * msg);

void driverEnterKeyValue (Driver * driver);
void driverExitKeyValue (Driver * driver);
void driverExitSimpleKey (Driver * driver, const Scalar * name);
void driverExitScalar (Driver * driver, Scalar * scalar);
void driverEnterSimpleTable (Driver * driver);
void driverEnterArray (Driver * driver);
void driverExitArray (Driver * driver);

#endif // ELEKTRA_PLUGIN_TOML_DRIVER_H
