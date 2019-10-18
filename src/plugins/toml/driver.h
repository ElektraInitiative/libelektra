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
	size_t value;
	struct _IndexList * next;
} IndexList;

typedef struct _TableArray {
    Key * key;
    char *keyStr;
    size_t currIndex;
    struct _TableArray * next;
} TableArray;

typedef struct
{
	KeySet * keys;
    Key * root;
	ParentList * parentStack;
    Key * currKey;
	IndexList * indexStack;
    TableArray * tableArrayStack;
	const char * filename;
	FILE * file;
	int tableActive;
} Driver;


Driver * createDriver (const Key * parent);
int driverParse (Driver * driver, KeySet * returned);
void driverError (Driver * driver, int lineno, const char * msg);

void driverEnterKey (Driver * driver);
void driverExitKey (Driver * driver);
void driverExitKeyValue (Driver * driver);

void driverExitSimpleKey (Driver * driver, const Scalar * name);

void driverExitScalar (Driver * driver, Scalar * scalar);

void driverEnterSimpleTable (Driver * driver);
void driverExitSimpleTable (Driver * driver);
void driverEnterTableArray (Driver * driver);
void driverExitTableArray (Driver * driver);

void driverEnterArray (Driver * driver);
void driverExitArray (Driver * driver);
void driverEnterArrayElement (Driver * driver);
void driverExitArrayElement (Driver * driver);

void driverEnterInlineTable (Driver * driver);

void driverExitComment (Driver * driver, const Scalar * comment);

#endif // ELEKTRA_PLUGIN_TOML_DRIVER_H
