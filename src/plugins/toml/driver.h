#ifndef ELEKTRA_PLUGIN_TOML_DRIVER_H
#define ELEKTRA_PLUGIN_TOML_DRIVER_H

#include <kdb.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

#include "comment_list.h"
#include "scalar.h"
#include "table_array.h"

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

typedef struct
{
	KeySet * keys;
	Key * root;
	ParentList * parentStack;
	Key * currKey;
	Key * prevKey;
	IndexList * indexStack;
	TableArrayList * tableArrayStack;
	CommentList * commentRoot;
	CommentList * commentBack;
	Scalar * lastScalar;
	char * filename;
	size_t order;
	size_t newlineCount;
	size_t currLine;
	bool simpleTableActive;
	bool drainCommentsOnKeyExit;
	bool errorSet;
} Driver;

int tomlRead (KeySet * keys, Key * parent);

void driverError (Driver * driver, int err, int lineno, const char * format, ...);
void driverErrorGeneric (Driver * driver, int err, const char * caller, const char * callee);

void driverExitToml (Driver * driver);
void driverEnterKey (Driver * driver);
void driverExitKey (Driver * driver);
void driverExitKeyValue (Driver * driver);

void driverExitOptCommentKeyPair (Driver * driver);
void driverExitOptCommentTable (Driver * driver);

void driverExitSimpleKey (Driver * driver, Scalar * name);

void driverExitValue (Driver * driver, Scalar * scalar);

void driverEnterSimpleTable (Driver * driver);
void driverExitSimpleTable (Driver * driver);
void driverEnterTableArray (Driver * driver);
void driverExitTableArray (Driver * driver);

void driverEnterArray (Driver * driver);
void driverExitArray (Driver * driver);
void driverEmptyArray (Driver * driver);
void driverEnterArrayElement (Driver * driver);
void driverExitArrayElement (Driver * driver);

void driverEnterInlineTable (Driver * driver);
void driverExitInlineTable (Driver * driver);
void driverEmptyInlineTable (Driver * driver);

void driverExitComment (Driver * driver, Scalar * comment);
void driverExitNewline (Driver * driver);

#endif // ELEKTRA_PLUGIN_TOML_DRIVER_H
