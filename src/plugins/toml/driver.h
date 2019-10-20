#ifndef ELEKTRA_PLUGIN_TOML_DRIVER_H
#define ELEKTRA_PLUGIN_TOML_DRIVER_H

#include <kdb.h>
#include <stdarg.h>
#include <stdbool.h>
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

typedef struct _TableArray
{
	Key * key;
	char * keyStr;
	size_t currIndex;
	struct _TableArray * next;
} TableArray;

typedef struct _CommentList
{
	char * comment;
	size_t spaces;
	struct _CommentList * next;
} CommentList;

typedef struct
{
	KeySet * keys;
	Key * root;
	ParentList * parentStack;
	Key * currKey;
	Key * prevKey;
	IndexList * indexStack;
	TableArray * tableArrayStack;
	CommentList * commentRoot;
	CommentList * commentBack;
	size_t spaceCount;
	size_t newlineCount;
	const char * filename;
	FILE * file;
	bool tableActive;
	bool drainCommentsOnKeyExit;
	int currLine;
} Driver;


Driver * createDriver (const Key * parent);
int driverParse (Driver * driver, KeySet * returned);
void driverError (Driver * driver, int lineno, const char * format, ...);

void driverExitToml (Driver * driver);
void driverEnterKey (Driver * driver);
void driverExitKey (Driver * driver);
void driverExitKeyValue (Driver * driver);

void driverExitOptCommentKeyPair (Driver * driver);
void driverExitOptCommentTable (Driver * driver);

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
void driverExitSpace (Driver * driver);
void driverExitNewline (Driver * driver);

#endif // ELEKTRA_PLUGIN_TOML_DRIVER_H
