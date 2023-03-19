/**
 * @file
 *
 * @brief Elektra Core Key API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_CORE_KEY_H
#define ELEKTRA_CORE_KEY_H

#include <elektra/macros/attributes.h>

#include <stdlib.h>

enum
{
	KEY_VALUE = 1 << 1,
	KEY_FLAGS = 3,
	KEY_BINARY = 1 << 4,
	KEY_SIZE = 1 << 11,
	KEY_FUNC = 1 << 12,
	KEY_META = 1 << 15,
	KEY_NULL = 1 << 16,
	KEY_LOCK_NAME = 1 << 17,
	KEY_LOCK_VALUE = 1 << 18,
	KEY_LOCK_META = 1 << 19,
};

typedef int elektraLockFlags;
typedef int elektraKeyFlags;

enum
{
	KEY_CP_NAME = 1 << 0,
	KEY_CP_STRING = 1 << 1,
	KEY_CP_VALUE = 1 << 2,
	KEY_CP_META = 1 << 3,
	KEY_CP_ALL = KEY_CP_NAME | KEY_CP_VALUE | KEY_CP_META,
};
typedef unsigned int elektraCopyFlags;


#ifdef __cplusplus
#define KEY_END (static_cast<void *> (0))
#else
#define KEY_END ((void *) 0)
#endif

#ifdef __cplusplus
// TODO: get rid of this
namespace ckdb
{
extern "C" {
#endif

typedef struct _Key Key;

/* Basic Methods */
Key * keyNew (const char * keyname, ...) ELEKTRA_SENTINEL;
Key * keyVNew (const char * keyname, va_list ap);

Key * keyCopy (Key * dest, const Key * source, elektraCopyFlags flags);

int keyClear (Key * key);
int keyDel (Key * key);

uint16_t keyIncRef (Key * key);
uint16_t keyDecRef (Key * key);
uint16_t keyGetRef (const Key * key);

/* Meta Info */
const Key * keyNextMeta (Key * key); /* TODO: remove, currently only needed for directoryvalue-plugin */
int keyCopyMeta (Key * dest, const Key * source, const char * metaName);
int keyCopyAllMeta (Key * dest, const Key * source);

const Key * keyGetMeta (const Key * key, const char * metaName);
ssize_t keySetMeta (Key * key, const char * metaName, const char * newMetaString);
KeySet * keyMeta (Key * key);

/* Methods for Making Tests */
int keyCmp (const Key * k1, const Key * k2);

int keyNeedSync (const Key * key);

int keyIsBelow (const Key * key, const Key * check);
int keyIsBelowOrSame (const Key * key, const Key * check);
int keyIsDirectlyBelow (const Key * key, const Key * check);

int keyIsBinary (const Key * key);
int keyIsString (const Key * key);

/* Name Manipulation Methods */
const char * keyName (const Key * key);
ssize_t keyGetNameSize (const Key * key);
ssize_t keyGetName (const Key * key, char * returnedName, size_t maxSize);

ssize_t keySetName (Key * key, const char * newname);
ssize_t keyAddName (Key * key, const char * addName);

const void * keyUnescapedName (const Key * key);
ssize_t keyGetUnescapedNameSize (const Key * key);

const char * keyBaseName (const Key * key);
ssize_t keyGetBaseNameSize (const Key * key);
ssize_t keyGetBaseName (const Key * key, char * returned, size_t maxSize);

ssize_t keySetBaseName (Key * key, const char * baseName);
ssize_t keyAddBaseName (Key * key, const char * baseName);

elektraNamespace keyGetNamespace (Key const * key);
ssize_t keySetNamespace (Key * key, elektraNamespace ns);

/* Value Manipulation Methods */
const void * keyValue (const Key * key);
ssize_t keyGetValueSize (const Key * key);

const char * keyString (const Key * key);
ssize_t keyGetString (const Key * key, char * returnedString, size_t maxSize);
ssize_t keySetString (Key * key, const char * newString);

ssize_t keyGetBinary (const Key * key, void * returnedBinary, size_t maxSize);
ssize_t keySetBinary (Key * key, const void * newBinary, size_t dataSize);


/* Lock Methods */
int keyLock (Key * key, elektraLockFlags what);
int keyIsLocked (const Key * key, elektraLockFlags what);

/* Aliases */
static inline Key * keyDup (const Key * source, elektraCopyFlags flags)
{
	return keyCopy (keyNew ("/", KEY_END), source, flags);
}

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_CORE_KEY_H