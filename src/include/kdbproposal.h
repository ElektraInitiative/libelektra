/***************************************************************************
 *    kdbproposal.h  -  Proposed declarations
 *
 * Do not use them if avoidable, they are likely not API/ABI stable.
 *
 *                         -------------------
 *  begin                : Sun 08 Dec, 2013
 *  copyright            : (C) 2013 by Markus Raab
 *  email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#ifndef KDBPROPOSAL_H
#define KDBPROPOSAL_H

#include <kdb.h>

#ifdef __cplusplus
namespace ckdb {
extern "C" {
#endif


// is this needed?
Key *ksPrev(KeySet *ks);
Key *ksPopAtCursor(KeySet *ks, cursor_t c);
Key *ksLookupBySpec(KeySet *ks, Key *specKey);

// is the unescaped name useful for applications?
const void *keyUnescapedName(const Key *key);
ssize_t keyGetUnescapedNameSize(const Key *key);

// can be made simply without elektra's internals, so better keep it as
// extension.
ssize_t keySetStringF(Key *key, const char *format, ...);

// could also be in an extension library.
int elektraArrayIncName(Key *key);
int elektraKsToMemArray(KeySet *ks, Key **buffer);
KeySet* elektraRenameKeys(KeySet *config, const char* name);

enum elektraLockOptions
{
	KEY_LOCK_NAME=1<<17,
	KEY_LOCK_VALUE=1<<18,
	KEY_LOCK_META=1<<19
};

enum elektraNameOptions
{
	KDB_O_CASCADING_NAME=1<<20,
	KDB_O_META_NAME=1<<21,
	KDB_O_EMPTY_NAME=1<<22

};

// alternative to keyAddBaseName (adds full name)
ssize_t keyAddName(Key *key, const char *addName);

// locks a key, is this needed externally?
int keyLock(Key *key,
	/*option_t*/ enum elektraLockOptions lock);

// this might become the new keySetName
ssize_t elektraKeySetName(Key *key, const char *newName,
	/*option_t*/ enum elektraNameOptions options);

Key *elektraArrayGetNextKey(KeySet *arrayKeys);
KeySet *elektraArrayGet(const Key *arrayParent, KeySet *keys);

KeySet *elektraKeyGetMetaKeySet(const Key *key);
int elektraKsFilter (KeySet *result, KeySet *input, int (*filter) (const Key *k));
int elektraKsFilterArgument (KeySet *result, KeySet *input, int (*filter) (const Key *k, void *argument), void *argument);

#ifdef __cplusplus
}
}
#endif


#endif
