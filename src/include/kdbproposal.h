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

enum elektraLookupOptions
{
	KDB_O_SPEC=1<<15,
	KDB_O_CREATE=1<<16
};

/**
 * Elektra currently supported Key namespaces.
 *
 * @ingroup keyname
 * @see kdbGet(), keyGetNamespace()
 */
typedef enum
{
	KEY_NS_NONE=0,          ///< no key given as parameter to keyGetNamespace()
	KEY_NS_EMPTY=1,         ///< key name was empty, e.g. invalid key name
	KEY_NS_META=2,          ///< meta key, i.e. any key name not under other categories
	KEY_NS_CASCADING=3,     ///< cascading key, starts with /, abstract name for any of the namespaces below
	KEY_NS_FIRST=4,         ///< For iteration over namespaces
	KEY_NS_USER=4,          ///< user key in the home directory of the current user
	KEY_NS_SYSTEM=5,        ///< system key is shared for a computer system
	KEY_NS_SPEC=6,          ///< spec contains the specification of the other namespaces
	KEY_NS_LAST=6           ///< For iteration over namespaces
} elektraNamespace;

elektraNamespace keyGetNamespace(Key const* key);


// alternative to keyAddBaseName (adds full name)
ssize_t keyAddName(Key *key, const char *addName);

// locks a key, is this needed externally?
int keyLock(Key *key,
	/*option_t*/ enum elektraLockOptions lock);

// this might become the new keySetName
ssize_t elektraKeySetName(Key *key, const char *newName,
	option_t options);

Key *elektraArrayGetNextKey(KeySet *arrayKeys);
KeySet *elektraArrayGet(const Key *arrayParent, KeySet *keys);

KeySet *elektraKeyGetMetaKeySet(const Key *key);

int elektraKsFilter (KeySet *result, KeySet *input, int (*filter) (const Key *k, void *argument), void *argument);

KeySet* ksDeepDup(const KeySet *source);

// is this needed? -> rather not
Key *ksPrev(KeySet *ks);
Key *ksPopAtCursor(KeySet *ks, cursor_t c);

#ifdef __cplusplus
}
}
#endif


#endif
