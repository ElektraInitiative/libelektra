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

// this might become the new keySetName
enum elektra_name_options
{
	KDB_O_CASCADING_NAME=1,
	KDB_O_META_NAME=2,
	KDB_O_EMPTY_NAME=4
};

ssize_t elektraKeySetName(Key *key, const char *newName,
		enum elektra_name_options options);


#ifdef __cplusplus
}
}
#endif


#endif
