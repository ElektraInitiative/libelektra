/**
 * @file
 *
 * @brief Internal API for Key
 *
 * @copyright BSD License (see doc/COPYING or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_CORE_KEY_API_INTERNAL_H
#define ELEKTRA_CORE_KEY_API_INTERNAL_H

#include <elektra/core/key.h>
#include <elektra/core/namespace.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

ssize_t keySetRaw (Key * key, const void * newBinary, size_t dataSize);
void keyInit (Key * key);
void keyClearSync (Key * key);
int keyReplacePrefix (Key * key, const Key * oldPrefix, const Key * newPrefix);
void keyDetachKeyName (Key * key);

static inline int keyIsSpec (const Key * key)
{
	return keyGetNamespace (key) == KEY_NS_SPEC;
}

static inline int keyIsProc (const Key * key)
{
	return keyGetNamespace (key) == KEY_NS_PROC;
}


static inline int keyIsDir (const Key * key)
{
	return keyGetNamespace (key) == KEY_NS_DIR;
}

static inline int keyIsSystem (const Key * key)
{
	return keyGetNamespace (key) == KEY_NS_SYSTEM;
}

static inline int keyIsUser (const Key * key)
{
	return keyGetNamespace (key) == KEY_NS_USER;
}

// TODO (kodebaach) [Q]: make public? move to ease/extra?
int elektraIsArrayPart (const char * namePart);

#include <internal/core/key/struct.h>

static inline KeySet * keyMetaNoAlloc (const Key * key)
{
	if (!key) return NULL;
	return key->meta;
}

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_CORE_KEY_API_INTERNAL_H
