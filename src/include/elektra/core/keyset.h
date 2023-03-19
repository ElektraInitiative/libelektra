/**
 * @file
 *
 * @brief Elektra Core KeySet API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_CORE_KEYSET_H
#define ELEKTRA_CORE_KEYSET_H

#include <elektra/macros/attributes.h>

#include <stdlib.h>

enum
{
	KDB_O_NONE = 0,
	KDB_O_DEL = 1,
	KDB_O_POP = 1 << 1
};
typedef int elektraLookupFlags;

typedef ssize_t elektraCursor;

#ifdef __cplusplus
#define KS_END (static_cast<ckdb::Key *> (0))
#else
#define KS_END ((Key *) 0)
#endif


#ifdef __cplusplus
// TODO: get rid of this
namespace ckdb
{
extern "C" {
#endif

typedef struct _KeySet KeySet;

KeySet * ksNew (size_t alloc, ...) ELEKTRA_SENTINEL;
KeySet * ksVNew (size_t alloc, va_list ap);

KeySet * ksDup (const KeySet * source);
int ksCopy (KeySet * dest, const KeySet * source);

uint16_t ksIncRef (KeySet * ks);
uint16_t ksDecRef (KeySet * ks);
uint16_t ksGetRef (const KeySet * ks);

int ksClear (KeySet * ks);
int ksDel (KeySet * ks);

ssize_t ksGetSize (const KeySet * ks);

ssize_t ksAppendKey (KeySet * ks, Key * toAppend);

ssize_t ksAppend (KeySet * ks, const KeySet * toAppend);
KeySet * ksCut (KeySet * ks, const Key * cutpoint);

Key * ksPop (KeySet * ks);

int ksRewind (KeySet * ks);
Key * ksNext (KeySet * ks);
Key * ksCurrent (const KeySet * ks);

elektraCursor ksGetCursor (const KeySet * ks);
int ksSetCursor (KeySet * ks, elektraCursor cursor);
Key * ksAtCursor (const KeySet * ks, elektraCursor cursor);

Key * ksLookup (KeySet * ks, Key * k, elektraLookupFlags options);
Key * ksLookupByName (KeySet * ks, const char * name, elektraLookupFlags options);

ssize_t ksSearch (const KeySet * ks, const Key * toAppend);

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_CORE_KEYSET_H