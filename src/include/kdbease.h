#ifndef KDBEASE_H
#define KDBEASE_H

#include <kdb.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

int elektraKsFilter (KeySet * result, KeySet * input, int (*filter) (const Key * k, void * argument), void * argument);

int elektraArrayIncName (Key * key);
int elektraArrayDecName (Key * key);

int elektraArrayValidateName (const Key * key);

const char * elektraKeyGetRelativeName (Key const * cur, Key const * parentKey);

KeySet * elektraArrayGet (const Key * arrayParent, KeySet * keys);
Key * elektraArrayGetNextKey (KeySet * arrayKeys);

keyswitch_t keyCompare (const Key * key1, const Key * key2);
keyswitch_t keyCompareMeta (const Key * key1, const Key * key2);

#ifdef __cplusplus
}
}
#endif


#endif
