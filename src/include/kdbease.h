#ifndef KDBEASE_H
#define KDBEASE_H

#include <kdb.h>

#ifdef __cplusplus
namespace ckdb {
extern "C" {
#endif

int elektraArrayIncName(Key *key);
int elektraKsFilter (KeySet *result, KeySet *input, int (*filter) (const Key *k, void *argument), void *argument);

KeySet *elektraArrayGet(const Key *arrayParent, KeySet *keys);
Key *elektraArrayGetNextKey(KeySet *arrayKeys);

#ifdef __cplusplus
}
}
#endif


#endif
