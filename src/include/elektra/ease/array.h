#ifndef ELEKTRA_EASE_ARRAY_H
#define ELEKTRA_EASE_ARRAY_H

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/type/types.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
using Key = ckdb::Key;
using KeySet = ckdb::KeySet;
#endif

int elektraArrayIncName (Key * key);
int elektraArrayDecName (Key * key);

int elektraArrayValidateName (const Key * key);
int elektraArrayValidateBaseNameString (const char * baseName);

KeySet * elektraArrayGet (const Key * arrayParent, KeySet * keys);
Key * elektraArrayGetNextKey (KeySet * arrayKeys);

int elektraReadArrayNumber (const char * baseName, kdb_long_long_t * oldIndex);

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_EASE_ARRAY_H