#ifndef ELEKTRA_EASE_REFERENCE_H
#define ELEKTRA_EASE_REFERENCE_H

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
using Key = ckdb::Key;
using KeySet = ckdb::KeySet;
#endif

int elektraIsReferenceRedundant (const char * reference);
char * elektraResolveReference (const char * reference, const Key * baseKey, const Key * parentKey);

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_EASE_REFERENCE_H
