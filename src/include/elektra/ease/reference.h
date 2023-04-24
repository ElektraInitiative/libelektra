#ifndef ELEKTRA_EASE_NAME_H
#define ELEKTRA_EASE_NAME_H

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>

int elektraIsReferenceRedundant (const char * reference);
char * elektraResolveReference (const char * reference, const Key * baseKey, const Key * parentKey);

#endif // ELEKTRA_EASE_NAME_H
