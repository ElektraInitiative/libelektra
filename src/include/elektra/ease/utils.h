#ifndef ELEKTRA_EASE_UTILS_H
#define ELEKTRA_EASE_UTILS_H

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>

int elektraKsFilter (KeySet * result, KeySet * input, int (*filter) (const Key * k, void * argument), void * argument);
int elektraKsToMemArray (KeySet * ks, Key ** buffer);

#endif // ELEKTRA_EASE_UTILS_H
