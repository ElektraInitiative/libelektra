#ifndef ELEKTRA_KDB_BACKENDS_H
#define ELEKTRA_KDB_BACKENDS_H

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>

#include <stdbool.h>

Key * backendsFindParent (KeySet * backends, const Key * key);
KeySet * backendsForParentKey (KeySet * backends, Key * parentKey);
bool backendsDivide (KeySet * backends, const KeySet * ks);
void backendsMerge (KeySet * backends, KeySet * ks);

#endif // ELEKTRA_KDB_BACKENDS_H
