#ifndef ELEKTRA_KDB_HOOKS_H
#define ELEKTRA_KDB_HOOKS_H

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/kdb/kdb.h>

int initHooks (KDB * kdb, const KeySet * config, KeySet * modules, const KeySet * contract, Key * errorKey);
void freeHooks (KDB * kdb, Key * errorKey);

#endif // ELEKTRA_KDB_HOOKS_H
