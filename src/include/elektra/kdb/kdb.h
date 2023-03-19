/**
 * @file
 *
 * @brief Elektra KDB main API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_KDB_H
#define ELEKTRA_KDB_KDB_H

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>

#ifdef __cplusplus
// TODO: get rid of this
namespace ckdb
{
extern "C" {
#endif

typedef struct _KDB KDB;

KDB * kdbOpen (const KeySet * contract, Key * parentKey);

int kdbClose (KDB * handle, Key * errorKey);

int kdbGet (KDB * handle, KeySet * returned, Key * parentKey);
int kdbSet (KDB * handle, KeySet * returned, Key * parentKey);


#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_KDB_KDB_H