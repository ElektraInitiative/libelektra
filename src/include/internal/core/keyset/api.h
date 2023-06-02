/**
 * @file
 *
 * @brief Internal API for KeySet
 *
 * @copyright BSD License (see doc/COPYING or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_CORE_KEYSET_API_INTERNAL_H
#define ELEKTRA_CORE_KEYSET_API_INTERNAL_H

#include <elektra/core/keyset.h>
#include <elektra/core/types.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

int ksInit (KeySet * ks);
int ksClose (KeySet * ks);

void keySetDetachData (KeySet * keyset);

int ksResize (KeySet * ks, size_t size);
size_t ksGetAlloc (const KeySet * ks);
KeySet * ksDeepDup (const KeySet * source);

Key * elektraKsPopAtCursor (KeySet * ks, elektraCursor pos);

KeySet * ksRenameKeys (KeySet * config, const char * name);

ssize_t ksRename (KeySet * ks, const Key * root, const Key * newRoot);

elektraCursor ksFindHierarchy (const KeySet * ks, const Key * root, elektraCursor * end);
KeySet * ksBelow (const KeySet * ks, const Key * root);

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_CORE_KEYSET_API_INTERNAL_H
