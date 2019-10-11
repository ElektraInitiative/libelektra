/**
 * @file
 *
 * @brief Proposed declarations.
 *
 * These functions are likely not API/ABI stable.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KDBPROPOSAL_H
#define KDBPROPOSAL_H

#include <kdb.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

KeySet * ksRenameKeys (KeySet * config, const Key * name);


// this might become the new keySetName
ssize_t elektraKeySetName (Key * key, const char * newName, option_t options);

KeySet * elektraKeyGetMetaKeySet (const Key * key);

Key * ksPopAtCursor (KeySet * ks, cursor_t c);

#ifdef __cplusplus
}
}
#endif


#endif
