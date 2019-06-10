/**
 * @file
 *
 * @brief Kdb merge tool
 *
 * @copyright BSD License (see LICENSE.md or https://ww.libelektra.org)
 */

#ifndef KDBMERGE_H_
#define KDBMERGE_H_

#include "kdb.h"
#include "kdberrors.h"
#include "kdbprivate.h"

#ifdef __cplusplus
extern "C" {
#endif

KeySet * kdbMerge (KeySet * our, Key * ourRoot, KeySet * their, Key * theirRoot, KeySet * base, Key * baseRoot, Key * resultKey);

#ifdef __cplusplus
}
#endif
#endif
