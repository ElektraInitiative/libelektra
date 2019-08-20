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

#define MERGE_STRATEGY_ABORT 1
#define MERGE_STRATEGY_INTERACTIVE 2
#define MERGE_STRATEGY_OUR 3
#define MERGE_STRATEGY_THEIR 4
#define MERGE_STRATEGY_BASE 5

KeySet * elektraMerge (KeySet * our, Key * ourRoot, KeySet * their, Key * theirRoot, KeySet * base, Key * baseRoot, Key * resultKey,
		   int strategy);

#ifdef __cplusplus
}
#endif
#endif
