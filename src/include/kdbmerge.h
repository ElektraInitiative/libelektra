/**
 * @file
 *
 * @brief Kdb merge tool
 *
 * @copyright BSD License (see LICENSE.md or https://ww.libelektra.org)
 */

#ifndef KDBMERGE_H_
#define KDBMERGE_H_

#include "elektra/kdb.h"
#include "kdberrors.h"
#include "kdbprivate.h"

#ifdef __cplusplus
extern "C" {
#endif

enum
{
	MERGE_STRATEGY_ABORT = 1,
	MERGE_STRATEGY_INTERACTIVE = 2,
	MERGE_STRATEGY_OUR = 3,
	MERGE_STRATEGY_THEIR = 4,
} strategies;


KeySet * elektraMerge (KeySet * our, Key * ourRoot, KeySet * their, Key * theirRoot, KeySet * base, Key * baseRoot, Key * resultKey,
		       int strategy, Key * informationKey);

#ifdef __cplusplus
}
#endif
#endif
