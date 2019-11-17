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
#include "kdbprivate.h"

#ifdef __cplusplus
extern "C" {
using Key = ckdb::Key;
using KeySet = ckdb::KeySet;
#endif

enum
{
	MERGE_STRATEGY_ABORT = 1,
	MERGE_STRATEGY_INTERACTIVE = 2,
	MERGE_STRATEGY_OUR = 3,
	MERGE_STRATEGY_THEIR = 4,
};

KeySet * elektraMerge (KeySet * our, Key * ourRoot, KeySet * their, Key * theirRoot, KeySet * base, Key * baseRoot, Key * resultKey,
		       int strategy, Key * informationKey);
int getConflicts (Key * informationKey);

#ifdef __cplusplus
}
#endif
#endif
