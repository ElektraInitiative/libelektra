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
#include "kdbtypes.h"

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

/**
 * The strategy to use when a merge conflict occurs
 */
enum MergeStrategy
{
	MERGE_STRATEGY_ABORT = 1, /*!< Abort merging if a conflict occurs */
	// MERGE_STRATEGY_INTERACTIVE = 2, /*!< Perform an interactive conflict resolution. Currently not implemented */
	MERGE_STRATEGY_OUR = 3,	  /*!< Prefer our keys in case of conflict */
	MERGE_STRATEGY_THEIR = 4, /*!< Prefer their keys in case of conflict */
};

KeySet * elektraMerge (KeySet * our, Key * ourRoot, KeySet * their, Key * theirRoot, KeySet * base, Key * baseRoot, Key * resultKey,
		       enum MergeStrategy strategy, Key * informationKey);
int elektraMergeGetConflicts (Key * informationKey);

KeySet * elektraMergeGetConflictingKeys (Key * informationKey, Key * root);
bool elektraMergeIsKeyConflicting (Key * informationKey, Key * root, Key * key);

#ifdef __cplusplus
}
}
#endif
#endif
