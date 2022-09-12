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

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

enum
{
	MERGE_STRATEGY_ABORT = 1,
	MERGE_STRATEGY_INTERACTIVE = 2,
	MERGE_STRATEGY_OUR = 3,
	MERGE_STRATEGY_THEIR = 4,
};

ElektraKeyset * elektraMerge (ElektraKeyset * our, ElektraKey * ourRoot, ElektraKeyset * their, ElektraKey * theirRoot, ElektraKeyset * base, ElektraKey * baseRoot, ElektraKey * resultKey,
		       int strategy, ElektraKey * informationKey);
int getConflicts (ElektraKey * informationKey);

#ifdef __cplusplus
}
}
#endif
#endif
