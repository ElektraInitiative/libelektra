/**
 * @file
 *
 * @brief Implementation of OneSideStrategy
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <helper/keyhelper.hpp>
#include <merging/onesidestrategy.hpp>
#include <string>

using namespace std;
using namespace kdb::tools::helper;

namespace kdb
{

namespace tools
{

namespace merging
{

void OneSideStrategy::resolveConflict (const MergeTask & task, Key & conflictKey, MergeResult & result)
{
	string lookupPath;
	Key winningKey;

	switch (winningSide)
	{
	case BASE:
		lookupPath = rebasePath (conflictKey, task.mergeRoot, task.baseParent);
		winningKey = task.base.lookup (lookupPath);
		break;
	case OURS:
		lookupPath = rebasePath (conflictKey, task.mergeRoot, task.ourParent);
		winningKey = task.ours.lookup (lookupPath);
		break;
	case THEIRS:
		lookupPath = rebasePath (conflictKey, task.mergeRoot, task.theirParent);
		winningKey = task.theirs.lookup (lookupPath);
		break;
	}

	if (winningKey)
	{
		copyKeyValue (winningKey, conflictKey);
		result.resolveConflict (conflictKey);
		result.addMergeKey (conflictKey);
	}
	else
	{
		result.resolveConflict (conflictKey);
		result.removeMergeKey (conflictKey);
	}
}
} // namespace merging
} // namespace tools
} // namespace kdb
