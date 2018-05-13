/**
 * @file
 *
 * @brief Implementation of OneSideStrategy
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <helper/keyhelper.hpp>
#include <merging/onesidevaluestrategy.hpp>
#include <string>

using namespace std;
using namespace kdb::tools::helper;

namespace kdb
{

namespace tools
{

namespace merging
{

void OneSideValueStrategy::resolveConflict (const MergeTask & task, Key & conflictKey, MergeResult & result)
{
	ConflictOperation ourOperation = getOurConflictOperation (conflictKey);
	ConflictOperation theirOperation = getTheirConflictOperation (conflictKey);

	string ourLookup = rebasePath (conflictKey, task.mergeRoot, task.ourParent);
	string theirLookup = rebasePath (conflictKey, task.mergeRoot, task.theirParent);

	// TODO: this is a subset of the onesidestrategy
	// the onesidestrategy could be split up into several smaller strategies
	if ((ourOperation == CONFLICT_SAME && theirOperation == CONFLICT_MODIFY) ||
	    (ourOperation == CONFLICT_MODIFY && theirOperation == CONFLICT_SAME))
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
	}
}
} // namespace merging
} // namespace tools
} // namespace kdb
