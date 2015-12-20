/**
 * @file
 *
 * @brief Implementation of AutoMergeStrategy
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <string>
#include <helper/keyhelper.hpp>
#include <merging/automergestrategy.hpp>

using namespace std;
using namespace kdb::tools::helper;

namespace kdb
{

namespace tools
{

namespace merging
{

void AutoMergeStrategy::resolveConflict(const MergeTask& task, Key& conflictKey, MergeResult& result)
{

	ConflictOperation ourOperation = getOurConflictOperation(conflictKey);
	ConflictOperation theirOperation = getTheirConflictOperation(conflictKey);

	string ourLookup = rebasePath (conflictKey, task.mergeRoot, task.ourParent);
	string theirLookup = rebasePath (conflictKey, task.mergeRoot, task.theirParent);

	switch (ourOperation)
	{
	case CONFLICT_SAME:
		if (theirOperation == CONFLICT_MODIFY || theirOperation == CONFLICT_ADD)
		{
			Key source = task.theirs.lookup(theirLookup);
			conflictKey.setString(source.getString());
			result.resolveConflict(conflictKey);
			result.addMergeKey(conflictKey);
		}

		if (theirOperation == CONFLICT_DELETE)
		{
			result.resolveConflict(conflictKey);
		}
		break;
	case CONFLICT_MODIFY:
	case CONFLICT_ADD:
		if (theirOperation == CONFLICT_SAME)
		{
			Key source = task.ours.lookup(ourLookup);
			conflictKey.setString(source.getString());
			result.resolveConflict(conflictKey);
			result.addMergeKey(conflictKey);
		}
		break;
	case CONFLICT_DELETE:
		if (theirOperation == CONFLICT_SAME)
		{
			result.resolveConflict(conflictKey);
		}
		break;
	case CONFLICT_META:
		break;
	}

}

}
}
}

