/**
 * @file
 *
 * @brief Implementation of OneSideStrategy
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <string>
#include <helper/keyhelper.hpp>
#include <merging/newkeystrategy.hpp>

using namespace std;
using namespace kdb::tools::helper;

namespace kdb
{

namespace tools
{

namespace merging
{

void NewKeyStrategy::resolveConflict(const MergeTask& task, Key& conflictKey, MergeResult& result)
{

	ConflictOperation ourOperation = getOurConflictOperation(conflictKey);
	ConflictOperation theirOperation = getTheirConflictOperation(conflictKey);

	string ourLookup = rebasePath (conflictKey, task.mergeRoot, task.ourParent);
	string theirLookup = rebasePath (conflictKey, task.mergeRoot, task.theirParent);

	// TODO: this is a subset of the automergestrategy
	// the automergestrategy could be split up into several smaller strategies
	switch (ourOperation)
	{
	case CONFLICT_SAME:
		if (theirOperation == CONFLICT_ADD)
		{
			Key source = task.theirs.lookup(theirLookup);
			conflictKey.setString(source.getString());
			result.resolveConflict(conflictKey);
			result.addMergeKey(conflictKey);
		}
		break;
	case CONFLICT_ADD:
		if (theirOperation == CONFLICT_SAME)
		{
			Key source = task.ours.lookup(ourLookup);
			conflictKey.setString(source.getString());
			result.resolveConflict(conflictKey);
			result.addMergeKey(conflictKey);
		}
		break;
	default:
		break;
	}
}

}
}
}

