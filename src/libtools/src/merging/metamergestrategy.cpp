/**
 * @file
 *
 * @brief Implementation of MetaMergeStrategy
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <string>
#include <merging/metamergestrategy.hpp>
#include <helper/keyhelper.hpp>

using namespace std;
using namespace kdb::tools::helper;

namespace kdb
{

namespace tools
{

namespace merging
{

KeySet MetaMergeStrategy::getMetaKeys(Key& key)
{
	KeySet result;

	if (key)
	{
		key.rewindMeta ();
		Key currentMeta;
		while ((currentMeta = key.nextMeta ()))
		{
			string resultName = "user/" + currentMeta.getName();
			Key resultMeta = Key(resultName.c_str(), KEY_VALUE, currentMeta.getString().c_str(), KEY_END);
			result.append(resultMeta);
		}
	}

	return result;
}

void MetaMergeStrategy::resolveConflict(const MergeTask& task, Key& conflictKey, MergeResult& result)
{
	conflictKey.rewindMeta();
	Key currentMeta;

	string baseLookup = rebasePath (conflictKey, task.mergeRoot, task.baseParent);
	string ourLookup = rebasePath (conflictKey, task.mergeRoot, task.ourParent);
	string theirLookup = rebasePath (conflictKey, task.mergeRoot, task.theirParent);

	Key baseKey = task.base.lookup(baseLookup);
	Key ourKey = task.ours.lookup(ourLookup);
	Key theirKey = task.theirs.lookup(theirLookup);

	Key root ("user/", KEY_END);
	KeySet baseMeta = getMetaKeys (baseKey);
	KeySet ourMeta = getMetaKeys (ourKey);
	KeySet theirMeta = getMetaKeys (theirKey);

	MergeTask metaTask(BaseMergeKeys (baseMeta, root), OurMergeKeys (ourMeta, root),
			TheirMergeKeys (theirMeta, root), root);

	MergeResult metaResult = innerMerger.mergeKeySet(metaTask);

	KeySet mergedMeta = metaResult.getMergedKeys();
	Key current;
	mergedMeta.rewind();
	while ((current = mergedMeta.next()))
	{
		string metaName = current.getName().substr(string("user/").length());
		conflictKey.setMeta(metaName, current.getString());
	}

	ConflictOperation ourOperation = getOurConflictOperation(conflictKey);
	ConflictOperation theirOperation = getTheirConflictOperation(conflictKey);

	if (!metaResult.hasConflicts ())
	{
		if (ourOperation == CONFLICT_META && theirOperation == CONFLICT_META)
		{
			// TODO: addConflict deletes the key content
			// without this strategy restoring the value the value would be lost
			// this happens only for CONFLICT_META <--> CONFLICT_META conflicts
			// add a test for this behaviour
			conflictKey.setString(ourKey.getString());
			result.resolveConflict (conflictKey);
			result.addMergeKey (conflictKey);
		}
	}

}

}
}
}


