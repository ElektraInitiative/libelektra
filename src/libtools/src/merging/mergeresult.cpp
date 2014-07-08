/**
 * \file
 *
 * \brief Implementation of MergeResult
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <merging/mergeresult.hpp>

using namespace std;

namespace kdb
{

namespace tools
{

namespace merging
{

MergeResult::MergeResult()
{
}

MergeResult::MergeResult(KeySet& _conflictSet, KeySet& _mergedKeys)
{
	conflictSet = _conflictSet;
	mergedKeys = _mergedKeys;
}

// TODO: consider making key const
void MergeResult::addConflict(Key& key, ConflictOperation ourOperation,
		ConflictOperation theirOperation)
{
	key.copyAllMeta(NULL);
	key.setString("");
	removeMergeKey (key);
	key.setMeta ("conflict/operation/our",
			MergeConflictOperation::getFromTag (ourOperation));
	key.setMeta ("conflict/operation/their",
			MergeConflictOperation::getFromTag (theirOperation));
	conflictSet.append (key);
}

void MergeResult::resolveConflict(Key& key)
{
	key.rewindMeta();
	Key currentMeta;
	while ((currentMeta = key.nextMeta()))
	{
		// TODO: this is just a workaround because keys with a prefix other than
		// user/ or system/ cannot be created and therefore isBelow cannot be used
		if (currentMeta.getName().find("conflict/") == 0)
		{
			// TODO: this is just a workaround for the meta deletion bug #8
			// key.setMeta(currentMeta.getName(), NULL);
			ckdb::keySetMeta(key.getKey(), currentMeta.getName().c_str(), 0);

		}
	}

	this->conflictSet.lookup(key, KDB_O_POP);
}

}
}
}
