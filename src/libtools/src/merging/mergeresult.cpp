/**
 * \file
 *
 * \brief Implements a way to build and deal with a backend
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <merging/mergeresult.hpp>

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

void MergeResult::removeMergeKey(Key& key)
{
	mergedKeys.lookup(key, KDB_O_POP);
}

void MergeResult::addConflict(Key& key, ConflictOperation ourOperation,
		ConflictOperation theirOperation)
{
	removeMergeKey (key);
	key.setMeta ("conflict/operation/our",
			MergeConflictOperation::getFromTag (ourOperation));
	key.setMeta ("conflict/operation/their",
			MergeConflictOperation::getFromTag (theirOperation));
	conflictSet.append (key);
}

}
}
}
