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

MergeResult::MergeResult(const KeySet& _conflictSet, const KeySet& _mergedKeys)
{
	conflictSet = _conflictSet;
	mergedKeys = _mergedKeys;
}

// TODO: this implementation is extremely inefficient and just a workaround
void MergeResult::removeMergeKey(Key& key)
{
	cursor_t cursor = mergedKeys.getCursor ();
	mergedKeys.rewind ();

	Key current;
	KeySet result;
	while ((current = mergedKeys.next ()))
	{
		if (current != key) result.append (current.dup ());
	}

	mergedKeys.setCursor (cursor);
	mergedKeys = result;
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
