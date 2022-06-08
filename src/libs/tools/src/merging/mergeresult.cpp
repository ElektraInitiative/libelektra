/**
 * @file
 *
 * @brief Implementation of MergeResult
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
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

MergeResult::MergeResult () : resolvedKeys (0)
{
}

MergeResult::MergeResult (KeySet & _conflictSet, KeySet & _mergedKeys)
: conflictSet (_conflictSet), mergedKeys (_mergedKeys), resolvedKeys (0)
{
}

void MergeResult::addConflict (Key & key, ConflictOperation ourOperation, ConflictOperation theirOperation)
{
	ckdb::KeySet * metaKeys = ckdb::keyMeta (key.getKey ());

	for (ssize_t it = 0; it < ckdb::ksGetSize (metaKeys); ++it)
	{
		// TODO: Test if deletion works with foreach-loop
		const Key & currentMeta = ckdb::ksAtCursor (metaKeys, it);
		key.delMeta (currentMeta.getName ());
	}

	if (key.isString ())
	{
		key.setString ("");
	}
	else
	{
		key.setBinary (nullptr, 0);
	}

	removeMergeKey (key);
	key.setMeta ("conflict/operation/our", MergeConflictOperation::getFromTag (ourOperation));
	key.setMeta ("conflict/operation/their", MergeConflictOperation::getFromTag (theirOperation));
	conflictSet.append (key);
}

void MergeResult::resolveConflict (Key & key)
{
	ckdb::KeySet * metaKeys = ckdb::keyMeta (key.getKey ());

	for (ssize_t it = 0; it < ckdb::ksGetSize (metaKeys); ++it)
	{
		const Key & currentMeta = ckdb::ksAtCursor (metaKeys, it);
		// TODO: this is just a workaround because keys with a prefix other than
		// user:/ or system:/ cannot be created and therefore isBelow cannot be used
		if (currentMeta.getName ().find ("meta:/conflict/") == 0)
		{
			// TODO: Test if deletion works with foreach-loop
			key.delMeta (currentMeta.getName ());
			--it; // metakey deleted, so next metakey is on same position as the deleted one
		}
	}

	conflictSet.lookup (key, KDB_O_POP);
	resolvedKeys++;
}
} // namespace merging
} // namespace tools
} // namespace kdb
