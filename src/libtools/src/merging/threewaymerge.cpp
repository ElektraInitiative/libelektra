/**
 * \file
 *
 * \brief Implementation of ThreeWayMerge
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <merging/threewaymerge.hpp>
#include <helper/comparison.hpp>
#include <helper/keyhelper.hpp>

using namespace std;
using namespace kdb::tools::helper;

namespace kdb
{

namespace tools
{

namespace merging
{

inline void addAsymmetricConflict(MergeResult& result, Key& key, ConflictOperation our, ConflictOperation their, bool reverse)
{
	if (!reverse)
	{
		result.addConflict (key, our, their);
	}
	else
	{
		result.addConflict (key, their, our);
	}
}

void ThreeWayMerge::detectConflicts(const MergeTask& task, MergeResult& mergeResult, bool reverseConflictMeta = false)
{
	Key our;
	cursor_t savedCursor = task.ours.getCursor ();
	task.ours.rewind ();

	while ((our = task.ours.next ()))
	{
		string theirLookup = rebasePath (our, task.ourParent, task.theirParent);
		Key theirLookupResult = task.theirs.lookup (theirLookup);

		// we have to copy it to obtain owner etc...
		Key mergeKey = rebaseKey (our, task.ourParent, task.mergeRoot);

		if (keyDataEqual (our, theirLookupResult))
		{
			// keydata matches, see if metakeys match
			if (keyMetaEqual (our, theirLookupResult))
			{
				mergeResult.addMergeKey (mergeKey);
			}
			else
			{
				// metakeys are different
				mergeResult.addConflict (mergeKey, META, META);
			}
		}
		else
		{
			string baseLookup = rebasePath (our, task.ourParent, task.baseParent);
			Key baseLookupResult = task.base.lookup (baseLookup);

			// check if the keys was newly added in ours
			if (baseLookupResult)
			{
				// the key exists in base, check if the key still exists in theirs
				if (theirLookupResult)
				{
					// check if only they modified it
					if (!keyDataEqual (our, baseLookupResult) && keyDataEqual (theirLookupResult, baseLookupResult))
					{
						// the key was only modified in ours
						addAsymmetricConflict (mergeResult, mergeKey, MODIFY, SAME, reverseConflictMeta);
					}
					else
					{
						// check if both modified it
						if (!keyDataEqual (our, baseLookupResult) && !keyDataEqual (theirLookupResult, baseLookupResult))
						{
							// the key was modified on both sides
							mergeResult.addConflict (mergeKey, MODIFY, MODIFY);
						}
					}
				}
				else
				{
					// the key does not exist in theirs anymore, check if ours has modified it
					if (keyDataEqual (our, baseLookupResult))
					{
						// the key was deleted in theirs, and not modified in ours
						addAsymmetricConflict (mergeResult, mergeKey, SAME, DELETE, reverseConflictMeta);
					}
					else
					{
						// the key was deleted in theirs, but modified in ours
						addAsymmetricConflict (mergeResult, mergeKey, MODIFY, DELETE, reverseConflictMeta);
					}
				}
			}
			else
			{
				// the key does not exist in base, check if the key was added in theirs
				if (theirLookupResult)
				{
					// check if the key was added with the same value in theirs
					if (keyDataEqual (mergeKey, theirLookupResult))
					{
						if (keyMetaEqual (our, theirLookupResult))
						{
							// the key was added on both sides with the same value
							mergeResult.addMergeKey (mergeKey);
						}
						else
						{
							// metakeys are different
							mergeResult.addConflict (mergeKey, META, META);
						}
					}
					else
					{
						// the key was added on both sides with different values
						mergeResult.addConflict (mergeKey, ADD, ADD);
					}
				}
				else
				{
					// the key was only added to ours
					addAsymmetricConflict (mergeResult, mergeKey, ADD, SAME, reverseConflictMeta);
				}
			}
		}
	}

	task.ours.setCursor (savedCursor);
}


MergeResult ThreeWayMerge::mergeKeySet(const MergeTask& task)
{

	MergeResult result;
	detectConflicts (task, result);
	detectConflicts (task.reverse (), result, true);

	if (!result.hasConflicts()) return result;

	Key current;
	KeySet conflicts = result.getConflictSet();
	conflicts.rewind();
	while ((current = conflicts.next ()))
	{
		for (vector<MergeConflictStrategy *>::iterator it = strategies.begin (); it != strategies.end (); ++it)
		{
			(*it)->resolveConflict (task, current, result);
		}
	}

	return result;
}

MergeResult ThreeWayMerge::mergeKeySet(const KeySet& base, const KeySet& ours,
		const KeySet& theirs, Key& mergeRoot)
{
	Key ourkey = ours.head ().dup ();
	Key theirkey = theirs.head ().dup ();
	Key basekey = base.head ().dup ();

	MergeResult merged = mergeKeySet (
			MergeTask (BaseMergeKeys (base, basekey),
					OurMergeKeys (ours, ourkey),
					TheirMergeKeys (theirs, theirkey), mergeRoot));

	return merged;
}

}
}
}
