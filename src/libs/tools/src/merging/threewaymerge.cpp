/**
 * @file
 *
 * @brief Implementation of ThreeWayMerge
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <helper/comparison.hpp>
#include <helper/keyhelper.hpp>
#include <merging/threewaymerge.hpp>

using namespace std;
using namespace kdb::tools::helper;

namespace kdb
{

namespace tools
{

namespace merging
{

inline void addAsymmetricConflict (MergeResult & result, Key & key, ConflictOperation our, ConflictOperation their, bool reverse)
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

void ThreeWayMerge::detectConflicts (const MergeTask & task, MergeResult & mergeResult, bool reverseConflictMeta = false)
{
	for (Key our : task.ours)
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
				if (task.ourParent.getName () == task.mergeRoot.getName ())
				{
					// the key was not rebased, we can reuse our (prevents that the key is rewritten)
					mergeResult.addMergeKey (our);
				}
				else
				{
					// the key causes no merge conflict, but the merge result is below a new parent
					mergeResult.addMergeKey (mergeKey);
				}
			}
			else
			{
				// metakeys are different
				mergeResult.addConflict (mergeKey, CONFLICT_META, CONFLICT_META);
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
						addAsymmetricConflict (mergeResult, mergeKey, CONFLICT_MODIFY, CONFLICT_SAME,
								       reverseConflictMeta);
					}
					else
					{
						// check if both modified it
						if (!keyDataEqual (our, baseLookupResult) &&
						    !keyDataEqual (theirLookupResult, baseLookupResult))
						{
							// the key was modified on both sides
							mergeResult.addConflict (mergeKey, CONFLICT_MODIFY, CONFLICT_MODIFY);
						}
					}
				}
				else
				{
					// the key does not exist in theirs anymore, check if ours has modified it
					if (keyDataEqual (our, baseLookupResult))
					{
						// the key was deleted in theirs, and not modified in ours
						addAsymmetricConflict (mergeResult, mergeKey, CONFLICT_SAME, CONFLICT_DELETE,
								       reverseConflictMeta);
					}
					else
					{
						// the key was deleted in theirs, but modified in ours
						addAsymmetricConflict (mergeResult, mergeKey, CONFLICT_MODIFY, CONFLICT_DELETE,
								       reverseConflictMeta);
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
							if (task.ourParent.getName () == task.mergeRoot.getName ())
							{
								// the key was not rebased, we can reuse our and prevent the sync flag being
								// set
								mergeResult.addMergeKey (our);
							}
							else
							{
								// the key causes no merge conflict, but the merge result is below a new
								// parent
								mergeResult.addMergeKey (mergeKey);
							}
						}
						else
						{
							// metakeys are different
							mergeResult.addConflict (mergeKey, CONFLICT_META, CONFLICT_META);
						}
					}
					else
					{
						// the key was added on both sides with different values
						mergeResult.addConflict (mergeKey, CONFLICT_ADD, CONFLICT_ADD);
					}
				}
				else
				{
					// the key was only added to ours
					addAsymmetricConflict (mergeResult, mergeKey, CONFLICT_ADD, CONFLICT_SAME, reverseConflictMeta);
				}
			}
		}
	}
}


MergeResult ThreeWayMerge::mergeKeySet (const MergeTask & task)
{

	MergeResult result;
	detectConflicts (task, result);
	detectConflicts (task.reverse (), result, true);

	if (!result.hasConflicts ()) return result;


	// TODO: test this behaviour (would probably need mocks)
	KeySet conflicts = result.getConflictSet ();

	for (Key current : conflicts)
	{
		for (auto & elem : strategies)
		{
			(elem)->resolveConflict (task, current, result);

			if (!result.isConflict (current)) break;
		}
	}

	return result;
}

MergeResult ThreeWayMerge::mergeKeySet (const KeySet & base, const KeySet & ours, const KeySet & theirs, const Key & mergeRoot)
{
	Key ourkey = ours.at (0).dup ();
	Key theirkey = theirs.at (0).dup ();
	Key basekey = base.at (0).dup ();

	MergeResult merged = mergeKeySet (
		MergeTask (BaseMergeKeys (base, basekey), OurMergeKeys (ours, ourkey), TheirMergeKeys (theirs, theirkey), mergeRoot));

	return merged;
}
} // namespace merging
} // namespace tools
} // namespace kdb
