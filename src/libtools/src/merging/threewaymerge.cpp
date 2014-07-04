/**
 * \file
 *
 * \brief Implements a way to build and deal with a backend
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


// TODO: compare metakeys
void ThreeWayMerge::automaticMerge(const MergeTask& task,
		MergeResult& mergeResult, bool reverseConflictMeta = false)
{
	Key our;
	cursor_t savedCursor = task.ours.getCursor ();
	task.ours.rewind ();

	while ((our = task.ours.next ()))
	{
		string theirLookup = rebasePath (our, task.ourParent, task.theirParent);
		Key theirLookupResult = task.theirs.lookup (theirLookup);
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
			string baseLookup = rebasePath (our, task.ourParent,
					task.baseParent);
			Key baseLookupResult = task.base.lookup (baseLookup);

			// check if the keys was newly added in ours
			if (baseLookupResult)
			{
				// the key exists in base, check if the key still exists in theirs
				if (theirLookupResult)
				{
					// check if only they modified it
					if (!keyDataEqual (our, baseLookupResult)
							&& keyDataEqual (theirLookupResult,
									baseLookupResult))
					{
						// the key was only modified in theirs, take their version
						mergeResult.addMergeKey (mergeKey);
					}
					else
					{
						// check if both modified it
						if (!keyDataEqual (our, baseLookupResult)
								&& !keyDataEqual (theirLookupResult,
										baseLookupResult))
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
						mergeResult.removeMergeKey (mergeKey);
					}
					else
					{
						// the key was deleted in theirs, but modified in ours
						if (!reverseConflictMeta)
							mergeResult.addConflict (mergeKey, MODIFY, DELETE);
						else
							mergeResult.addConflict (mergeKey, DELETE, MODIFY);

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
						// the key was added on both sides with the same value
						mergeResult.addMergeKey (mergeKey);
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
					mergeResult.addMergeKey (mergeKey);
				}
			}
		}
	}

	task.ours.setCursor (savedCursor);
}

/**
 * Performs a threeway merge according to the supplied MergeTask. All merged keys will
 * be below the given mergeParent in the MergeTask. Found conflicts will be
 * reported in the MergeResult. Conflicts are below the mergeParent as well and
 * are not part of the mergedKeys.
 *
 * @see MergeTask
 * @see MergeResult
 *
 * @param task a MergeTask describing the intended merge oparation
 * @return a MergeResult that contains the merged keys as well as all found conflicts.
 *
 **/
MergeResult ThreeWayMerge::mergeKeySet(const MergeTask& task)
{

	MergeResult result;
	automaticMerge (task, result);
	automaticMerge (task.reverse (), result, true);
	return result;
}


/**
 * Performs a threeway merge based on the supplied KeySets. The result is the same
 * as for ThreeWayMerge::mergeKeySet(const MergeTask&). The first key (i.e. the shortest)
 * in each of the supplied KeySets is considered to be the corresponding parentKey.
 * This means that the parent key of each KeySet MUST be part of the KeySet.
 *
 * @see ThreeWayMerge::mergeKeySet(const MergeTask&)
 *
 * @param base the KeySet containing the base keys and the base parentKey
 * @param ours the KeySet containing our keys and our parentKey
 * @param theirs the KeySet containing their keys and their parentKey
 * @param meregRoot the parentKey for the merged keys
 * @return a MergeResult that contains the merged keys as well as all found conflicts.
 */
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
