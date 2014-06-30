/**
 * \file
 *
 * \brief Implements a way to build and deal with a backend
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <merging/threewaymerge.hpp>

using namespace std;

namespace kdb
{

namespace tools
{

namespace merging
{

/**
 * @brief Determines if two keys are equal based on their GetString() values.
 * Returns true if they are equal. False if they are not.
 */
bool ThreeWayMerge::keyDataEqual(const kdb::Key& k1, const kdb::Key& k2)
{
	if (!k1 && k2) return false;
	if (k1 && !k2) return false;
	if (k1.getString () != k2.getString ())
	{
		return false;
	}
	return true;
}

string ThreeWayMerge::rebasePath(const Key& key, const Key& oldParent,
		const Key& newParent)
{
	string oldPath = key.getFullName ();
	string relativePath = oldPath.substr (oldParent.getFullName ().length (),
			oldPath.length ());
	string newPath = newParent.getFullName () + relativePath;

	return newPath;
}

Key ThreeWayMerge::rebaseKey(const Key& key, const Key& oldParent,
		const Key& newParent)
{
	Key result = key.dup ();
	result.setName (rebasePath (key, oldParent, newParent));
	return result;
}

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
			// keys match, nothing to do
			mergeResult.addMergeKey (mergeKey);
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
 * Returns a keyset that is the result of a merge on two keysets (ours and theirs) using a base keyset as a refernece (a three-way merge). 
 * If the merge function is unscuessful an empty KeySet will be returned. 
 * This function is inteded as a full version for the kdb merge command or for  the C++ API. 
 * It works by taking in three keysets, their parent keys and a parent key for where to store the merged KeySet.
 **/
MergeResult ThreeWayMerge::mergeKeySet(const MergeTask& task)
{

	MergeResult result;
	automaticMerge (task, result);
	automaticMerge (task.reverse (), result, true);
	return result;
}


/**
 *
 * Returns a keyset that is the result of a merge on two keysets (ours and theirs) using a base keyset as a refernece (a three-way merge).
 * If the merge function is unscuessful an empty KeySet will be returned.
 * This function is inteded as a basic version for the C++ API. It takes in three keysets and a parent key for where to store the merged keys.
 * It works by finidng the parent key for each keyset and then calling the more complex function above.
**/
MergeResult ThreeWayMerge::mergeKeySet(const KeySet& base, const KeySet& ours, const KeySet& theirs, Key& mergeRoot){
	Key ourkey = ours.head().dup();
	Key theirkey = theirs.head().dup();
	Key basekey = base.head().dup();

	MergeResult merged = mergeKeySet(MergeTask(BaseMergeKeys(base, basekey), OurMergeKeys(ours, ourkey), TheirMergeKeys(theirs, theirkey), mergeRoot));

	return merged;
}

}
}
}
