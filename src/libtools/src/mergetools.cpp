/**
 * \file
 *
 * \brief Implementation of mergetools
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <mergetools.hpp>

#include <kdb.hpp>
#include <modules.hpp>
#include <keysetio.hpp>

#include <iostream>
#include <string>

using namespace std;
using namespace kdb;

namespace kdb{

namespace tools{

ThreeWayMerge::ThreeWayMerge()
{}

/**
 * @brief Determines if two keys are equal based on their GetString() values.
 * Returns true if they are equal. False if they are not.
 */
// TODO: should compare metakeys
bool ThreeWayMerge::keyDataEqual(const kdb::Key& k1, const kdb::Key& k2){
	if(k1.getString() != k2.getString()){
		return false;
	}
	return true;
}

string getKeyRelativeName(const Key& key, const Key& parent)
{
	return key.getFullName().substr(parent.getFullName().size());
}

/**
 * @brief Determines if two KeySets are equal.
 * KeySets are considered equal if they are the same size and both keysets have the same keys determined by their name and KeyEqual.
 */
bool ThreeWayMerge::keySetEqual(const kdb::KeySet& ks1, const Key& parentKey1, const kdb::KeySet& ks2, const Key& parentKey2)
{

	if (ks1.size () != ks2.size ()) return false;

	cursor_t c1 = ks1.getCursor ();
	cursor_t c2 = ks2.getCursor ();
	ks1.rewind ();
	ks2.rewind ();

	Key k1;
	Key k2;
	bool equal = true;
	while ((k1 = ks1.next ()))
	{
		k2 = ks2.next ();
		if (getKeyRelativeName(k1, parentKey1) != getKeyRelativeName(k2, parentKey2))
		{
			//cerr << "Name mismatch!" << endl << "k1 name: " << k1.getBaseName() << " k2 name: " << k2.getBaseName() << endl;
			equal = false;
			break;
		}
		else if (!keyDataEqual (k1, k2))
		{
			equal = false;
			break;
		}

	}

	ks1.setCursor (c1);
	ks2.setCursor (c2);

	return equal;
}

Key ThreeWayMerge::rebaseKey(const Key& key, const Key& oldParent, const Key& newParent)
{
	Key result = key.dup();
	string oldPath = key.getFullName();
	string relativePath = oldPath.substr(oldParent.getFullName().length(), oldPath.length());
	result.setName(newParent.getFullName() + relativePath);
	return result;
}

KeySet ThreeWayMerge::rebaseKeySet(const KeySet& keySet, const Key& oldParent, const Key& newParent)
{
	KeySet result;
	cursor_t cursor = keySet.getCursor();
	keySet.rewind();

	Key current;
	while ((current = keySet.next()))
	{
		result.append(rebaseKey(current, oldParent, newParent));
	}
	keySet.setCursor(cursor);
	return result;
}

KeySet ThreeWayMerge::getKeySetDifference(const KeySet& keySetA, const Key& parentA, const KeySet& keySetB, const Key& parentB)
{
	cursor_t cursorA = keySetA.getCursor();

	KeySet result;
	Key current;
	keySetA.rewind();

	/* get keys unique to A */
	while ((current = keySetA.next()))
	{
		string currentRelativePath = current.getFullName().substr(parentA.getFullName().size());
		string lookupPath = parentB.getFullName() + currentRelativePath;

		Key lookupKey = keySetB.lookup(lookupPath.c_str());

		if (!lookupKey)
		{
			result.append(current);
		}
		else if (!keyDataEqual(lookupKey, current))
		{
			result.append(current);
		}
	}

	keySetA.setCursor(cursorA);
	return result;
}


/**
 * Returns a keyset that is the result of a merge on two keysets (ours and theirs) using a base keyset as a refernece (a three-way merge). 
 * If the merge function is unscuessful an empty KeySet will be returned. 
 * This function is inteded as a full version for the kdb merge command or for  the C++ API. 
 * It works by taking in three keysets, their parent keys and a parent key for where to store the merged KeySet.
**/
MergeResult ThreeWayMerge::mergeKeySet(const KeySet& base, const Key& baseParent,
		const KeySet& ours, const Key& ourParent, const KeySet& theirs,
		const Key& theirParent, Key& mergeParent)
{
	Key ourkey;
	Key theirkey;
	Key basekey;
	Key mergekey;
	KeySet merged;
	MergeResult result;

	cursor_t baseCursor = base.getCursor();
	cursor_t oursCursor = ours.getCursor();
	cursor_t theirsCursor = theirs.getCursor();

	base.rewind();
	ours.rewind();
	theirs.rewind();

	
	string base_lookup;
	string our_lookup;
	string their_lookup;
	string base_path;
	string our_path;
	string their_path;
	string merge_path;

	//Get path of each keyset
  	base_path = baseParent.getName();
  	our_path = ourParent.getName();
  	their_path = theirParent.getName();
	merge_path = mergeParent.getName();


	/*
	KeySet oursMinusTheirs = getKeySetDifference(ours, ourParent, theirs, theirParent);
	KeySet theirsMinusOurs = getKeySetDifference(theirs, theirParent, ours, ourParent);
	KeySet differenceSet = KeySet(oursMinusTheirs);
	differenceSet.append(theirsMinusOurs);

	differenceSet.rewind();
	Key current;

	while ((current = differenceSet.next()))
	{

	}
	*/

	/* nothing to merge -> take ours */
	if(keySetEqual(ours, ourParent, theirs, theirParent)){
		merged = rebaseKeySet(ours, ourParent, mergeParent);
		return MergeResult(KeySet(), merged);
	}

	/* theirs was not changed, but ours -> take ours */
	if(keySetEqual(base, baseParent, theirs, theirParent)){
		merged = rebaseKeySet(ours, ourParent, mergeParent);
		return MergeResult(KeySet(), merged);
	}

	/* ours was not changed, but theirs -> take theirs */
	if(keySetEqual(base, baseParent, ours, ourParent)){
		merged = rebaseKeySet(theirs, theirParent, mergeParent);
		return MergeResult(KeySet(), merged);
	}

		//Iterate though ours and theirs and check each key. If keys don't match, refernece base.

		//Iterate over keysets.
		//Iterate over base first. Look for keys in all three KeySets and try to merge those keys.
		base.rewind();
		while(basekey=base.next()){
			base_lookup = base_path + basekey.getName().substr(base_path.length());
			our_lookup = our_path + basekey.getName().substr(base_path.length());
			their_lookup = their_path + basekey.getName().substr(base_path.length());
			//If a key exists in all three keysets, compare it and append it to merged unless their is a conflict.
			if(ours.lookup(our_lookup.c_str()) && theirs.lookup(their_lookup.c_str())){
				ourkey = ours.lookup(our_lookup.c_str());
				theirkey = theirs.lookup(their_lookup.c_str());
				if(keyDataEqual(ourkey, theirkey)){
					mergekey = ourkey.dup();
					mergekey.setName(merge_path + ourkey.getName().substr(our_path.length()));
					result.addMergeKey(mergekey);
				}
				else if(keyDataEqual(basekey, theirkey)){
					mergekey = ourkey.dup();
					mergekey.setName(merge_path + ourkey.getName().substr(our_path.length()));
					result.addMergeKey(mergekey);
				}
				else if(keyDataEqual(basekey, ourkey)){
					mergekey = theirkey.dup();
					mergekey.setName(merge_path + theirkey.getName().substr(their_path.length()));
					result.addMergeKey(mergekey);
				}
				else{
					//conflict!
					cerr << "Key content conflict! basekey = " << basekey << " ourkey = " << ourkey << " theirkey = " << theirkey << endl;
					result.addConflict(ourkey, MODIFY, MODIFY);
				}
			}
		}

		//Iterate over ours. If it has any unique keys, append them to merged.
		ours.rewind();
		while(ourkey=ours.next()){
			base_lookup = base_path + ourkey.getName().substr(our_path.length());
			our_lookup = our_path + ourkey.getName().substr(our_path.length());
			their_lookup = their_path + ourkey.getName().substr(our_path.length());
			//If ours has a key that theirs and base don't have.
			if(!base.lookup(base_lookup.c_str())){
				if(!theirs.lookup(their_lookup.c_str())){
					mergekey = ourkey.dup();
					mergekey.setName(merge_path + ourkey.getName().substr(our_path.length()));
					result.addMergeKey(mergekey);
				}
				//If base doesn't have it but theirs does.
				else{
					theirkey = theirs.lookup(their_lookup.c_str());
					if(!keyDataEqual(ourkey, theirkey)){
						//Conflict!
						cerr << "Key content conflict, no base key, ourkey != theirkey. ourkey = " << ourkey << " theirkey = " << theirkey << endl;
						result.addConflict(ourkey, APPEND, APPEND);
					}
					else{
						mergekey = ourkey.dup();
						mergekey.setName(merge_path + ourkey.getName().substr(our_path.length()));
						result.addMergeKey(mergekey);
					}
				}
			}

		}

		//Iterate over theirs. If it has any unique keys, append them to merged.
		theirs.rewind();
		while(theirkey=theirs.next()){
			base_lookup = base_path + theirkey.getName().substr(their_path.length());
			our_lookup = our_path + theirkey.getName().substr(their_path.length());
			their_lookup = their_path + theirkey.getName().substr(their_path.length());
			//If theirs has a key that ours and base don't have.
			if(!base.lookup(base_lookup.c_str())){
				if(!ours.lookup(our_lookup.c_str())){
					mergekey = theirkey.dup();
					mergekey.setName(merge_path + theirkey.getName().substr(their_path.length()));
					result.addMergeKey(mergekey);
				}
				//If base doesn't have it but ours does.
				else{
					ourkey = ours.lookup(our_lookup.c_str());
					if(!keyDataEqual(theirkey, ourkey)){
						//Conflict!
						cerr << "Key content conflict, no base key, ourkey != theirkey. ourkey = " << ourkey << " theirkey = " << theirkey << endl;
						result.addConflict(ourkey, APPEND, APPEND);
					}
				}
			}
		}



	base.setCursor(baseCursor);
	ours.setCursor(oursCursor);
	theirs.setCursor(theirsCursor);

	//Return merged
	return result;
}

/**
 * @return path of a key.
 */
string getPath(const kdb::Key& key){
	unsigned found;
	string path;
	found = key.getName().find_last_of("/");
  	path = key.getName().substr(0,found+1);
	return path;
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

	MergeResult merged = mergeKeySet(base, basekey, ours, ourkey, theirs, theirkey, mergeRoot);

	return merged;
}




ThreeWayMerge::~ThreeWayMerge()
{}

MergeResult::MergeResult()
{}

MergeResult::MergeResult(const KeySet& _conflictSet, const KeySet& _mergedKeys)
{
	conflictSet = _conflictSet;
	mergedKeys = _mergedKeys;
}

void MergeResult::addConflict(Key& key, ConflictOperation ourOperation, ConflictOperation theirOperation)
{
	addConflictMeta(key, "our", ourOperation);
	addConflictMeta(key, "their", theirOperation);
	conflictSet.append(key);
}

void MergeResult::addConflictMeta(Key& key, std::string const & who, ConflictOperation operation)
{
	std::string metaName = "conflict/" + who;
	switch (operation)
	{
	case APPEND:
		key.setMeta(metaName, "append");
		break;
	case DELETE:
		key.setMeta(metaName, "delete");
		break;
	case MODIFY:
		key.setMeta(metaName, "modify");
		break;
	default:
		key.setMeta(metaName, "unknown");
	}
}


MergeResult::~MergeResult()
{}

}

}

