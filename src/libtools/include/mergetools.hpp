#ifndef MERGETOOLS_HPP
#define MERGETOOLS_HPP

#include <kdb.hpp>
#include <toolexcept.hpp>
#include <string>

namespace kdb{

namespace tools{

enum ConflictOperation {APPEND, DELETE, MODIFY};

class MergeResult
{
public:
	MergeResult();
	MergeResult(const KeySet& conflictSet, const KeySet& mergedKeys);
	~MergeResult();

	void addConflict(Key& key, ConflictOperation ourOperation, ConflictOperation theirOperation);

	void addMergeKey(Key& key) { mergedKeys.append(key); }
	bool hasConflicts() { return conflictSet.size() != 0; }
	KeySet getConflictSet() { return conflictSet; }
	KeySet getMergedKeys() { return mergedKeys; }

private:
	KeySet conflictSet;
	KeySet mergedKeys;

	void addConflictMeta(Key& key, std::string const & who, ConflictOperation operation);

};

class ThreeWayMerge
{

public:	

	ThreeWayMerge();
	~ThreeWayMerge();

	static MergeResult mergeKeySet(const KeySet& base, const KeySet& ours, const KeySet& theirs, Key& mergeRoot);
	static MergeResult mergeKeySet(const kdb::KeySet&, const kdb::Key&,
			const kdb::KeySet&, const kdb::Key&, const kdb::KeySet&,
			const kdb::Key&, kdb::Key&);

// TODO: move to own class
private:
	static Key rebaseKey(const Key& key, const Key& oldParent, const Key& newParent);
	static KeySet rebaseKeySet(const KeySet& keySet, const Key& oldParent, const Key& newParent);
	static bool keyDataEqual(const kdb::Key&, const kdb::Key&);
	static bool keySetEqual(const kdb::KeySet& ks1, const Key& parentKey1, const kdb::KeySet& ks2, const Key& parentKey2);
	static KeySet getKeySetDifference(const KeySet& keySetA, const Key& parentA, const KeySet& keySetB, const Key& parentB);

};

}

}

#endif
