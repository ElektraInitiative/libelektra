/**
 * \file
 *
 * \brief Implements a way to build and deal with a backend
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */
#ifndef MERGERESULT_HPP_
#define MERGERESULT_HPP_

#include <kdb.hpp>
#include <merging/mergeconflict.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{

class MergeResult
{
public:
	MergeResult();
	MergeResult(KeySet& conflictSet, KeySet& mergedKeys);
	~MergeResult()
	{
	}

	void addConflict(Key& key, ConflictOperation ourOperation,
			ConflictOperation theirOperation);
	void addMergeKey(Key& key)
	{
		mergedKeys.append (key);
	}
	void removeMergeKey(Key& key);
	bool hasConflicts()
	{
		return conflictSet.size () != 0;
	}

	KeySet getConflictSet()
	{
		return conflictSet;
	}
	KeySet getMergedKeys()
	{
		return mergedKeys;
	}

private:
	KeySet conflictSet;
	KeySet mergedKeys;

	void addConflictMeta(Key& key, std::string const & who,
			ConflictOperation operation);

};

}
}
}

#endif /* MERGERESULT_HPP_ */
