/**
 * @file
 *
 * @brief Class modelling the result of a three way merge
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
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
	MergeResult ();
	MergeResult (KeySet & conflictSet, KeySet & mergedKeys);
	~MergeResult ()
	{
	}

	void addConflict (Key & key, ConflictOperation ourOperation, ConflictOperation theirOperation);

	void resolveConflict (Key & key);

	bool isConflict (const Key & key)
	{
		return conflictSet.lookup (key);
	}

	bool hasConflicts ()
	{
		return conflictSet.size () != 0;
	}

	void addMergeKey (const Key & key)
	{
		if (!mergedKeys.lookup (key))
		{
			mergedKeys.append (key);
		}
	}

	void removeMergeKey (const Key & key)
	{
		mergedKeys.lookup (key, KDB_O_POP);
	}

	KeySet getConflictSet ()
	{
		return conflictSet;
	}

	KeySet getMergedKeys ()
	{
		return mergedKeys;
	}

	unsigned int getNumberOfResolvedKeys ()
	{
		return resolvedKeys;
	}

	unsigned int getNumberOfEqualKeys ()
	{
		return mergedKeys.size () - resolvedKeys;
	}

private:
	KeySet conflictSet;
	KeySet mergedKeys;
	unsigned int resolvedKeys;

	void addConflictMeta (Key & key, std::string const & who, ConflictOperation operation);
};
} // namespace merging
} // namespace tools
} // namespace kdb

#endif /* MERGERESULT_HPP_ */
