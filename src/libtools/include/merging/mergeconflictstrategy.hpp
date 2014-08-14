/**
 * \file
 *
 * \brief Interface for a MergeConflictStrategy
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef MERGECONFLICTSTRATEGY_HPP_
#define MERGECONFLICTSTRATEGY_HPP_

#include <merging/mergeresult.hpp>
#include <merging/mergetask.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{

enum ConflictResolutionSide { BASE, OURS, THEIRS };

class MergeConflictStrategy
{

public:
	virtual ~MergeConflictStrategy() {};
	virtual void resolveConflict(const MergeTask& task, Key& conflictKey, MergeResult& result) = 0;

protected:
	virtual ConflictOperation getOurConflictOperation(Key& conflictKey);
	virtual ConflictOperation getTheirConflictOperation(Key& conflictKey);
};

}
}
}

#endif /* MERGECONFLICTSTRATEGY_HPP_ */
