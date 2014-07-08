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

class MergeConflictStrategy
{

public:
	virtual ~MergeConflictStrategy() {};
	virtual void resolveConflict(const MergeTask& task, Key& conflictKey, MergeResult& result) = 0;
};

}
}
}

#endif /* MERGECONFLICTSTRATEGY_HPP_ */
