/**
 * \file
 *
 * \brief A strategy for taking the value of
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */
#ifndef ONESIDESTRATEGY_HPP_
#define ONESIDESTRATEGY_HPP_

#include <merging/mergeconflictstrategy.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{

class AutoMergeStrategy : public MergeConflictStrategy
{
public:
	virtual void resolveConflict(const MergeTask& task, Key& conflictKey, MergeResult& result);
};

}
}
}

#endif /* ONESIDESTRATEGY_HPP_ */
