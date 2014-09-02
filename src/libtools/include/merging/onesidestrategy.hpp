/**
 * \file
 *
 * \brief A strategy which always takes the value from one side
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

class OneSideStrategy : public MergeConflictStrategy
{

public:
	ConflictResolutionSide winningSide;

	OneSideStrategy(ConflictResolutionSide _winningSide) : winningSide (_winningSide)
	{
	}

	virtual void resolveConflict(const MergeTask& task, Key& conflictKey, MergeResult& result);
};

}
}
}

#endif /* ONESIDESTRATEGY_HPP_ */
