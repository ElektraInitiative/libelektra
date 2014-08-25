/*
 * onesidevaluestrategy.hpp
 *
 *  Created on: 12 Aug 2014
 *      Author: felixl
 */

#ifndef ONESIDEVALUESTRATEGY_HPP_
#define ONESIDEVALUESTRATEGY_HPP_

#include <merging/mergeconflictstrategy.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{

class OneSideValueStrategy : public MergeConflictStrategy
{

public:
	ConflictResolutionSide winningSide;

	OneSideValueStrategy(ConflictResolutionSide _winningSide) : winningSide (_winningSide)
	{
	}

	virtual void resolveConflict(const MergeTask& task, Key& conflictKey, MergeResult& result);
};

}
}
}

#endif /* ONESIDEVALUESTRATEGY_HPP_ */
