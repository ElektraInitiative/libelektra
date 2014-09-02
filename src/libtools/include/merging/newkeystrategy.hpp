/**
 * \file
 *
 * \brief A strategy which always takes the value from one side
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef NEWKEYSTRATEGY_HPP_
#define NEWKEYSTRATEGY_HPP_

#include <merging/mergeconflictstrategy.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{

class NewKeyStrategy : public MergeConflictStrategy
{
public:
	NewKeyStrategy()
	{
	}

	virtual void resolveConflict(const MergeTask& task, Key& conflictKey, MergeResult& result);
};

}
}
}

#endif /* NEWKEYSTRATEGY_HPP_ */
