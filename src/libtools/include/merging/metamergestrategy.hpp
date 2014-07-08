/**
 * \file
 *
 * \brief Applies a MergeConflictStrategy on the meta keys
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef METAMERGESTRATEGY_HPP_
#define METAMERGESTRATEGY_HPP_

#include <merging/threewaymerge.hpp>
#include <merging/mergeconflictstrategy.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{

class MetaMergeStrategy : public MergeConflictStrategy
{

public:
	ThreeWayMerge& innerMerger;

	MetaMergeStrategy(ThreeWayMerge& _innerStrategy) :
			innerMerger (_innerStrategy)
	{
	}

	virtual void resolveConflict(const MergeTask& task, Key& conflictKey, MergeResult& result);

private:
	KeySet getMetaKeys (Key& key);
};

}
}
}

#endif /* METAMERGESTRATEGY_HPP_ */



