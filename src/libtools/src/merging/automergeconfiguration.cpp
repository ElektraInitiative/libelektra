/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <merging/automergeconfiguration.hpp>
#include <merging/metamergestrategy.hpp>
#include <merging/automergestrategy.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{

void AutoMergeConfiguration::configureMerger(ThreeWayMerge& merger)
{
	MetaMergeStrategy *metaMergeStrategy = new MetaMergeStrategy(merger);
	allocatedStrategies.push_back(metaMergeStrategy);
	merger.addConflictStrategy(metaMergeStrategy);

	AutoMergeStrategy *autoMergeStrategy = new AutoMergeStrategy();
	allocatedStrategies.push_back(autoMergeStrategy);
	merger.addConflictStrategy(autoMergeStrategy);
}

}
}
}
