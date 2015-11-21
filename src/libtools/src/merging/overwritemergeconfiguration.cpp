/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <merging/overwritemergeconfiguration.hpp>
#include <merging/metamergestrategy.hpp>
#include <merging/onesidestrategy.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{

void OverwriteMergeConfiguration::configureMerger(ThreeWayMerge& merger)
{
	MetaMergeStrategy *metaMergeStrategy = new MetaMergeStrategy(merger);
	allocatedStrategies.push_back(metaMergeStrategy);
	merger.addConflictStrategy(metaMergeStrategy);

	OneSideStrategy *oneSideStrategy = new OneSideStrategy(winningSide);
	allocatedStrategies.push_back(oneSideStrategy);
	merger.addConflictStrategy(oneSideStrategy);
}

}
}
}
