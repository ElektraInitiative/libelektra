/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <merging/metamergestrategy.hpp>
#include <merging/onesidestrategy.hpp>
#include <merging/overwritemergeconfiguration.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{

void OverwriteMergeConfiguration::configureMerger (ThreeWayMerge & merger)
{
	auto metaMergeStrategy = new MetaMergeStrategy (merger);
	allocatedStrategies.push_back (metaMergeStrategy);
	merger.addConflictStrategy (metaMergeStrategy);

	auto oneSideStrategy = new OneSideStrategy (winningSide);
	allocatedStrategies.push_back (oneSideStrategy);
	merger.addConflictStrategy (oneSideStrategy);
}
} // namespace merging
} // namespace tools
} // namespace kdb
