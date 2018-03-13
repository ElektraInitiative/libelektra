/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <merging/onesidemergeconfiguration.hpp>
#include <merging/onesidestrategy.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{

void OneSideMergeConfiguration::configureMerger (ThreeWayMerge & merger)
{
	AutoMergeConfiguration::configureMerger (merger);
	auto oneSideStrategy = new OneSideStrategy (winningSide);
	allocatedStrategies.push_back (oneSideStrategy);
	merger.addConflictStrategy (oneSideStrategy);
}
} // namespace merging
} // namespace tools
} // namespace kdb
