/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <merging/importmergeconfiguration.hpp>
#include <merging/metamergestrategy.hpp>
#include <merging/newkeystrategy.hpp>
#include <merging/onesidevaluestrategy.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{

void ImportMergeConfiguration::configureMerger (ThreeWayMerge & merger)
{
	auto metaMergeStrategy = new MetaMergeStrategy (merger);
	allocatedStrategies.push_back (metaMergeStrategy);
	merger.addConflictStrategy (metaMergeStrategy);

	auto newKeyStrategy = new NewKeyStrategy ();
	allocatedStrategies.push_back (newKeyStrategy);
	merger.addConflictStrategy (newKeyStrategy);

	auto oneSideValueStrategy = new OneSideValueStrategy (THEIRS);
	allocatedStrategies.push_back (oneSideValueStrategy);
	merger.addConflictStrategy (oneSideValueStrategy);
}
} // namespace merging
} // namespace tools
} // namespace kdb
