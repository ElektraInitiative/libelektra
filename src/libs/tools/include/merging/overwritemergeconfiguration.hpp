/**
 * @file
 *
 * @brief A configuration for a simple automerge and guaranteed conflict resolution by one side
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef OVERWRITEMERGECONFIGURATION_HPP_
#define OVERWRITEMERGECONFIGURATION_HPP_

#include <merging/automergeconfiguration.hpp>

using namespace std;

namespace kdb
{

namespace tools
{

namespace merging
{

// This configuration is primarily used for importing and simply
// resolves all conflicts by taking one side. This can be used
// during import to overwrite existing keys
class OverwriteMergeConfiguration : public AutoMergeConfiguration
{

private:
	ConflictResolutionSide winningSide;

public:
	explicit OverwriteMergeConfiguration (ConflictResolutionSide _winningSide) : winningSide (_winningSide)
	{
	}
	virtual void configureMerger (ThreeWayMerge & merger) override;
};
} // namespace merging
} // namespace tools
} // namespace kdb

#endif /* OVERWRITEMERGECONFIGURATION_HPP_ */
