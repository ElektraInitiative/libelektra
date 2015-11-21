/**
 * @file
 *
 * @brief A configuration for a simple automerge and guaranteed conflict resolution by one side
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
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
	OverwriteMergeConfiguration(ConflictResolutionSide _winningSide) : winningSide(_winningSide) {}
	virtual void configureMerger(ThreeWayMerge& merger);
};

}
}
}

#endif /* OVERWRITEMERGECONFIGURATION_HPP_ */

