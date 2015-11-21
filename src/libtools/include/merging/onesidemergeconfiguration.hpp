/**
 * @file
 *
 * @brief A configuration for a simple automerge and guaranteed conflict resolution by one side
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef ONESIDEMERGECONFIGURATION_HPP_
#define ONESIDEMERGECONFIGURATION_HPP_

#include <merging/automergeconfiguration.hpp>

using namespace std;

namespace kdb
{

namespace tools
{

namespace merging
{

// This configuration is like the auto merge configuration except
// that it resolves any unresolved conflicts by using one side only
class OneSideMergeConfiguration : public AutoMergeConfiguration
{

private:
	ConflictResolutionSide winningSide;

public:
	OneSideMergeConfiguration(ConflictResolutionSide _winningSide) : winningSide(_winningSide) {}
	virtual void configureMerger(ThreeWayMerge& merger);
};

}
}
}

#endif /* ONESIDEMERGECONFIGURATION_HPP_ */

