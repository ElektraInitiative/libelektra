/**
 * \file
 *
 * \brief A configuration for a simple automerge and guaranteed conflict resolution by one side
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

