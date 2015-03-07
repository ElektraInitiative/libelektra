#include <merging/onesidemergeconfiguration.hpp>
#include <merging/onesidestrategy.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{

void OneSideMergeConfiguration::configureMerger(ThreeWayMerge& merger)
{
	AutoMergeConfiguration::configureMerger(merger);
	OneSideStrategy *oneSideStrategy = new OneSideStrategy(winningSide);
	allocatedStrategies.push_back(oneSideStrategy);
	merger.addConflictStrategy(oneSideStrategy);
}

}
}
}
