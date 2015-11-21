/**
 * @file
 *
 * @brief Base class for defining preconfigured merge configurations
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef MERGECONFIGURATION_HPP_
#define MERGECONFIGURATION_HPP_

#include <merging/threewaymerge.hpp>

using namespace std;

namespace kdb
{

namespace tools
{

namespace merging
{


// A merge configuration should configure a passed threeway merger with one or
// morge merge configurations. A class subclassing this class may add
// merge strategies to the allocatedStrategies vector and they will be
// freed on destruction
class MergeConfiguration
{
protected:
	vector<MergeConflictStrategy *> allocatedStrategies;

public:
	virtual ~MergeConfiguration()
	{
		for (vector<MergeConflictStrategy*>::iterator it = allocatedStrategies.begin(); it != allocatedStrategies.end (); ++it)
		{
			delete (*it);
		}
	};
	virtual void configureMerger(ThreeWayMerge& merger) = 0;
};


}
}
}


#endif /* MERGECONFIGURATION_HPP_ */
