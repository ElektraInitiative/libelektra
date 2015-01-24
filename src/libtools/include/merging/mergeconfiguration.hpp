/**
 * \file
 *
 * \brief Base class for defining preconfigured merge configurations
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
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
