/**
 * \file
 *
 * \brief A configuration for a simple automerge and guaranteed conflict resolution by one side
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef IMPORTMERGECONFIGURATION_HPP_
#define IMPORTMERGECONFIGURATION_HPP_

#include <merging/automergeconfiguration.hpp>

using namespace std;

namespace kdb
{

namespace tools
{

namespace merging
{

class ImportMergeConfiguration : public AutoMergeConfiguration
{
public:
	virtual void configureMerger(ThreeWayMerge& merger);
};

}
}
}

#endif /* IMPORTMERGECONFIGURATION_HPP_ */

