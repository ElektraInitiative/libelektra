/**
 * @file
 *
 * @brief A configuration for a simple automerge and guaranteed conflict resolution by one side
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
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
// This strategy is primarily used for importing. It keeps all
// keeps all keys present on both sides, but overwrites
// the value of our side with theirs whenever keys conflict.
class ImportMergeConfiguration : public AutoMergeConfiguration
{
public:
	virtual void configureMerger(ThreeWayMerge& merger);
};

}
}
}

#endif /* IMPORTMERGECONFIGURATION_HPP_ */

