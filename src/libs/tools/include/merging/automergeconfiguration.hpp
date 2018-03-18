/**
 * @file
 *
 * @brief A configuration for a simple automerge
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef AUTOMERGECONFIGURATION_HPP_
#define AUTOMERGECONFIGURATION_HPP_

#include <merging/mergeconfiguration.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{

class AutoMergeConfiguration : public MergeConfiguration
{
public:
	virtual void configureMerger (ThreeWayMerge & merger) override;
};
} // namespace merging
} // namespace tools
} // namespace kdb

#endif /* AUTOMERGECONFIGURATION_HPP_ */
