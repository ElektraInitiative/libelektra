/**
 * @file
 *
 * @brief A strategy which always takes the value from one side
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef NEWKEYSTRATEGY_HPP_
#define NEWKEYSTRATEGY_HPP_

#include <merging/mergeconflictstrategy.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{
// This strategy is basically a subset of the AutoMergeStrategy. It resolves
// only conflicts where one side added key, while the other side did nothing.
class NewKeyStrategy : public MergeConflictStrategy
{
public:
	NewKeyStrategy ()
	{
	}

	virtual void resolveConflict (const MergeTask & task, Key & conflictKey, MergeResult & result) override;
};
} // namespace merging
} // namespace tools
} // namespace kdb

#endif /* NEWKEYSTRATEGY_HPP_ */
