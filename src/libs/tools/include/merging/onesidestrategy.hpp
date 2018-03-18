/**
 * @file
 *
 * @brief A strategy which always takes the value from one side
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ONESIDESTRATEGY_HPP_
#define ONESIDESTRATEGY_HPP_

#include <merging/mergeconflictstrategy.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{
// This strategy is able to resolve every kind of conflict by always
// using the key of the winning side. Note that this also includes removing
// keys if the keys were deleted at the winning side.
class OneSideStrategy : public MergeConflictStrategy
{

public:
	ConflictResolutionSide winningSide;

	explicit OneSideStrategy (ConflictResolutionSide _winningSide) : winningSide (_winningSide)
	{
	}

	virtual void resolveConflict (const MergeTask & task, Key & conflictKey, MergeResult & result) override;
};
} // namespace merging
} // namespace tools
} // namespace kdb

#endif /* ONESIDESTRATEGY_HPP_ */
