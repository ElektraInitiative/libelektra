/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ONESIDEVALUESTRATEGY_HPP_
#define ONESIDEVALUESTRATEGY_HPP_

#include <merging/mergeconflictstrategy.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{
// This strategy is a subset of the OneSideStrategy. It also uses
// the key of the winning side in case of a conflict. However, different
// than the OneSideStrategy it only resolves conflicts where no new keys are
// introduced or old ones deleted.
class OneSideValueStrategy : public MergeConflictStrategy
{

public:
	ConflictResolutionSide winningSide;

	explicit OneSideValueStrategy (ConflictResolutionSide _winningSide) : winningSide (_winningSide)
	{
	}

	virtual void resolveConflict (const MergeTask & task, Key & conflictKey, MergeResult & result) override;
};
} // namespace merging
} // namespace tools
} // namespace kdb

#endif /* ONESIDEVALUESTRATEGY_HPP_ */
