/**
 * @file
 *
 * @brief Applies a MergeConflictStrategy on the metakeys
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef METAMERGESTRATEGY_HPP_
#define METAMERGESTRATEGY_HPP_

#include <merging/mergeconflictstrategy.hpp>
#include <merging/threewaymerge.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{
// The MetaMergeStrategy differs from other MergeConflictStrategies because
// it does not resolve conflicts by itself. Instead it uses the supplied ThreeWayMerger
// instance and applies it to the MetaKeys of conflicting Keys.
// Only if both conflict operations are META (i.e. if both sides modified only the MetaKeys of a key) and
// the supplied ThreeWayMerger is able to successfully merge the metakeys, the
// MetaMergeStrategy will mark the conflict as resolved.
// If the supplied merger is not able to resolve all conflicts
// in the MetaKeys this strategy won't resolve even a META <--> META conflict.
// If the conflict operations are anything else than META the MetaMergeStrategy will also
// not resolve the conflict, although the MetaKeys might be merged successul. This allows
// strategies later in the chain to resolve the value conflict of the conflicting key.
class MetaMergeStrategy : public MergeConflictStrategy
{

public:
	ThreeWayMerge & innerMerger;

	explicit MetaMergeStrategy (ThreeWayMerge & _innerStrategy) : innerMerger (_innerStrategy)
	{
	}

	virtual void resolveConflict (const MergeTask & task, Key & conflictKey, MergeResult & result) override;

private:
	KeySet getMetaKeys (Key & key);
};
} // namespace merging
} // namespace tools
} // namespace kdb

#endif /* METAMERGESTRATEGY_HPP_ */
