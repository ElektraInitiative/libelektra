/**
 * @file
 *
 * @brief Implements a way to build and deal with a backend
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef THREEWAYMERGE_HPP_
#define THREEWAYMERGE_HPP_

#include <kdb.hpp>
#include <memory>
#include <merging/mergeconflictstrategy.hpp>
#include <merging/mergeresult.hpp>
#include <merging/mergetask.hpp>
#include <string>
#include <vector>

namespace kdb
{

namespace tools
{

namespace merging
{

class ThreeWayMerge
{

public:
	/**
	 * Performs a threeway merge according to the supplied MergeTask. All merged keys will
	 * be below the given mergeParent in the MergeTask. Found conflicts will be
	 * reported in the MergeResult. Conflicts are below the mergeParent as well and
	 * are not part of the mergedKeys.
	 *
	 * @see MergeTask
	 * @see MergeResult
	 *
	 * @param task a MergeTask describing the intended merge oparation
	 * @return a MergeResult that contains the merged keys as well as all found conflicts.
	 *
	 **/
	MergeResult mergeKeySet (const KeySet & base, const KeySet & ours, const KeySet & theirs, const Key & mergeRoot);

	/**
	 * Performs a threeway merge based on the supplied KeySets. The result is the same
	 * as for ThreeWayMerge::mergeKeySet(const MergeTask&). The first key (i.e. the shortest)
	 * in each of the supplied KeySets is considered to be the corresponding parentKey.
	 * This means that the parent key of each KeySet MUST be part of the KeySet.
	 *
	 * @see ThreeWayMerge::mergeKeySet(const MergeTask&)
	 * @return a MergeResult that contains the merged keys as well as all found conflicts.
	 */
	MergeResult mergeKeySet (const MergeTask & task);

	/**
	 * Adds a conflict resolution strategy to the merger. The merger tries to resolve conflicts
	 * with the strategies registered in the order they were added. The caller is responsible for
	 * freeing the strategy, but it must not be freed as long as the merger is used.
	 *
	 * @param a conflict reoslution strategy to be consulted in case of conflicts
	 */
	void addConflictStrategy (MergeConflictStrategy * strategy)
	{
		strategies.push_back (strategy);
	}

private:
	std::vector<MergeConflictStrategy *> strategies;
	void detectConflicts (const MergeTask & task, MergeResult & mergeResult, bool reverseConflictMeta);
};
} // namespace merging
} // namespace tools
} // namespace kdb

#endif /* THREEWAYMERGE_HPP_ */
