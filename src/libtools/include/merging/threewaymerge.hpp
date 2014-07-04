/**
 * \file
 *
 * \brief Implements a way to build and deal with a backend
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef THREEWAYMERGE_HPP_
#define THREEWAYMERGE_HPP_

#include <string>
#include <kdb.hpp>
#include <merging/mergeresult.hpp>
#include <merging/mergetask.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{

class ThreeWayMerge
{

public:

	ThreeWayMerge()
	{
	}
	;
	~ThreeWayMerge()
	{
	}
	;

	static MergeResult mergeKeySet(const KeySet& base, const KeySet& ours,
			const KeySet& theirs, Key& mergeRoot);
	static MergeResult mergeKeySet(const MergeTask& task);

private:
	static void automaticMerge(const MergeTask& task, MergeResult& mergeResult,
			bool reverseConflictMeta);
};

}
}
}

#endif /* THREEWAYMERGE_HPP_ */
