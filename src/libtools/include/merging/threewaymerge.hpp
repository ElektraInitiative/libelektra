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

#include <kdb.hpp>
#include <merging/mergeresult.hpp>
#include <merging/mergetask.hpp>
#include <string>

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

// TODO: move to own class
private:
	static Key rebaseKey(const Key& key, const Key& oldParent,
			const Key& newParent);
	static std::string rebasePath(const Key& key, const Key& oldParent,
			const Key& newParent);
	static bool keyDataEqual(const Key&, const Key&);
	static bool keyMetaEqual(Key&, Key&);
	static void automaticMerge(const MergeTask& task, MergeResult& mergeResult,
			bool reverseConflictMeta);
};

}
}
}

#endif /* THREEWAYMERGE_HPP_ */
