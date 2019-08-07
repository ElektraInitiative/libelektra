/**
 * @file
 *
 * @brief Models a merge task
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef MERGETASK_HPP_
#define MERGETASK_HPP_

#include <elektra/kdb.h>


namespace kdb
{

namespace tools
{

namespace merging
{

class MergeKeys
{
public:
	const KeySet keys;
	const Key parent;

protected:
	MergeKeys (const KeySet & _keys, const Key & _parentKey) : keys (_keys), parent (_parentKey)
	{
	}
};

class BaseMergeKeys : public MergeKeys
{
public:
	BaseMergeKeys (const KeySet & _keys, const Key & _parentKey) : MergeKeys (_keys, _parentKey)
	{
	}
};

class TheirMergeKeys : public MergeKeys
{
public:
	TheirMergeKeys (const KeySet & _keys, const Key & _parentKey) : MergeKeys (_keys, _parentKey)
	{
	}
};

class OurMergeKeys : public MergeKeys
{
public:
	OurMergeKeys (const KeySet & _keys, const Key & _parentKey) : MergeKeys (_keys, _parentKey)
	{
	}
};

class MergeTask
{
public:
	KeySet base;
	KeySet ours;
	KeySet theirs;
	Key baseParent;
	Key ourParent;
	Key theirParent;
	Key mergeRoot;

	/**
	 * @param _base the KeySet containing the base keys and the base parentKey
	 * @param _ours the KeySet containing our keys and our parentKey
	 * @param _theirs the KeySet containing their keys and their parentKey
	 * @param _mergeRoot the parentKey for the merged keys
	 */
	MergeTask (const BaseMergeKeys & _base, const OurMergeKeys & _ours, const TheirMergeKeys & _theirs, const Key & _mergeRoot)
	: base (_base.keys), ours (_ours.keys), theirs (_theirs.keys), baseParent (_base.parent), ourParent (_ours.parent),
	  theirParent (_theirs.parent), mergeRoot (_mergeRoot)
	{
	}

	~MergeTask ()
	{
	}

	MergeTask reverse () const
	{
		return MergeTask (BaseMergeKeys (base, baseParent), OurMergeKeys (theirs, theirParent), TheirMergeKeys (ours, ourParent),
				  mergeRoot);
	}
};
} // namespace merging
} // namespace tools
} // namespace kdb

#endif /* MERGETASK_HPP_ */
