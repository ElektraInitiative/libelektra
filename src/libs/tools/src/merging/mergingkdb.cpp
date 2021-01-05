/**
 * @file
 *
 * @brief Implementation of MergeResult
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <merging/mergingkdb.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{

MergingKDB::MergingKDB ()
{
}

MergingKDB::MergingKDB (KDB & kdb) : KDB (kdb)
{
}

MergingKDB::~MergingKDB () throw ()
{
}

int MergingKDB::get (KeySet & returned, std::string const & keyname)
{
	int ret = KDB::get (returned, keyname);
	base = returned.dup ();
	return ret;
}

int MergingKDB::get (KeySet & returned, Key & parentKey)
{
	int ret = KDB::get (returned, parentKey);
	base = returned.dup ();
	return ret;
}

int MergingKDB::synchronize (KeySet & returned, std::string const & keyname, ThreeWayMerge & merger)
{
	Key parentKey (keyname.c_str (), KEY_END);
	return synchronize (returned, parentKey, merger);
}

int MergingKDB::synchronize (KeySet & returned, Key & parentKey, ThreeWayMerge & merger)
{
	try
	{
		// write our config
		int ret = KDB::set (returned, parentKey);

		// update our config (if no conflict)
		KDB::get (returned, parentKey);

		return ret;
	}
	catch (KDBException const &)
	{
		// a conflict occurred, see if we can solve it with the merger

		// refresh the key database
		KeySet theirs = returned.dup ();
		KDB::get (theirs, parentKey);

		// try to merge
		MergeResult result = merger.mergeKeySet (MergeTask (BaseMergeKeys (base, parentKey), OurMergeKeys (returned, parentKey),
								    TheirMergeKeys (theirs, parentKey), parentKey));

		if (!result.hasConflicts ())
		{
			// hurray, we solved the issue
			KeySet resultKeys = result.getMergedKeys ();
			int ret = KDB::set (resultKeys, parentKey);
			base = resultKeys;
			return ret;
		}
		else
		{
			// nothing we can do anymore
			KeySet conflictSet = result.getConflictSet ();
			throw MergingKDBException (parentKey, conflictSet);
		}
	}
}
} // namespace merging
} // namespace tools
} // namespace kdb
