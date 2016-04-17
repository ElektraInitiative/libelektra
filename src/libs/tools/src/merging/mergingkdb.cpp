/**
 * @file
 *
 * @brief Implementation of MergeResult
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <merging/mergingkdb.hpp>

namespace kdb
{

namespace tools
{

namespace merging
{

int MergingKDB::get (KeySet & returned, std::string const & keyname)
{
	int ret = innerKdb->get(returned, keyname);
	base = returned.dup();
	return ret;
}

int MergingKDB::get (KeySet & returned, Key & parentKey)
{
	int ret = innerKdb->get(returned, parentKey);
	base = returned.dup();
	return ret;
}

int MergingKDB::set (KeySet & returned, std::string const & keyname, ThreeWayMerge & merger)
{
	Key parentKey (keyname.c_str (), KEY_CASCADING_NAME, KEY_END);
	return set(returned, parentKey, merger);
}

int MergingKDB::set (KeySet & returned, Key & parentKey, ThreeWayMerge & merger)
{
	try
	{
		// write our config
		int ret = innerKdb->set (returned, parentKey);

		// update our config (if no conflict)
		innerKdb->get (returned, parentKey);

		return ret;
	}
	catch(KDBException const &)
	{
		// a conflict occurred, see if we can solve it with the merger

		// refresh the key database
		KeySet theirs = returned.dup ();
		innerKdb->get (theirs, parentKey);

		// try to merge
		MergeResult result = merger.mergeKeySet (MergeTask (
				BaseMergeKeys (base, parentKey),
				OurMergeKeys (returned, parentKey),
				TheirMergeKeys (theirs, parentKey), parentKey));

		if (!result.hasConflicts ())
		{
			// hurray, we solved the issue
			KeySet resultKeys = result.getMergedKeys ();
			int ret = innerKdb->set (resultKeys, parentKey);
			base = resultKeys;
			return ret;
		}
		else
		{
			// nothing we can do anymore
			KeySet conflictSet = result.getConflictSet ();
			throw MergingKDBException(parentKey, conflictSet);
		}
	}
}

}
}
}
