/**
 * @file
 *
 * @brief Comparison helper functions
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <helper/comparison.hpp>

using namespace std;

namespace kdb
{

namespace tools
{

namespace helper
{

bool keyDataEqual (const Key & k1, const Key & k2)
{
	if (!k1 || !k2) return false;

	if (k1.isBinary () != k2.isBinary ()) return false;

	if (k1.isBinary () && k2.isBinary ())
	{
		return k1.getBinary () == k2.getBinary ();
	}
	else
	{
		return k1.getString () == k2.getString ();
	}

	return true;
}

bool keyMetaEqual (Key & k1, Key & k2)
{
	if (!k1 || !k2) return false;

	k1.rewindMeta ();
	Key currentMeta;
	while ((currentMeta = k1.nextMeta ()))
	{
		string metaName = currentMeta.getName ();
		if (!k2.hasMeta (metaName)) return false;
		if (currentMeta.getString () != k2.getMeta<string> (metaName)) return false;
	}


	k2.rewindMeta ();
	while ((currentMeta = k2.nextMeta ()))
	{
		string metaName = currentMeta.getName ();
		if (!k1.hasMeta (metaName)) return false;
		if (currentMeta.getString () != k1.getMeta<string> (metaName)) return false;
	}


	return true;
}
}
}
}
