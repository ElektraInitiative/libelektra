/**
 * @file
 *
 * @brief Comparison helper functions
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
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


	KeySet metaKeys = ckdb::keyMeta (k1.getKey ());
	for (const Key & currentMeta : metaKeys)
	{
		string metaName = currentMeta.getName ();
		if (!k2.hasMeta (metaName)) return false;
		if (currentMeta.getString () != k2.getMeta<string> (metaName)) return false;
	}

	metaKeys = ckdb::keyMeta (k2.getKey ());
	for (const Key & currentMeta : metaKeys)
	{
		string metaName = currentMeta.getName ();
		if (!k1.hasMeta (metaName)) return false;
		if (currentMeta.getString () != k1.getMeta<string> (metaName)) return false;
	}

	return true;
}
} // namespace helper
} // namespace tools
} // namespace kdb
