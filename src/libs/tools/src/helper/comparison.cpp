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


	ckdb::KeySet * metaKeys1 = ckdb::keyMeta (k1.getKey ());
	ckdb::KeySet * metaKeys2 = ckdb::keyMeta (k2.getKey ());

	if (!metaKeys1 && !metaKeys2) return true;
	if ((!metaKeys1 && metaKeys2) || (metaKeys1 && !metaKeys2)) return false;
	if (ckdb::ksGetSize (metaKeys1) != ckdb::ksGetSize (metaKeys2)) return false;
	if (ckdb::ksGetSize (metaKeys1) == 0 && ckdb::ksGetSize (metaKeys2) == 0) return true;

	for (ssize_t it = 0; it < ckdb::ksGetSize (metaKeys1); ++it)
	{
		Key currentMeta (ckdb::ksAtCursor (metaKeys1, it));
		string metaName = currentMeta.getName ();
		if (!k2.hasMeta (metaName)) return false;
		if (currentMeta.getString () != k2.getMeta<string> (metaName)) return false;
	}

	for (ssize_t it = 0; it < ckdb::ksGetSize (metaKeys2); ++it)
	{
		Key currentMeta (ckdb::ksAtCursor (metaKeys2, it));
		string metaName = currentMeta.getName ();
		if (!k1.hasMeta (metaName)) return false;
		if (currentMeta.getString () != k1.getMeta<string> (metaName)) return false;
	}

	return true;
}
} // namespace helper
} // namespace tools
} // namespace kdb
