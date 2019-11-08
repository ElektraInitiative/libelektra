/**
 * @file
 *
 * @brief example how easy it is to put keys in different data structures
 *
 * - * They might even have complete other properties than KeySet, e.g. in
 * this case:
 * - duplicates are allowed
 * - after sorting order is given by metadata "order"
 *
 * It is easy to create any other data structure, with any properties
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <keyset.hpp>

#include <algorithm>
#include <iostream>
#include <vector>

bool keyOrder (kdb::Key k1, kdb::Key k2)
{
	int o1 = k1.getMeta<int> ("order");
	int o2 = k2.getMeta<int> ("order");
	return o1 < o2;
}

int main ()
{
	using namespace kdb;

	std::vector<Key> vc;
	vc.push_back (Key ("user:/key3/1", KEY_META, "order", "2", KEY_END));
	vc.push_back (Key ("user:/begin", KEY_META, "order", "1", KEY_END));
	vc.push_back (Key ("user:/key3/4", KEY_META, "order", "3", KEY_END));
	vc.push_back (Key ("user:/key3/dup", KEY_META, "order", "4", KEY_END));
	vc.push_back (Key ("user:/key3/dup", KEY_END));
	vc.push_back (Key ("user:/unordered", KEY_END));
	vc.push_back (Key ("user:/end", KEY_META, "order", "5", KEY_END));

	std::sort (vc.begin (), vc.end (), keyOrder);

	KeySet ks (20, KS_END);
	std::cout << "Our Vector with special ordering:" << std::endl;
	for (auto k : vc)
	{

		std::cout << k.getName () << std::endl;
		ks.append (k);
	}
	// now we have a keyset (of course again with KeySet ordering and
	// duplicates removed.
	std::cout << "\nNow KeySet:" << std::endl;
	for (auto && ks_i : ks)
	{
		Key k (ks_i);
		std::cout << k.getName () << std::endl;
	}
}
