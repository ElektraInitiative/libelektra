/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <keyset.hpp>

#include <iostream>

int main ()
{
	kdb::KeySet ks (2, *kdb::Key ("user:/1", KEY_END), *kdb::Key ("user:/2", KEY_END), KS_END);

	for (ssize_t it = 0; it < ks.size (); ++it)
	{
		std::cout << ks.at (it).getName () << std::endl;
	}
	return 0;
}
