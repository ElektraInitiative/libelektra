/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <keyset.hpp>

#include <iostream>

int main ()
{
	kdb::KeySet ks (2, *kdb::Key ("user/1", KEY_END), *kdb::Key ("user/2", KEY_END), KS_END);

	ks.rewind ();
	while (ks.next ())
	{
		std::cout << ks.current ().getName () << std::endl;
	}
	return 0;
}
