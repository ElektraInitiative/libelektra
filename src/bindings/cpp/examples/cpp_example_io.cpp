/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.hpp>

#include <kdbio.hpp>
#include <keyio.hpp>
#include <keysetio.hpp>

int main ()
{
	kdb::Key k ("user:/sw/MyApp", KEY_END);
	std::cout << "created a key: " << k << std::endl;

	kdb::KeySet ks;

	try
	{
		kdb::KDB kdb (k);
		kdb.get (ks, k);

		std::cout << "print out (full) keyset:" << std::endl;
		std::cout << ks;

		kdb.set (ks, k);
		kdb.close (k);
		printWarnings (std::cout, k, true, true);
	}
	catch (kdb::KDBException const & e)
	{
		std::cout << e.what ();
	}
}
