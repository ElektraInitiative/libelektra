/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./lift_nested.hpp"

#include <iostream>

int main ()
{
	using namespace kdb;

	KDB kdb;
	KeySet ks;
	kdb.get (ks, "/test/lift");
	kdb.get (ks, "/test/material_lift");
	kdb.get (ks, "/test/heavy_material_lift");
	kdb.get (ks, "/test/person_lift");

	Parameters par (ks);

	std::cout << std::boolalpha;
	std::cout << "delay: " << par.test ().lift ().emergency ().getDelay () << std::endl;
	std::cout << "stops: " << par.test ().lift ().emergency ().action ().getStops () << std::endl;
	// std::cout << "algorithm: " << par.getTestLiftAlgorithm() << std::endl;
	kdb::test::Lift const & lift = par.test ().lift ();
	std::cout << "height #3: " << lift.floor ().n3 ().getHeight () << std::endl;
	std::cout << "limit: " << par.test ().lift ().getLimit () << std::endl;

	bool write = lift.getWrite ();
	par.test ().lift ().setWrite (false);

	// write back to user:/test/lift, see comments in lift.c
	if (write)
	{
		kdb.set (ks, "user:/test/lift");
	}

	return 0;
}
