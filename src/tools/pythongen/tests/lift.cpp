/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./lift.hpp"

#include <iostream>

int main (int argc, char ** argv)
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
	std::cout << "delay: " << par.getTestLiftEmergencyDelay () << std::endl;
	std::cout << "stops: " << par.getTestLiftEmergencyActionStops () << std::endl;
	// std::cout << "algorithm: " << par.getTestLiftAlgorithm() << std::endl;
	std::cout << "height #3: " << par.getTestLiftFloor3Height () << std::endl;
	std::cout << "limit: " << par.getTestLiftLimit () << std::endl;

	bool write = par.getTestLiftWrite ();
	par.setTestLiftWrite (false);

	// write back to user:/test/lift, see comments in lift.c
	if (write)
	{
		kdb.set (ks, "user:/test/lift");
	}

	return 0;
}
