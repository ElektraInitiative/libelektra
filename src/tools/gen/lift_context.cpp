#include "lift_context.hpp"
#include <kdb.hpp>

#include <iostream>

int main()
{
	using namespace kdb;

	KDB kdb;
	KeySet ks;
	Context c;
	kdb.get(ks, "/test/lift");
	kdb.get(ks, "/test/material_lift");
	kdb.get(ks, "/test/heavy_material_lift");
	kdb.get(ks, "/test/person_lift");

	Parameters par(ks,c);

	std::cout << std::boolalpha;
	std::cout << "delay: " << par.test.lift.emergency.delay << std::endl;
	std::cout << "stops: " << par.test.lift.emergency.action.stops << std::endl;
	kdb::test::Lift const & lift = par.test.lift;
	std::cout << "height #3: " << lift.floor.n3.height << std::endl;
	std::cout << "limit: " << par.test.lift.limit << std::endl;

	bool write = lift.write;
	par.test.lift.write = false;

	// write back to user/test/lift, see comments in lift.c
	if(write)
	{
		kdb.set(ks, "user/test/lift");
	}

	return 0;
}
