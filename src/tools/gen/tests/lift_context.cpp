#include "lift_context_dynamic.hpp"
#include <kdb.hpp>

#include <iostream>

int main()
{
	using namespace kdb;

	KDB kdb;
	KeySet ks;
	Coordinator c;
	ThreadContext tc(c);
	kdb.get(ks, "/test/lift");
	kdb.get(ks, "/test/material_lift");
	kdb.get(ks, "/test/heavy_material_lift");
	kdb.get(ks, "/test/person_lift");

	Environment env(ks,tc);
	std::cout << std::boolalpha;
	std::cout << "delay: " << env.test.lift.emergency.delay << std::endl;
	std::cout << "stops: " << env.test.lift.emergency.action.stops << std::endl;
	kdb::test::Lift const & lift = env.test.lift;
	std::cout << "height #3: " << lift.floor.n3.height << std::endl;
	std::cout << "limit: " << env.test.lift.limit << std::endl;

	bool write = lift.write;
	env.test.lift.write = false;

	// write back to user/test/lift, see comments in lift.c
	if(write)
	{
		kdb.set(ks, "user/test/lift");
	}

	return 0;
}
