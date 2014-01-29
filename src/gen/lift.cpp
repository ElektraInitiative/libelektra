#include "lift.hpp"

#include <iostream>

int main(int argc, char**argv)
{
	using namespace kdb;

	KDB kdb;
	KeySet ks;
	Key parentKey("system/sw/app/lift", KEY_END);
	kdb.get(ks, parentKey);
	parentKey.setName("user/sw/app/lift");
	kdb.get(ks, parentKey);

	Parameters par(ks);

	std::cout << par.getSwLiftLimit() << std::endl;

	par.setSwLiftLimit(42);

	std::cout << par.getSwLiftLimit() << std::endl;
	// kdb.set(ks, parentKey); // write back to user/

	return 0;
}
