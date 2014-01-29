#include "lift.hpp"

#include <iostream>

int main()
{
	using namespace kdb;

	KDB kdb;
	KeySet ks;
	Key parentKey("user/sw/app/lift", KEY_END);
	kdb.get(ks, parentKey);

	Parameters par(ks);

	std::cout << par.getSwLiftLimit() << std::endl;

	par.setSwLiftLimit(42);

	std::cout << par.getSwLiftLimit() << std::endl;
	// kdb.set(ks, parentKey);

	return 0;
}
