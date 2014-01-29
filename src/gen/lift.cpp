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

	std::cout << par.getUserSwAppLiftLimit() << std::endl;

	par.setUserSwAppLiftLimit(42);

	std::cout << par.getUserSwAppLiftLimit() << std::endl;
	// kdb.set(ks, parentKey);

	return 0;
}
