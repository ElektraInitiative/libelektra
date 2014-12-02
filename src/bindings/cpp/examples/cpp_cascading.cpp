#include <kdb.hpp>
#include <keysetio.hpp>
#include <iostream>

int main()
{
	using namespace kdb;
	KDB kdb;
	KeySet conf;
	kdb.get(conf, "/");
	std::cout << conf;
	kdb.set(conf, "/");
}
