#include <kdb.hpp>

#include <iostream>

int main()
{
	KeySet config;
	KDB kdb;
	kdb.get(config, "/sw/MyApp");

	Key k = config.lookup("/sw/MyApp/mykey");
	if (k)
	{
		std::cout << k << std::endl;
	}
	else
	{
		std::cerr << "No key found" << std::endl;
		return 1;
	}
}
