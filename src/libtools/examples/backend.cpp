#include <backend.hpp>
#include <backends.hpp>

#include <iostream>

int main()
{
	using namespace kdb;
	using namespace kdb::tools;
	Backend b("my_backend", "/");
	b.addPlugin("resolver");
	b.addPlugin("dump");

	Key rootKey(Backends::mountpointsPath, KEY_END);
	KeySet mountConfig;
	b.serialize(rootKey, mountConfig);

	mountConfig.rewind();
	while(mountConfig.next())
	{
		std::cout << mountConfig.current().getName()
			  << " = " << mountConfig.current().getString()
			  << std::endl;
	}
}
