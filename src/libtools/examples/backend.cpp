#include <backend.hpp>
#include <backends.hpp>

#include <iostream>

int main()
{
	using namespace kdb;
	using namespace kdb::tools;
	Backend b;
	b.setMountpoint(Key("/", KDB_O_CASCADING_NAME, KEY_END), KeySet(0, KS_END));
	b.addPlugin("resolver");
	b.addPlugin("dump");
	b.validated();

	Key rootKey(Backends::mountpointsPath, KEY_END);
	KeySet mountConfig;
	b.serialise(rootKey, mountConfig);

	mountConfig.rewind();
	while(mountConfig.next())
	{
		std::cout << mountConfig.current().getName()
			  << " = " << mountConfig.current().getString()
			  << std::endl;
	}
}
