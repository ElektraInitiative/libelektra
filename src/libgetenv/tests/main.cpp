#include <iostream>

namespace ckdb {
extern "C" {
extern ckdb::KeySet *elektraDocu;
extern std::ostream *elektraLog;
}
}

int main(int argc, char **argv)
{
	using namespace ckdb;
	::testing::InitGoogleTest(&argc, argv);
	int ret = RUN_ALL_TESTS();
	elektraClose(); // valgrind does not detect cleanup outside main, so lets do it here

	// make everything really proper clean:
	ksDel(elektraDocu);
	if (elektraLog && elektraLog != &std::cerr) delete elektraLog;
	return ret;
}
