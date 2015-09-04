namespace ckdb {
extern "C" {
extern ckdb::KeySet *elektraDocu;
}
}

int main(int argc, char **argv)
{
	using namespace ckdb;
	::testing::InitGoogleTest(&argc, argv);
	int ret = RUN_ALL_TESTS();
	elektraClose(); // valgrind does not detect cleanup outside main, so lets do it here
	ksDel(elektraDocu); // make everything clean
	return ret;
}
