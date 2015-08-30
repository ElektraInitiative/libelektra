/**
 * \file
 *
 * \brief Tests for the getenv library
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <kdbgetenv.h>
#include <gtest/gtest.h>

TEST(GetEnv, NonExist)
{
	EXPECT_EQ(getenv("du4Maiwi/does-not-exist"), static_cast<char*>(0));
}

TEST(GetEnv, Exist)
{
	using namespace ckdb;
	ksAppendKey(elektraConfig,
			keyNew("user/sw/app/lift/does-exist",
				KEY_VALUE, "hello", KEY_END));
	ASSERT_NE(getenv("does-exist"), static_cast<char*>(0));
	EXPECT_EQ(getenv("does-exist"), std::string("hello"));
}

TEST(GetEnv, OpenClose)
{
	using namespace ckdb;
	// KeySet *oldElektraConfig = elektraConfig;
	elektraOpen(0, 0);
	// EXPECT_NE(elektraConfig, oldElektraConfig); // even its a new object, it might point to same address
	EXPECT_EQ(getenv("du4Maiwi/does-not-exist"), static_cast<char*>(0));
	ksAppendKey(elektraConfig,
			keyNew("user/sw/app/lift/does-exist",
				KEY_VALUE, "hello", KEY_END));

	ASSERT_NE(getenv("does-exist"), static_cast<char*>(0));
	EXPECT_EQ(getenv("does-exist"), std::string("hello"));
	EXPECT_EQ(getenv("du4Maiwi/does-not-exist"), static_cast<char*>(0));
	elektraClose();
}

void elektraPrintConfig()
{
	using namespace ckdb;
	Key *c;
	ksRewind(elektraConfig);
	while ((c = ksNext(elektraConfig)))
	{
		printf ("%s - %s\n", keyName(c), keyString(c));
	}
}

TEST(GetEnv, ArgvParam)
{
	const char *cargv[] = {"name", "--elektra:does-exist=hello", 0};
	char **argv = const_cast<char **>(cargv);
	int argc = 2;
	using namespace ckdb;
	elektraOpen(&argc, argv);
	EXPECT_EQ(argc, 1) << "elektra proc not consumed";
	EXPECT_EQ(argv[0], std::string("name"));
	EXPECT_EQ(argv[1], static_cast<char*>(0));

	ckdb::Key *k = ksLookupByName(elektraConfig, "proc/does-exist", 0);

	ASSERT_NE(k, static_cast<ckdb::Key*>(0));
	EXPECT_EQ(keyString(k), std::string("hello"));

	ASSERT_NE(getenv("does-exist"), static_cast<char*>(0));
	EXPECT_EQ(getenv("does-exist"), std::string("hello"));
	elektraClose();
}

TEST(GetEnv, ArgvParamUninvolved)
{
	const char *cargv[] = {"name",  "--uninvolved", "--not-used", "-L", "--elektra:does-exist=hello", "--uninvolved", "--not-used", "-L",  0};
	char **argv = const_cast<char **>(cargv);
	int argc = 8;
	using namespace ckdb;
	elektraOpen(&argc, argv);
	EXPECT_EQ(argc, 7) << "elektra proc not consumed";
	EXPECT_EQ(argv[0], std::string("name"));
	EXPECT_EQ(argv[1], std::string("--uninvolved"));
	EXPECT_EQ(argv[2], std::string("--not-used"));
	EXPECT_EQ(argv[3], std::string("-L"));
	EXPECT_EQ(argv[4], std::string("--uninvolved"));
	EXPECT_EQ(argv[5], std::string("--not-used"));
	EXPECT_EQ(argv[6], std::string("-L"));
	EXPECT_EQ(argv[7], static_cast<char*>(0));

	ckdb::Key *k = ksLookupByName(elektraConfig, "proc/does-exist", 0);

	ASSERT_NE(k, static_cast<ckdb::Key*>(0));
	EXPECT_EQ(keyString(k), std::string("hello"));

	ASSERT_NE(getenv("does-exist"), static_cast<char*>(0));
	EXPECT_EQ(getenv("does-exist"), std::string("hello"));
	elektraClose();
}

TEST(GetEnv, Name)
{
	using namespace ckdb;
	elektraOpen(0, 0);
	EXPECT_EQ(keyName(elektraParentKey), std::string("/sw/env/default"));
	elektraClose();
}

TEST(GetEnv, NameArgv0)
{
	using namespace ckdb;
	int argc = 1;
	const char *cargv[] = {"any-name", 0};
	char **argv = const_cast<char **>(cargv);

	elektraOpen(&argc, argv);
	EXPECT_EQ(keyName(elektraParentKey), std::string("/sw/env/any-name"));
	elektraClose();
}


TEST(GetEnv, NameExplicit)
{
	using namespace ckdb;
	int argc = 2;
	const char *cargv[] = {"any-name", "--elektra-name=other-name"};
	char **argv = const_cast<char **>(cargv);

	elektraOpen(&argc, argv);
	EXPECT_EQ(keyName(elektraParentKey), std::string("/sw/env/other-name"));
	elektraClose();
}

int main(int argc, char **argv)
{
	using namespace ckdb;
	::testing::InitGoogleTest(&argc, argv);
	int ret = RUN_ALL_TESTS();
	elektraClose(); // valgrind does not detect cleanup outside main, so lets do it here
	return ret;
}
