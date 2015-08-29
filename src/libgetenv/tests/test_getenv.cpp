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
	EXPECT_EQ(getenv("does-exist"), std::string("hello"));
}

TEST(GetEnv, OpenClose)
{
	using namespace ckdb;
	KeySet *oldElektraConfig = elektraConfig;
	elektraOpen(0, 0);
	EXPECT_NE(elektraConfig, oldElektraConfig);
	EXPECT_EQ(getenv("du4Maiwi/does-not-exist"), static_cast<char*>(0));
	ksAppendKey(elektraConfig,
			keyNew("user/sw/app/lift/does-exist",
				KEY_VALUE, "hello", KEY_END));

	Key *c;
	ksRewind(elektraConfig);
	while ((c = ksNext(elektraConfig)))
	{
		printf ("%s - %s\n", keyName(c), keyString(c));
	}

	EXPECT_EQ(getenv("does-exist"), std::string("hello"));
	EXPECT_EQ(getenv("du4Maiwi/does-not-exist"), static_cast<char*>(0));
	elektraClose();
}

TEST(GetEnv, ArgvParam)
{
	const char *cargv[] = {"name", "--elektra-proc:does-exist=hello", 0};
	char **argv = const_cast<char **>(cargv);
	int argc = 2;
	using namespace ckdb;
	elektraOpen(&argc, argv);
	EXPECT_EQ(argc, 1) << "elektra proc not consumed";

	Key *c;
	ksRewind(elektraConfig);
	while ((c = ksNext(elektraConfig)))
	{
		printf ("%s - %s\n", keyName(c), keyString(c));
	}

	ASSERT_NE(getenv("does-exist"), static_cast<char*>(0));
	EXPECT_EQ(getenv("does-exist"), std::string("hello"));
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
