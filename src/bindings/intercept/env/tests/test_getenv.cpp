/**
 * @file
 *
 * @brief Tests for the getenv library
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <gtest/gtest.h>
#include <kdbgetenv.h>

TEST (GetEnv, NonExist)
{
	EXPECT_EQ (getenv ("du4Maiwi/does-not-exist"), static_cast<char *> (nullptr));
}

TEST (GetEnv, ExistOverride)
{
	using namespace ckdb;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/override/does-exist", KEY_VALUE, "hello", KEY_END));
	ASSERT_NE (getenv ("does-exist"), static_cast<char *> (nullptr));
	EXPECT_EQ (getenv ("does-exist"), std::string ("hello"));
	elektraClose ();
}

TEST (GetEnv, ExistOverrideFallback)
{
	using namespace ckdb;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, keyNew ("user:/env/override/does-exist-fb", KEY_VALUE, "hello", KEY_END));
	ASSERT_NE (getenv ("does-exist-fb"), static_cast<char *> (nullptr));
	EXPECT_EQ (getenv ("does-exist-fb"), std::string ("hello"));
	elektraClose ();
}

TEST (GetEnv, ExistEnv)
{
	using namespace ckdb;
	elektraOpen (nullptr, nullptr);
	setenv ("does-exist", "hello", 1);
	ASSERT_NE (getenv ("does-exist"), static_cast<char *> (nullptr));
	EXPECT_EQ (getenv ("does-exist"), std::string ("hello"));
	elektraClose ();
}

TEST (GetEnv, ExistEnvFallback)
{
	using namespace ckdb;
	elektraOpen (nullptr, nullptr);
	setenv ("does-exist-fb", "hello", 1);
	ASSERT_NE (getenv ("does-exist-fb"), static_cast<char *> (nullptr));
	EXPECT_EQ (getenv ("does-exist-fb"), std::string ("hello"));
	elektraClose ();
}

TEST (GetEnv, ExistFallback)
{
	using namespace ckdb;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/fallback/does-exist", KEY_VALUE, "hello", KEY_END));
	ASSERT_NE (getenv ("does-exist"), static_cast<char *> (nullptr));
	EXPECT_EQ (getenv ("does-exist"), std::string ("hello"));
	elektraClose ();
}

TEST (GetEnv, ExistFallbackFallback)
{
	using namespace ckdb;
	elektraOpen (nullptr, nullptr);
	ksAppendKey (elektraConfig, keyNew ("user:/env/fallback/does-exist-fb", KEY_VALUE, "hello", KEY_END));
	ASSERT_NE (getenv ("does-exist-fb"), static_cast<char *> (nullptr));
	EXPECT_EQ (getenv ("does-exist-fb"), std::string ("hello"));
	elektraClose ();
}

TEST (GetEnv, OpenClose)
{
	using namespace ckdb;
	// KeySet *oldElektraConfig = elektraConfig;
	elektraOpen (nullptr, nullptr);
	// EXPECT_NE(elektraConfig, oldElektraConfig); // even its a new object, it might point to same address
	EXPECT_EQ (getenv ("du4Maiwi/does-not-exist"), static_cast<char *> (nullptr));
	ksAppendKey (elektraConfig, keyNew ("user:/elektra/intercept/getenv/override/does-exist", KEY_VALUE, "hello", KEY_END));

	ASSERT_NE (getenv ("does-exist"), static_cast<char *> (nullptr));
	EXPECT_EQ (getenv ("does-exist"), std::string ("hello"));
	EXPECT_EQ (getenv ("du4Maiwi/does-not-exist"), static_cast<char *> (nullptr));
	elektraClose ();
}

TEST (GetEnv, OpenCloseFallback)
{
	using namespace ckdb;
	// KeySet *oldElektraConfig = elektraConfig;
	elektraOpen (nullptr, nullptr);
	// EXPECT_NE(elektraConfig, oldElektraConfig); // even its a new object, it might point to same address
	EXPECT_EQ (getenv ("du4Maiwi/does-not-exist-fb"), static_cast<char *> (nullptr));
	ksAppendKey (elektraConfig, keyNew ("user:/env/override/does-exist-fb", KEY_VALUE, "hello", KEY_END));

	ASSERT_NE (getenv ("does-exist-fb"), static_cast<char *> (nullptr));
	EXPECT_EQ (getenv ("does-exist-fb"), std::string ("hello"));
	EXPECT_EQ (getenv ("du4Maiwi/does-not-exist-fb"), static_cast<char *> (nullptr));
	elektraClose ();
}

void elektraPrintConfig ()
{
	using namespace ckdb;

	for (elektraCursor it = 0; it < ksGetSize (elektraConfig); ++it)
	{
		const Key * c = ksAtCursor (elektraConfig, it);
		printf ("%s - %s\n", keyName (c), keyString (c));
	}
}

// FIXME [new_backend]: tests disabled

TEST (GetEnv, DISABLED_ArgvParam)
{
	const char * cargv[] = { "name", "--elektra:does-exist=hello", nullptr };
	char ** argv = const_cast<char **> (cargv);
	int argc = 2;
	using namespace ckdb;
	elektraOpen (&argc, argv);
	EXPECT_EQ (argc, 1) << "elektra proc not consumed";
	EXPECT_EQ (argv[0], std::string ("name"));
	EXPECT_EQ (argv[1], static_cast<char *> (nullptr));

	ckdb::Key * k = ksLookupByName (elektraConfig, "proc:/elektra/intercept/getenv/override/does-exist", 0);

	ASSERT_NE (k, static_cast<ckdb::Key *> (nullptr));
	EXPECT_EQ (keyString (k), std::string ("hello"));

	ASSERT_NE (getenv ("does-exist"), static_cast<char *> (nullptr));
	EXPECT_EQ (getenv ("does-exist"), std::string ("hello"));
	elektraClose ();
}

TEST (GetEnv, DISABLED_ArgvParamUninvolved)
{
	const char * cargv[] = { "name",	 "--uninvolved", "--not-used", "-L",   "--elektra:does-exist=hello",
				 "--uninvolved", "--not-used",	 "-L",	       nullptr };
	char ** argv = const_cast<char **> (cargv);
	int argc = 8;
	using namespace ckdb;
	elektraOpen (&argc, argv);
	EXPECT_EQ (argc, 7) << "elektra proc not consumed";
	EXPECT_EQ (argv[0], std::string ("name"));
	EXPECT_EQ (argv[1], std::string ("--uninvolved"));
	EXPECT_EQ (argv[2], std::string ("--not-used"));
	EXPECT_EQ (argv[3], std::string ("-L"));
	EXPECT_EQ (argv[4], std::string ("--uninvolved"));
	EXPECT_EQ (argv[5], std::string ("--not-used"));
	EXPECT_EQ (argv[6], std::string ("-L"));
	EXPECT_EQ (argv[7], static_cast<char *> (nullptr));

	ckdb::Key * k = ksLookupByName (elektraConfig, "proc:/elektra/intercept/getenv/override/does-exist", 0);

	ASSERT_NE (k, static_cast<ckdb::Key *> (nullptr));
	EXPECT_EQ (keyString (k), std::string ("hello"));

	ASSERT_NE (getenv ("does-exist"), static_cast<char *> (nullptr));
	EXPECT_EQ (getenv ("does-exist"), std::string ("hello"));
	elektraClose ();
}

TEST (GetEnv, DISABLED_ArgvParamVersion)
{
	const char * cargv[] = { "curl", "--elektra-version", nullptr };
	char ** argv = const_cast<char **> (cargv);
	int argc = 2;
	using namespace ckdb;
	elektraOpen (&argc, argv);
	EXPECT_EQ (argc, 1) << "elektra proc not consumed";
	EXPECT_EQ (argv[0], std::string ("curl"));
	EXPECT_EQ (argv[1], std::string ("--elektra-version"));
	EXPECT_EQ (argv[2], static_cast<char *> (nullptr));

	ckdb::Key * kdb_version = ksLookupByName (elektraConfig, "system:/elektra/version/constants/KDB_VERSION", 0);

	EXPECT_EQ (getenv ("version"), "Elektra getenv is active\n" + std::string ("KDB_VERSION: ") +
					       std::string (keyString (kdb_version)) + std::string ("\nKDB_GETENV_VERSION: ") +
					       KDB_GETENV_VERSION);

	elektraClose ();
}

TEST (GetEnv, DISABLED_NameArgv0)
{
	using namespace ckdb;
	int argc = 1;
	const char * cargv[] = { "path/to/any-name", nullptr };
	char ** argv = const_cast<char **> (cargv);

	elektraOpen (&argc, argv);
	ckdb::Key * k = ksLookupByName (elektraConfig, "proc:/elektra/intercept/getenv/layer/name", 0);
	ASSERT_NE (k, static_cast<ckdb::Key *> (nullptr));
	EXPECT_EQ (keyString (k), std::string ("path/to/any-name"));

	k = ksLookupByName (elektraConfig, "proc:/elektra/intercept/getenv/layer/basename", 0);
	ASSERT_NE (k, static_cast<ckdb::Key *> (nullptr));
	EXPECT_EQ (keyString (k), std::string ("any-name"));
	elektraClose ();
}


TEST (GetEnv, DISABLED_NameExplicit)
{
	using namespace ckdb;
	int argc = 2;
	const char * cargv[] = { "any-name", "--elektra%name%=other-name" };
	char ** argv = const_cast<char **> (cargv);

	elektraOpen (&argc, argv);
	ckdb::Key * k = ksLookupByName (elektraConfig, "proc:/elektra/intercept/getenv/layer/name", 0);
	ASSERT_NE (k, static_cast<ckdb::Key *> (nullptr));
	EXPECT_EQ (keyString (k), std::string ("other-name"));
	elektraClose ();
}

#include "main.cpp"
