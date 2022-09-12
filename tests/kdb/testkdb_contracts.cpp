/**
 * @file
 *
 * @brief Tests for the Backend builder class
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <gtest/gtest-elektra.h>
#include <kdb.hpp>

#include <regex>

class Contracts : public ::testing::Test
{
protected:
	static const char * testRoot;
	static const char * configFileRoot;
	std::string specRoot;
	std::string userRoot;

	testing::Namespaces namespaces;
	testing::MountpointPtr mpRoot;

	Contracts () : specRoot (std::string ("spec:") + testRoot), userRoot (std::string ("user:") + testRoot), namespaces ()
	{
	}

	virtual void SetUp () override
	{
		mpRoot.reset (new testing::Mountpoint (testRoot, configFileRoot));

		using namespace kdb;

		KeySet spec = KeySet (ckdb::ksNew (
			10, ckdb::keyNew (specRoot.c_str (), ELEKTRA_KEY_META, "command", "", ELEKTRA_KEY_END),
			ckdb::keyNew ((specRoot + "/printversion").c_str (), ELEKTRA_KEY_META, "description",
				      "print version information and exit (ignoring all other options/commands/parameters)", ELEKTRA_KEY_META,
				      "opt", "v", ELEKTRA_KEY_META, "opt/arg", "none", ELEKTRA_KEY_META, "opt/long", "version", ELEKTRA_KEY_END),
			ckdb::keyNew ((specRoot + "/getter").c_str (), ELEKTRA_KEY_META, "description", "get a key's value", ELEKTRA_KEY_META, "command",
				      "get", ELEKTRA_KEY_END),
			ckdb::keyNew ((specRoot + "/getter/verbose").c_str (), ELEKTRA_KEY_META, "description",
				      "print additional information about where the value comes from", ELEKTRA_KEY_META, "opt", "v", ELEKTRA_KEY_META,
				      "opt/long", "verbose", ELEKTRA_KEY_META, "opt/arg", "none", ELEKTRA_KEY_END),
			ckdb::keyNew ((specRoot + "/getter/keyname").c_str (), ELEKTRA_KEY_META, "description", "name of the key to read", ELEKTRA_KEY_META,
				      "args", "indexed", ELEKTRA_KEY_META, "args/index", "0", ELEKTRA_KEY_END),
			ckdb::keyNew ((specRoot + "/setter").c_str (), ELEKTRA_KEY_META, "description", "set a key's value", ELEKTRA_KEY_META, "command",
				      "set", ELEKTRA_KEY_END),
			ckdb::keyNew ((specRoot + "/setter/verbose").c_str (), ELEKTRA_KEY_META, "description",
				      "print additional information about where the value will be stored", ELEKTRA_KEY_META, "opt", "v", ELEKTRA_KEY_META,
				      "opt/long", "verbose", ELEKTRA_KEY_META, "opt/arg", "none", ELEKTRA_KEY_END),
			ckdb::keyNew ((specRoot + "/setter/keyname").c_str (), ELEKTRA_KEY_META, "description", "name of the key to write",
				      ELEKTRA_KEY_META, "args", "indexed", ELEKTRA_KEY_META, "args/index", "0", ELEKTRA_KEY_END),
			ckdb::keyNew ((specRoot + "/setter/value").c_str (), ELEKTRA_KEY_META, "description", "value to be written", ELEKTRA_KEY_META,
				      "args", "indexed", ELEKTRA_KEY_META, "args/index", "1", ELEKTRA_KEY_END),
			ckdb::keyNew ((specRoot + "/dynamic/#").c_str (), ELEKTRA_KEY_META, "description",
				      "dynamically call a user-supplied command", ELEKTRA_KEY_META, "args", "remaining", ELEKTRA_KEY_END),
			ELEKTRA_KS_END));

		KDB kdb;

		KeySet ks;
		kdb.get (ks, specRoot);
		ASSERT_EQ (ks.cut (Key (specRoot, ELEKTRA_KEY_END)).size (), 0) << "Couldn't setup spec, keys exist!";

		ks.append (spec);
		kdb.set (ks, specRoot);
	}

	virtual void TearDown () override
	{
		mpRoot.reset ();
	}

	static bool checkTracerOutput (const std::string & capture);
};

const char * Contracts::testRoot = "/tests/kdb/contracts";
const char * Contracts::configFileRoot = "kdbFileContracts.dump";

TEST_F (Contracts, GOpts)
{
	using namespace kdb;

	KeySet contract;
	KeySet config = KeySet (ckdb::ksNew (1, ckdb::keyNew ("user:/offset", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END), ELEKTRA_KS_END));

	std::vector<const char *> customArgv = { "dummy", "test", "get", "-v", "user:/", NULL };
	std::vector<const char *> customEnvp = { NULL };
	goptsContract (contract, customArgv.size (), customArgv.data (), customEnvp.data (), Key (testRoot, ELEKTRA_KEY_END), config);

	KDB kdb (contract);

	KeySet ks;
	kdb.get (ks, testRoot);

	Key k = ks.lookup (testRoot);
	EXPECT_TRUE (k);

	EXPECT_EQ (k.get<std::string> (), "getter");

	k = ks.lookup (std::string (testRoot) + "/getter/keyname");
	EXPECT_EQ (k.get<std::string> (), "user:/");

	k = ks.lookup (std::string (testRoot) + "/getter/verbose");
	EXPECT_EQ (k.get<std::string> (), "1");
}

TEST_F (Contracts, GOptsStringVector)
{
	using namespace kdb;

	KeySet contract;
	KeySet config = KeySet (ckdb::ksNew (1, ckdb::keyNew ("user:/offset", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END), ELEKTRA_KS_END));

	std::vector<std::string> customArgv = { "dummy", "test", "get", "-v", "user:/" };
	std::vector<std::string> customEnvp = {};
	goptsContract (contract, customArgv, customEnvp, Key (testRoot, ELEKTRA_KEY_END), config);

	KDB kdb (contract);

	KeySet ks;
	kdb.get (ks, testRoot);

	Key k = ks.lookup (testRoot);
	EXPECT_TRUE (k);

	EXPECT_EQ (k.get<std::string> (), "getter");

	k = ks.lookup (std::string (testRoot) + "/getter/keyname");
	EXPECT_EQ (k.get<std::string> (), "user:/");

	k = ks.lookup (std::string (testRoot) + "/getter/verbose");
	EXPECT_EQ (k.get<std::string> (), "1");
}
