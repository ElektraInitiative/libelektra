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

class Ensure : public ::testing::Test
{
protected:
	static const char * testRoot;
	static const char * configFileRoot;
	std::string specRoot;
	std::string userRoot;

	testing::Namespaces namespaces;
	testing::MountpointPtr mpRoot;

	Ensure () : specRoot (std::string ("spec:") + testRoot), userRoot (std::string ("user:") + testRoot), namespaces ()
	{
	}

	virtual void SetUp () override
	{
		mpRoot.reset (new testing::Mountpoint (testRoot, configFileRoot));

		using namespace kdb;
		KDB kdb;

		KeySet ks;
		kdb.get (ks, testRoot);

		ks.append (Key (specRoot + "/speckey/#", KEY_META, "mymeta", "1", KEY_END));
		ks.append (Key (userRoot + "/speckey", KEY_META, "array", "#0", KEY_END));
		ks.append (Key (userRoot + "/speckey/#0", KEY_VALUE, "", KEY_END));
		ks.append (Key (userRoot + "/errorkey", KEY_META, "trigger/warnings", "3", KEY_END));

		kdb.set (ks, testRoot);
	}

	virtual void TearDown () override
	{
		mpRoot.reset ();
	}

	static bool checkTracerOutput (const std::string & capture);
};

const char * Ensure::testRoot = "/tests/kdb/ensure";
const char * Ensure::configFileRoot = "kdbFileEnsure.dump";

/* FIXME: kdbEnsure
TEST_F (Ensure, GlobalUnmount)
{
	using namespace kdb;
	KDB kdb;

	{
		KeySet ks;
		kdb.get (ks, testRoot);

		EXPECT_EQ (ks.lookup (userRoot + "/speckey/#0", 0).getMeta<std::string> ("mymeta"), "1") << "spec plugin didn't run";
	}

	{
		KeySet contract;
		contract.append (Key ("system:/elektra/ensure/plugins/global/spec", KEY_VALUE, "unmounted", KEY_END));
		Key root (specRoot, KEY_END);
		kdb.ensure (contract, root);

		KeySet ks;
		kdb.get (ks, testRoot);

		EXPECT_FALSE (ks.lookup (userRoot + "/speckey/#0", 0).hasMeta ("mymeta")) << "spec plugin shouldn't have run";

		kdb.ensure (contract, root);
		kdb.get (ks, testRoot);

		EXPECT_FALSE (ks.lookup (userRoot + "/speckey/#0", 0).hasMeta ("mymeta")) << "global unmount should be idempotent";
	}
}

TEST_F (Ensure, GlobalRemount)
{
	using namespace kdb;
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, testRoot);

		ks.append (Key (specRoot + "/otherspeckey", KEY_META, "require", "", KEY_END));
		kdb.set (ks, testRoot);
	}

	{
		KDB kdb;
		KeySet contract;
		contract.append (Key ("system:/elektra/ensure/plugins/global/spec", KEY_VALUE, "remount", KEY_END));
		contract.append (Key ("system:/elektra/ensure/plugins/global/spec/config/conflict/get", KEY_VALUE, "ERROR", KEY_END));
		contract.append (Key ("system:/elektra/ensure/plugins/global/spec/config/conflict/set", KEY_VALUE, "ERROR", KEY_END));
		Key root (specRoot, KEY_END);
		kdb.ensure (contract, root);


		Key newRoot (testRoot, KEY_END);
		KeySet ks;
		EXPECT_THROW (kdb.get (ks, newRoot), KDBException);

		EXPECT_EQ (newRoot.getMeta<std::string> ("error/number"), "C03200") << "spec plugin should have produced error";
	}
}

TEST_F (Ensure, Unmount)
{
	using namespace kdb;
	KDB kdb;

	{
		KeySet ks;
		Key root (testRoot, KEY_END);
		kdb.get (ks, root);
		kdb.set (ks, root);

		EXPECT_EQ (root.getMeta<std::string> ("warnings/#0/number"), "C01310") << "error plugin didn't run";
	}

	{
		KeySet contract;
		contract.append (Key ("system:/elektra/ensure/plugins/parent/error", KEY_VALUE, "unmounted", KEY_END));
		Key uroot (userRoot, KEY_END);
		kdb.ensure (contract, uroot);

		KeySet ks;
		Key root (testRoot, KEY_END);
		kdb.get (ks, root);
		kdb.set (ks, root);

		EXPECT_FALSE (root.hasMeta ("warnings")) << "error plugin shouldn't have run";

		root.delMeta ("warnings");

		kdb.ensure (contract, uroot);
		kdb.get (ks, root);
		kdb.set (ks, root);

		EXPECT_FALSE (root.hasMeta ("warnings")) << "unmount should be idempotent";
	}
}

bool Ensure::checkTracerOutput (const std::string & capture)
{
	EXPECT_FALSE (capture.empty ()) << "tracer plugin not run";

	std::istringstream lines (capture);
	std::string line;
	size_t i = 0;
	bool success = true;
	while (std::getline (lines, line))
	{
		std::smatch match;
		success = success && std::regex_match (line, match, std::regex (R"(^tracer: .*$)"));
		EXPECT_TRUE (success) << "A line didn't match the expected tracer output:" << std::endl << "line: " << line << std::endl;
		++i;
	}

	return success;
}

TEST_F (Ensure, GlobalMount)
{
	using namespace kdb;
	KDB kdb;
	Key parent (testRoot, KEY_META, "debugGlobalPositions", "", KEY_END);

	{
		testing::internal::CaptureStdout ();
		KeySet ks;
		kdb.get (ks, parent);

		EXPECT_TRUE (testing::internal::GetCapturedStdout ().empty ()) << "there should be no output on stdout";
	}

	{
		KeySet contract;
		contract.append (Key ("system:/elektra/ensure/plugins/global/tracer", KEY_VALUE, "mounted", KEY_END));
		Key root (specRoot, KEY_END);
		kdb.ensure (contract, root);

		testing::internal::CaptureStdout ();
		KeySet ks;
		kdb.get (ks, parent);

		{
			SCOPED_TRACE ("first ensure");
			EXPECT_TRUE (checkTracerOutput (testing::internal::GetCapturedStdout ())) << "tracer output didn't match";
		}

		kdb.ensure (contract, root);

		testing::internal::CaptureStdout ();
		kdb.get (ks, parent);

		{
			SCOPED_TRACE ("second ensure");
			EXPECT_TRUE (checkTracerOutput (testing::internal::GetCapturedStdout ())) << "global mount should be idempotent";
		}
	}
}
*/