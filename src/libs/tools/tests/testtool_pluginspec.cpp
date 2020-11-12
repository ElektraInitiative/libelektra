/**
 * @file
 *
 * @brief Tests for the pluginspec class
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#define ELEKTRA_PLUGINSPEC_WITH_COMPARE


#include <pluginspec.hpp>

#include <toolexcept.hpp>

#include <gtest/gtest.h>


TEST (PluginSpec, construct)
{
	using namespace kdb;
	using namespace kdb::tools;
	EXPECT_EQ (PluginSpec ("c").getName (), "c");
	EXPECT_EQ (PluginSpec ("c").getFullName (), "c#c");

	EXPECT_EQ (PluginSpec ("c_c").getName (), "c_c");
	EXPECT_EQ (PluginSpec ("c_c").getFullName (), "c_c#c_c");

	EXPECT_EQ (PluginSpec ("a_").getName (), "a_");
	EXPECT_EQ (PluginSpec ("a_").getFullName (), "a_#a_");

	EXPECT_EQ (PluginSpec ("python2").getName (), "python2");
	EXPECT_EQ (PluginSpec ("python2").getFullName (), "python2#python2");
	EXPECT_FALSE (PluginSpec ("python2").isRefNumber ());

	EXPECT_EQ (PluginSpec ("c", "b").getName (), "c");
	EXPECT_EQ (PluginSpec ("c", "b").getRefName (), "b");
	EXPECT_EQ (PluginSpec ("c", "b").getFullName (), "c#b");

	PluginSpec s1 ("c#b");
	EXPECT_EQ (s1.getName (), "c");
	EXPECT_EQ (s1.getRefName (), "b");
	EXPECT_FALSE (s1.isRefNumber ());
	EXPECT_EQ (s1.getFullName (), "c#b");

	s1.setRefNumber (20);
	EXPECT_EQ (s1.getName (), "c");
	EXPECT_EQ (s1.getRefName (), "20");
	EXPECT_TRUE (s1.isRefNumber ());
	EXPECT_EQ (s1.getFullName (), "c#20");

	s1.setFullName ("c#b");
	EXPECT_EQ (s1.getName (), "c");
	EXPECT_EQ (s1.getRefName (), "b");
	EXPECT_FALSE (s1.isRefNumber ());
	EXPECT_EQ (s1.getFullName (), "c#b");

	EXPECT_THROW (s1.setRefName ("20"), BadPluginName);
	EXPECT_THROW (s1.setFullName ("a#20"), BadPluginName);
	EXPECT_THROW (s1.setName ("a#20"), BadPluginName);
	EXPECT_THROW (s1.setName ("0"), BadPluginName);
}


TEST (PluginSpec, appendConfig)
{
	using namespace kdb;
	using namespace kdb::tools;
	PluginSpec s1 ("c#b");
	EXPECT_EQ (s1, PluginSpec ("c#b"));
	s1.appendConfig (KeySet (5, *Key ("user:/a", KEY_END), KS_END));
	EXPECT_EQ (s1, PluginSpec ("c#b", KeySet (5, *Key ("user:/a", KEY_END), KS_END)));
	s1.appendConfig (KeySet (5, *Key ("user:/b", KEY_END), KS_END));
	EXPECT_EQ (s1, PluginSpec ("c#b", KeySet (5, *Key ("user:/a", KEY_END), *Key ("user:/b", KEY_END), KS_END)));
	EXPECT_EQ (s1.getConfig ().lookup ("user:/b").getString (), "");
	s1.appendConfig (KeySet (5, *Key ("user:/b", KEY_VALUE, "abc", KEY_END), KS_END));
	EXPECT_EQ (s1, PluginSpec ("c#b", KeySet (5, *Key ("user:/a", KEY_END), *Key ("user:/b", KEY_END), KS_END)));
	EXPECT_EQ (s1.getConfig ().lookup ("user:/b").getString (), "abc");
}

TEST (PluginSpec, wrongNames)
{
	using namespace kdb;
	using namespace kdb::tools;
	EXPECT_THROW (PluginSpec ("c#"), BadPluginName);
	EXPECT_THROW (PluginSpec ("#"), BadPluginName);
	EXPECT_THROW (PluginSpec ("#a"), BadPluginName);
	EXPECT_THROW (PluginSpec ("a##a"), BadPluginName);
	EXPECT_THROW (PluginSpec ("a#a#"), BadPluginName);
	EXPECT_THROW (PluginSpec ("0#a"), BadPluginName);
	EXPECT_THROW (PluginSpec ("a#0"), BadPluginName);
	EXPECT_THROW (PluginSpec ("a#?"), BadPluginName);
	EXPECT_THROW (PluginSpec ("a#$"), BadPluginName);
	EXPECT_THROW (PluginSpec ("a#a!"), BadPluginName);
	EXPECT_THROW (PluginSpec ("a#(a"), BadPluginName);
	EXPECT_THROW (PluginSpec ("a#a)"), BadPluginName);
	EXPECT_THROW (PluginSpec ("(a#a"), BadPluginName);
	EXPECT_THROW (PluginSpec ("[a#a"), BadPluginName);
	EXPECT_THROW (PluginSpec ("K#a"), BadPluginName);
	EXPECT_THROW (PluginSpec ("y#Y"), BadPluginName);
	EXPECT_THROW (PluginSpec ("1a"), BadPluginName);
	EXPECT_THROW (PluginSpec ("1"), BadPluginName);
	EXPECT_THROW (PluginSpec ("_"), BadPluginName);
	EXPECT_THROW (PluginSpec ("_1"), BadPluginName);
	EXPECT_THROW (PluginSpec ("_a"), BadPluginName);
}


TEST (PluginSpec, compare)
{
	using namespace kdb;
	using namespace kdb::tools;

	// by default we compare by value
	EXPECT_EQ (PluginSpec ("c"), PluginSpec ("c"));
	EXPECT_EQ (PluginSpec ("c", KeySet (2, *Key ("user:/abc", KEY_END), KS_END)),
		   PluginSpec ("c", KeySet (2, *Key ("user:/abc", KEY_END), KS_END)));
	EXPECT_EQ (PluginSpec ("c", "abc", KeySet (2, *Key ("user:/abc", KEY_END), KS_END)),
		   PluginSpec ("c", "abc", KeySet (2, *Key ("user:/abc", KEY_END), KS_END)));
	EXPECT_EQ (PluginSpec ("c", 5, KeySet (2, *Key ("user:/abc", KEY_END), KS_END)),
		   PluginSpec ("c", 5, KeySet (2, *Key ("user:/abc", KEY_END), KS_END)));

	EXPECT_NE (PluginSpec ("c"), PluginSpec ("d"));
	EXPECT_NE (PluginSpec ("c", KeySet (2, *Key ("user:/abe", KEY_END), KS_END)),
		   PluginSpec ("c", KeySet (2, *Key ("user:/abc", KEY_END), KS_END)));
	EXPECT_NE (PluginSpec ("c", "ab", KeySet (2, *Key ("user:/abc", KEY_END), KS_END)),
		   PluginSpec ("c", "abc", KeySet (2, *Key ("user:/abc", KEY_END), KS_END)));
	EXPECT_NE (PluginSpec ("c", 6, KeySet (2, *Key ("user:/abc", KEY_END), KS_END)),
		   PluginSpec ("c", 5, KeySet (2, *Key ("user:/abc", KEY_END), KS_END)));

	EXPECT_NE (PluginSpec ("c", "b"), PluginSpec ("c"));
	EXPECT_NE (PluginSpec ("c", "b", KeySet (2, *Key ("user:/abc", KEY_END), KS_END)), PluginSpec ("c"));
	EXPECT_NE (PluginSpec ("c", "b"), PluginSpec ("c", KeySet (2, *Key ("user:/abc", KEY_END), KS_END)));
	EXPECT_NE (PluginSpec ("c", "b", KeySet (2, *Key ("user:/abc", KEY_END), KS_END)),
		   PluginSpec ("c", KeySet (2, *Key ("user:/def", KEY_END), KS_END)));
	EXPECT_NE (PluginSpec ("c", "b", KeySet (2, *Key ("user:/a", KEY_END), KS_END)),
		   PluginSpec ("c", KeySet (2, *Key ("user:/aa", KEY_END), KS_END)));
	EXPECT_NE (PluginSpec ("c", "b", KeySet (2, *Key ("user:/a", KEY_END), KS_END)),
		   PluginSpec ("c", KeySet (2, *Key ("user:/a", KEY_END), KS_END)));

	// compare by full name (config does not matter)
	EXPECT_PRED2 (PluginSpecFullName (), PluginSpec ("c"), PluginSpec ("c", KeySet (2, *Key ("user:/abc", KEY_END), KS_END)));
	EXPECT_PRED2 (PluginSpecFullName (), PluginSpec ("c", KeySet (2, *Key ("user:/abc", KEY_END), KS_END)),
		      PluginSpec ("c", KeySet (2, *Key ("user:/def", KEY_END), KS_END)));
	EXPECT_PRED2 (PluginSpecFullName (), PluginSpec ("c", KeySet (2, *Key ("user:/a", KEY_END), KS_END)),
		      PluginSpec ("c", KeySet (2, *Key ("user:/aa", KEY_END), KS_END)));
	EXPECT_PRED2 (PluginSpecFullName (), PluginSpec ("c", KeySet (2, *Key ("user:/a", KEY_END), KS_END)),
		      PluginSpec ("c", KeySet (2, *Key ("user:/a", KEY_END), KS_END)));

	// compare by ref name (name+config does not matter)
	EXPECT_PRED2 (PluginSpecRefName (), PluginSpec ("d", "e"), PluginSpec ("c", "e", KeySet (2, *Key ("user:/abc", KEY_END), KS_END)));
	EXPECT_PRED2 (PluginSpecRefName (), PluginSpec ("e", "e", KeySet (2, *Key ("user:/abc", KEY_END), KS_END)),
		      PluginSpec ("c", "e", KeySet (2, *Key ("user:/def", KEY_END), KS_END)));
	EXPECT_PRED2 (PluginSpecRefName (), PluginSpec ("e", "e", KeySet (2, *Key ("user:/c", KEY_END), KS_END)),
		      PluginSpec ("c", "e", KeySet (2, *Key ("user:/aa", KEY_END), KS_END)));
	EXPECT_PRED2 (PluginSpecRefName (), PluginSpec ("c", "e", KeySet (2, *Key ("user:/c", KEY_END), KS_END)),
		      PluginSpec ("c", "e", KeySet (2, *Key ("user:/a", KEY_END), KS_END)));
	EXPECT_PRED2 (PluginSpecRefName (), PluginSpec ("c", "abc", KeySet (2, *Key ("user:/c", KEY_END), KS_END)),
		      PluginSpec ("c", "abc", KeySet (2, *Key ("user:/a", KEY_END), KS_END)));
	EXPECT_PRED2 (PluginSpecRefName (), PluginSpec ("c", 5, KeySet (2, *Key ("user:/c", KEY_END), KS_END)),
		      PluginSpec ("c", 5, KeySet (2, *Key ("user:/a", KEY_END), KS_END)));
}


TEST (PluginSpec, hash)
{
	using namespace kdb;
	using namespace kdb::tools;
	PluginSpecHash hashFun;
	EXPECT_EQ (hashFun (PluginSpec ("c", KeySet (2, *Key ("user:/a", KEY_END), KS_END))),
		   hashFun (PluginSpec ("c", KeySet (2, *Key ("user:/x", KEY_END), KS_END))));
	EXPECT_EQ (hashFun (PluginSpec ("c", "b", KeySet (2, *Key ("user:/a", KEY_END), KS_END))),
		   hashFun (PluginSpec ("c", "b", KeySet (2, *Key ("user:/x", KEY_END), KS_END))));
	EXPECT_EQ (hashFun (PluginSpec ("c", "b", KeySet (2, *Key ("user:/a", KEY_END), KS_END))),
		   hashFun (PluginSpec ("c", "d", KeySet (2, *Key ("user:/x", KEY_END), KS_END))));
	EXPECT_NE (hashFun (PluginSpec ("c", KeySet (2, *Key ("user:/a", KEY_END), KS_END))),
		   hashFun (PluginSpec ("d", KeySet (2, *Key ("user:/x", KEY_END), KS_END))));
}
