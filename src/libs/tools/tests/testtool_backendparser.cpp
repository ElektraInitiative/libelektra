/**
 * @file
 *
 * @brief Tests for the Backend parser class
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#include <backendparser.hpp>

#include <backendbuilder.hpp>

#include <string>

#include <toolexcept.hpp>

#include <gtest/gtest.h>


TEST(MountBackendBuilder, parsePluginArguments)
{
	using namespace kdb;
	using namespace kdb::tools;
	EXPECT_EQ (KeySet(5, *Key("user/a", KEY_VALUE, "5", KEY_END), KS_END),
		   parsePluginArguments("a=5"));
	EXPECT_EQ (KeySet(5, *Key("user", KEY_END), KS_END),
		   parsePluginArguments("="));
	EXPECT_EQ (KeySet (5,
			*Key("user/a", KEY_VALUE, "5", KEY_END),
			*Key("user/ax", KEY_VALUE, "a", KEY_END),
			*Key("user/ax/bx", KEY_VALUE, "8", KEY_END),
			KS_END),
		  parsePluginArguments ("a=5,ax=a,ax/bx=8"));
	EXPECT_EQ (KeySet (5,
			*Key("user", KEY_VALUE, "5", KEY_END),
			*Key("user/ax", KEY_END, KEY_END),
			*Key("user/ax/bx", KEY_VALUE, "8", KEY_END),
			KS_END),
		  parsePluginArguments ("=5,ax=,ax/bx=8"));
}


bool cmpPsv(kdb::tools::PluginSpecVector psv1, kdb::tools::PluginSpecVector psv2)
{
	EXPECT_EQ(psv1.size(), psv2.size());
	if (psv1.size() != psv2.size()) return false;
	for (size_t i=0; i<psv1.size(); ++i)
	{
		EXPECT_EQ (psv1[i], psv2[i]);
		if (!(psv1[i] == psv2[i])) return false;
	}
	return true;
}

TEST(MountBackendBuilder, parseArguments)
{
	using namespace kdb;
	using namespace kdb::tools;
	PluginSpecVector psv1;
	psv1.push_back (PluginSpec ("a", KeySet(5, *Key("user/a", KEY_VALUE, "5", KEY_END), KS_END)));
	psv1.push_back (PluginSpec ("b"));
	psv1.push_back (PluginSpec ("c"));
	PluginSpecVector psv2 = parseArguments ("a a=5 b c");
	EXPECT_TRUE(cmpPsv (psv1, psv2));
	psv2 = parseArguments ("  a  a=5  b c   ");
	EXPECT_TRUE(cmpPsv (psv1, psv2));
	psv2 = parseArguments ("  a 	 a=5	  b c ,  ");
	EXPECT_TRUE(cmpPsv (psv1, psv2));
	EXPECT_THROW(parseArguments ("a=5 a b c"), ParseException);
	EXPECT_THROW(parseArguments ("a#a b c a#a"), ParseException);
}

TEST(MountBackendBuilder, parseVectorArguments)
{
	using namespace kdb;
	using namespace kdb::tools;
	PluginSpecVector psv1;
	psv1.push_back (PluginSpec ("a", KeySet(5,
					*Key("user/a", KEY_VALUE, "5", KEY_END),
					*Key("user/b", KEY_VALUE, "3", KEY_END),
					KS_END)));
	psv1.push_back (PluginSpec ("b"));
	psv1.push_back (PluginSpec ("c", KeySet(5,
					*Key("user/a", KEY_VALUE, "5", KEY_END),
					*Key("user/b", KEY_VALUE, "3", KEY_END),
					KS_END)));
	PluginSpecVector psv2 = parseArguments ({"a", "a=5 b=3", "b",  "c", "a=5, b=3"});
	EXPECT_TRUE(cmpPsv (psv1, psv2));
	psv2 = parseArguments ("  a  a=5  b c   ");
	EXPECT_TRUE(cmpPsv (psv1, psv2));
	psv2 = parseArguments ("  a 	 a=5	  b c ,  ");
	EXPECT_TRUE(cmpPsv (psv1, psv2));
	EXPECT_THROW(parseArguments ("a=5 a b c"), ParseException);
	EXPECT_THROW(parseArguments ("a#a b c a#a"), ParseException);
}

TEST(MountBackendBuilder, parseArgumentsIdentical)
{
	using namespace kdb;
	using namespace kdb::tools;
	EXPECT_THROW(parseArguments ("a#a a#a"), ParseException);
	EXPECT_THROW(parseArguments ("a#a b#a"), ParseException);
	EXPECT_THROW(parseArguments ("a#a a#0"), BadPluginName);
	EXPECT_THROW(parseArguments ("a#b b#b"), ParseException);
	EXPECT_THROW(parseArguments ("a#same b#same"), ParseException);
	EXPECT_THROW(parseArguments ("a#same b c#same"), ParseException);
	EXPECT_THROW(parseArguments ("a#same b c d#same"), ParseException);
	EXPECT_THROW(parseArguments ("a#same b c d e#same"), ParseException);
	EXPECT_THROW(parseArguments ("a#same b c d e#same f g h"), ParseException);
}

TEST(MountBackendBuilder, parseArgumentsNearlyIdentical)
{
	using namespace kdb;
	using namespace kdb::tools;
	PluginSpecVector psv1;
	psv1.push_back (PluginSpec ("a", "a"));
	psv1.push_back (PluginSpec ("a", 0));
	PluginSpecVector psv2 = parseArguments ("a#a a");
	EXPECT_TRUE(cmpPsv (psv1, psv2));
}

TEST(MountBackendBuilder, parseArgumentsDoubleOccur)
{
	using namespace kdb;
	using namespace kdb::tools;
	PluginSpecVector psv1;
	psv1.push_back (PluginSpec ("a", 0, KeySet(5,
				*Key("user/a", KEY_VALUE, "5", KEY_END),
				KS_END)));
	psv1.push_back (PluginSpec ("b"));
	psv1.push_back (PluginSpec ("c"));
	psv1.push_back (PluginSpec ("a", 1, KeySet(5,
				*Key("user/b", KEY_VALUE, "c", KEY_END),
				KS_END)));
	PluginSpecVector psv2 = parseArguments ("a a=5 b c a b=c");
	EXPECT_TRUE(cmpPsv (psv1, psv2));
	psv2 = parseArguments ("  a  a=5  b c a b=c   ");
	EXPECT_TRUE(cmpPsv (psv1, psv2));
	psv2 = parseArguments ("  a 	 a=5	  b c , a b=c  ");
	EXPECT_TRUE(cmpPsv (psv1, psv2));
	EXPECT_THROW(parseArguments ("a=5 a b c a b=c"), ParseException);
	EXPECT_THROW(parseArguments ("a a=5 b c a#0 b=c"), BadPluginName);
	EXPECT_THROW(parseArguments ("a a=5 b c # b=c"), BadPluginName);
	EXPECT_THROW(parseArguments ("a a=5 b c #a b=c"), BadPluginName);
}

TEST(MountBackendBuilder, parseArgumentsRef)
{
	using namespace kdb;
	using namespace kdb::tools;
	PluginSpecVector psv1;
	psv1.push_back (PluginSpec ("a", "mya", KeySet(5,
				*Key("user/a", KEY_VALUE, "5", KEY_END),
				KS_END)));
	psv1.push_back (PluginSpec ("b", "myb"));
	psv1.push_back (PluginSpec ("c", "myc"));
	psv1.push_back (PluginSpec ("a", "othera", KeySet(5,
				*Key("user/b", KEY_VALUE, "c", KEY_END),
				KS_END)));
	PluginSpecVector psv2 = parseArguments ("a#mya a=5 b#myb c#myc a#othera b=c");
	EXPECT_TRUE(cmpPsv (psv1, psv2));
	psv2 = parseArguments ("  a#mya  a=5  b#myb c#myc a#othera b=c   ");
	EXPECT_TRUE(cmpPsv (psv1, psv2));
	psv2 = parseArguments ("  a#mya 	 a=5 	  b#myb c#myc , a#othera b=c  ");
	EXPECT_TRUE(cmpPsv (psv1, psv2));
}


TEST(MountBackendBuilder, parseArgumentsProvider)
{
	using namespace kdb;
	using namespace kdb::tools;
	PluginSpecVector psv1;
	psv1.push_back (PluginSpec ("augeas", "aaa", KeySet(5,
				*Key("user/a", KEY_VALUE, "5", KEY_END),
				KS_END)));
	psv1.push_back (PluginSpec ("logging", "logg"));
	psv1.push_back (PluginSpec ("code", "nee"));
	psv1.push_back (PluginSpec ("hexcode", "hexcode", KeySet(5,
				*Key("user/b", KEY_VALUE, "c", KEY_END),
				KS_END)));
	PluginSpecVector psv2 = parseArguments ("augeas#aaa logging#logg code#nee escape=% hexcode escape=-");
	EXPECT_TRUE (cmpPsv (psv1, psv2));


	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data[PluginSpec("ccode")]["provides"] = "code";
	mpd->data[PluginSpec("syslog")]["provides"] = "logging";
	mpd->data[PluginSpec("augeas")][""];
	mpd->data[PluginSpec("hexcode")][""];
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	for (auto const & p : psv2)
	{
		bb.addPlugin(p);
	}
	PluginSpecVector psv3;
	psv3.push_back (PluginSpec ("augeas", "aaa", KeySet(5,
				*Key("user/a", KEY_VALUE, "5", KEY_END),
				KS_END)));
	psv3.push_back (PluginSpec ("syslog", "logg"));
	psv3.push_back (PluginSpec ("ccode", "nee"));
	psv3.push_back (PluginSpec ("hexcode", "hexcode", KeySet(5,
				*Key("user/b", KEY_VALUE, "c", KEY_END),
				KS_END)));
	PluginSpecVector psv4 (bb.begin(), bb.end());
	EXPECT_TRUE (cmpPsv (psv3, psv4));
}

TEST(MountBackendBuilder, parseArgumentsSameProvider)
{
	using namespace kdb;
	using namespace kdb::tools;
	PluginSpecVector psv1;
	psv1.push_back (PluginSpec ("augeas", "a1"));
	psv1.push_back (PluginSpec ("logging", "a2"));
	psv1.push_back (PluginSpec ("code", "a3", KeySet(5,
				*Key("user/escape", KEY_VALUE, "%", KEY_END),
				KS_END)));
	psv1.push_back (PluginSpec ("code", "a4", KeySet(5,
				*Key("user/escape", KEY_VALUE, "-", KEY_END),
				KS_END)));
	psv1.push_back (PluginSpec ("code", "a5"));
	PluginSpecVector psv2 = parseArguments ("augeas#a1 logging#a2 code#a3 escape=% code#a4 escape=- code#a5");
	EXPECT_TRUE (cmpPsv (psv1, psv2));


	std::shared_ptr<MockPluginDatabase> mpd = std::make_shared<MockPluginDatabase>();
	mpd->data[PluginSpec("ccode")]["provides"] = "code";
	mpd->data[PluginSpec("syslog")]["provides"] = "logging";
	mpd->data[PluginSpec("augeas")][""];
	mpd->data[PluginSpec("hexcode")][""];
	BackendBuilderInit bbi (mpd);
	BackendBuilder bb (bbi);
	for (auto const & p : psv2)
	{
		bb.addPlugin(p);
	}
	PluginSpecVector psv3;
	PluginSpecVector psv4 (bb.begin(), bb.end());
	psv3.push_back (PluginSpec ("augeas", "a1"));
	psv3.push_back (PluginSpec ("syslog", "a2"));
	psv3.push_back (PluginSpec ("ccode", "a3", KeySet(5,
				*Key("user/escape", KEY_VALUE, "%", KEY_END),
				KS_END)));
	psv3.push_back (PluginSpec ("ccode", "a4", KeySet(5,
				*Key("user/escape", KEY_VALUE, "-", KEY_END),
				KS_END)));
	psv3.push_back (PluginSpec ("ccode", "a5"));
	EXPECT_TRUE (cmpPsv (psv3, psv4));
}

