/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests.hpp>

#include <stdexcept>
#include <string>
#include <vector>

TEST (meta, basic)
{
	// cout << "testing metainfo" << endl;
	Key test;

	succeed_if (test.hasMeta ("mode") == false, "has meta?");
	succeed_if (test.getMeta<mode_t> ("mode") == 0, "not properly default constructed");

	test.setMeta<mode_t> ("mode", 0775);
	succeed_if (test.hasMeta ("mode") == true, "has not meta even though set?");

	succeed_if (test.getMeta<mode_t> ("mode") == 0775, "not correct default mode for dir");

	test.setMeta<int> ("myint", 333);
	succeed_if (test.getMeta<int> ("myint") == 333, "could not set other meta");

	test.setMeta<double> ("mydouble", 333.3);
	succeed_if (test.hasMeta ("mydouble"), "no metadata even though it was just set");
	succeed_if (test.getMeta<double> ("mydouble") >= 333.2, "could not set other meta");
	succeed_if (test.getMeta<double> ("mydouble") <= 333.4, "could not set other meta");

	test.delMeta ("mydouble");
	succeed_if (!test.hasMeta ("mydouble"), "metadata there even though it was just deleted");

	test.setMeta<std::string> ("mystr", "str");
	succeed_if (test.getMeta<std::string> ("mystr") == "str", "could not set other meta");

	const ckdb::Key * cmeta = test.getMeta<const ckdb::Key *> ("mystr");
	succeed_if (!strcmp (static_cast<const char *> (ckdb::keyValue (cmeta)), "str"), "could not set other meta");

	const ckdb::Key * nmeta = test.getMeta<const ckdb::Key *> ("not available");
	succeed_if (nmeta == nullptr, "not available metadata did not give a null pointer");

	const Key meta = test.getMeta<const Key> ("mystr");
	succeed_if (meta, "null key");
	succeed_if (meta.getString () == "str", "could not get other meta");

	const Key xmeta = test.getMeta<const Key> ("not available");
	succeed_if (!xmeta, "not a null key");

	const char * str = test.getMeta<const char *> ("mystr");
	succeed_if (!strcmp (str, "str"), "could not get meta as c-string");

	const char * nstr = test.getMeta<const char *> ("not available");
	succeed_if (nstr == nullptr, "did not get null pointer on not available metadata");

	succeed_if (test.getMeta<int> ("not available") == 0, "not default constructed");
	succeed_if (test.getMeta<std::string> ("not available") == "", "not default constructed");

	test.setMeta<std::string> ("wrong", "not an int");

	try
	{
		succeed_if (test.hasMeta ("wrong") == true, "meta is here");
		test.getMeta<int> ("wrong");
		succeed_if (0, "exception did not raise");
	}
	catch (KeyTypeConversion const & e)
	{
		succeed_if (1, "no such metadata");
	}
}

TEST (meta, iter)
{
	// clang-format off
	Key k ("user:/metakey",
		KEY_META, "a", "meta",
		KEY_META, "b", "my",
		KEY_META, "c", "other",
		KEY_END);
	// clang-format on

	Key meta; // key = keyNew(0)

	succeed_if (meta, "key is a not null key");

	Key end = static_cast<ckdb::Key *> (nullptr); // key = 0
	succeed_if (!end, "key is a null key");

	ckdb::KeySet * metaKeys = ckdb::keyMeta (k.getKey ());
	succeed_if (ckdb::ksGetSize (metaKeys) == 3, "Not the correct number of metadata");

	k.setMeta ("d", "more");
	k.setMeta ("e", "even more");

	metaKeys = ckdb::keyMeta (k.getKey ());
	succeed_if (ckdb::ksGetSize (metaKeys) == 5, "Not the correct number of metadata");
}

TEST (test, copy)
{
	// cout << "testing copy meta" << std::endl;

	Key k ("user:/metakey", KEY_META, "", "metavalue", KEY_META, "a", "a metavalue", KEY_META, "b", "b metavalue", KEY_META, "c",
	       "c metavalue", KEY_END);
	Key c;

	c.copyMeta (k, "a");

	succeed_if (k.getMeta<const ckdb::Key *> ("a") == c.getMeta<const ckdb::Key *> ("a"), "copy meta did not work");

	c.copyMeta (k, "");

	succeed_if (k.getMeta<const ckdb::Key *> ("") == c.getMeta<const ckdb::Key *> (""), "copy meta did not work");

	k.setMeta<int> ("a", 420);

	succeed_if (k.getMeta<int> ("a") == 420, "could not get value set before");

	c.copyMeta (k, "a");
	succeed_if (c.getMeta<int> ("a") == 420, "could not get value copied before");
	succeed_if (k.getMeta<const ckdb::Key *> ("a") == c.getMeta<const ckdb::Key *> ("a"), "copy meta did not work");

	c.copyMeta (k, "a");
	succeed_if (c.getMeta<int> ("a") == 420, "could not get value copied before (again)");
	succeed_if (k.getMeta<const ckdb::Key *> ("a") == c.getMeta<const ckdb::Key *> ("a"), "copy meta did not work (again)");


	Key d;
	ckdb::KeySet * metaKeys = ckdb::keyMeta (k.getKey ());
	for (ssize_t it = 0; it < ckdb::ksGetSize (metaKeys); ++it)
	{
		const Key & curMeta = ckdb::ksAtCursor (metaKeys, it);
		d.copyMeta (k, curMeta.getName ());
	}

	succeed_if (d.getMeta<std::string> ("a") == "420", "did not copy metavalue in the loop");
	succeed_if (d.getMeta<const ckdb::Key *> ("a") == d.getMeta<const ckdb::Key *> ("a"), "copy meta did not work in the loop");

	succeed_if (d.getMeta<std::string> ("") == "metavalue", "did not copy metavalue in the loop");
	succeed_if (d.getMeta<const ckdb::Key *> ("") == d.getMeta<const ckdb::Key *> (""), "copy meta did not work in the loop");

	succeed_if (d.getMeta<std::string> ("b") == "b metavalue", "did not copy metavalue in the loop");
	succeed_if (d.getMeta<const ckdb::Key *> ("b") == d.getMeta<const ckdb::Key *> ("b"), "copy meta did not work in the loop");

	succeed_if (d.getMeta<std::string> ("c") == "c metavalue", "did not copy metavalue in the loop");
	succeed_if (d.getMeta<const ckdb::Key *> ("c") == d.getMeta<const ckdb::Key *> ("c"), "copy meta did not work in the loop");
}

TEST (meta, string)
{
	Key k ("user:/anything", KEY_META, "", "metavalue", KEY_META, "a", "a metavalue", KEY_META, "b", "b metavalue", KEY_META, "c",
	       "c metavalue", KEY_END);

	succeed_if (k.getMeta<string> ("a") == "a metavalue", "could not get metavalue");

	Key m = k.getMeta<const Key> ("a");
	succeed_if (m, "could not get metakey");
	succeed_if (m.getString () == "a metavalue", "could not get meta string");

	Key m1 = k.getMeta<const Key> ("x");
	succeed_if (!m1, "got not existing metakey");
}

TEST (meta, copyAll)
{
	Key k ("user:/metakey", KEY_META, "", "metavalue", KEY_META, "a", "a metavalue", KEY_META, "b", "b metavalue", KEY_META, "c",
	       "c metavalue", KEY_META, "i", "420", KEY_END);
	Key c;

	c.copyAllMeta (k);

	succeed_if (c.getMeta<const ckdb::Key *> ("a") == c.getMeta<const ckdb::Key *> ("a"), "copy meta did not work");
	succeed_if (c.getMeta<const ckdb::Key *> ("") == c.getMeta<const ckdb::Key *> (""), "copy meta did not work");
	succeed_if (c.getMeta<int> ("i") == 420, "could not get value copied before");
	succeed_if (c.getMeta<std::string> ("i") == "420", "did not copy metavalue in the loop");
	succeed_if (c.getMeta<const ckdb::Key *> ("a") == c.getMeta<const ckdb::Key *> ("a"), "copy meta did not work");

	Key d;

	d.copyAllMeta (k);
	succeed_if (d.getMeta<std::string> ("i") == "420", "did not copy metavalue in the loop");
	succeed_if (d.getMeta<int> ("i") == 420, "could not get value copied before");
	succeed_if (k.getMeta<const ckdb::Key *> ("a") == d.getMeta<const ckdb::Key *> ("a"), "copy meta did not work in the loop");

	succeed_if (d.getMeta<std::string> ("") == "metavalue", "did not copy metavalue in the loop");
	succeed_if (k.getMeta<const ckdb::Key *> ("") == d.getMeta<const ckdb::Key *> (""), "copy meta did not work in the loop");

	succeed_if (d.getMeta<std::string> ("b") == "b metavalue", "did not copy metavalue in the loop");
	succeed_if (k.getMeta<const ckdb::Key *> ("b") == d.getMeta<const ckdb::Key *> ("b"), "copy meta did not work in the loop");

	succeed_if (d.getMeta<std::string> ("c") == "c metavalue", "did not copy metavalue in the loop");
	succeed_if (k.getMeta<const ckdb::Key *> ("c") == d.getMeta<const ckdb::Key *> ("c"), "copy meta did not work in the loop");
}

/* check the wrappers for underlying c-functions with a metakey
 * exceptions are thrown if the underlying c-functions return error codes (-1 or NULL) */
TEST (meta, cErrorsMetaKeys)
{
	Key k ("user:/key", KEY_VALUE, "testkey", KEY_END);
	Key m;

	k.setMeta ("metaKey", "metaValue");

	ckdb::KeySet * metaKeys = ckdb::keyMeta (k.getKey ());

	EXPECT_EQ (ckdb::ksGetSize (metaKeys), 1);
	m = ckdb::ksAtCursor (metaKeys, 0);


	EXPECT_THROW (m.addName ("test"), KeyInvalidName);
	EXPECT_THROW (m.setName ("test"), KeyInvalidName);
	EXPECT_THROW (m.addBaseName ("test"), KeyInvalidName);
	EXPECT_THROW (m.setBaseName ("test"), KeyInvalidName);
	EXPECT_THROW (m.delBaseName (), KeyInvalidName);

	EXPECT_THROW (m.setMeta ("metaKey2", "metaValue2"), KeyException);
	EXPECT_THROW (m.delMeta ("metaKey2"), KeyException);

	EXPECT_THROW (m.set ("Test"), KeyException);

	/* m should be read-only */
	EXPECT_THROW (m.copy (k), KeyException);
	EXPECT_NE (k, m);
	/* copying m to k should work */
	EXPECT_NO_THROW (k.copy (m));
	EXPECT_EQ (k, m);

	EXPECT_THROW (m.setString ("changedMetaValue"), KeyException);
	EXPECT_EQ (m.getString (), "metaValue");
	EXPECT_EQ (m.getReferenceCounter (), 1);

	/* should only fail on null key */
	EXPECT_NO_THROW (m--);
	EXPECT_EQ (m.getReferenceCounter (), 0);

	/* should only fail on null key */
	EXPECT_NO_THROW (m.clear ());
}
