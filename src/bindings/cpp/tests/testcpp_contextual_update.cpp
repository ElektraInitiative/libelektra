/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbthread.hpp>


#include <gtest/gtest.h>

using namespace kdb;

struct test_contextual_update : ::testing::Test
{
	test_contextual_update ()
	: ks (), gc (), c (gc), i (ks, c, Key ("/ignore/id", KEY_META, "default", "my", KEY_END)),
	  x (ks, c, Key ("/%id%/key", KEY_META, "default", "33", KEY_END)){};

	KeySet ks;
	Coordinator gc;
	ThreadContext c;
	ThreadValue<std::string> i;
	ThreadValue<int> x;
};

TEST_F (test_contextual_update, activate)
{
	ASSERT_EQ (x.getName (), "default:/%/key");
	ASSERT_TRUE (ks.lookup ("/%/key"));
	c.activate (i);
	ASSERT_EQ (x.getName (), "default:/my/key");
	ASSERT_TRUE (ks.lookup ("/my/key"));
}

TEST_F (test_contextual_update, changeKey)
{
	ks.append (Key ("default:/other/key", KEY_VALUE, "88", KEY_END));
	i = "other";
	c.activate (i);
	ASSERT_EQ (x.getName (), "default:/other/key");
	ASSERT_TRUE (ks.lookup ("/other/key"));
	ASSERT_EQ (x, 88);
	ASSERT_EQ (ks.lookup ("/other/key").getString (), "88");

	ks.lookup ("/other/key").setString ("100");

	c.syncLayers ();
	ASSERT_EQ (x, 88) << "should not influence cache";
	ASSERT_EQ (x.getName (), "default:/other/key");
	ASSERT_EQ (ks.lookup ("/other/key").getString (), "100");

	x.syncCache ();
	ASSERT_EQ (x.getName (), "default:/other/key");
	ASSERT_EQ (ks.lookup ("/other/key").getString (), "100");
	ASSERT_EQ (x, 100) << "cache should be updated";
}

TEST_F (test_contextual_update, syncCache)
{
	ks.append (Key ("default:/%/key", KEY_VALUE, "111", KEY_END));

	x.syncCache ();
	ASSERT_EQ (x.getName (), "default:/%/key");
	ASSERT_EQ (ks.lookup ("/%/key").getString (), "111");
	ASSERT_EQ (x, 111) << "reevaluated context, should have found new key";
}

TEST_F (test_contextual_update, notifyAllEvents)
{
	ks.append (Key ("default:/%/key", KEY_VALUE, "133", KEY_END));

	c.notifyAllEvents ();
	ASSERT_EQ (x.getName (), "default:/%/key");
	ASSERT_EQ (x, 33) << "should not be changed (optimization)";
	ASSERT_EQ (ks.lookup ("/%/key").getString (), "133") << "nothing done, so its not changed";
}

TEST_F (test_contextual_update, notifyAllEventsChange)
{
	ASSERT_EQ (ks.size (), 2);
	ks.append (Key ("default:/other/key", KEY_VALUE, "133", KEY_END));
	ASSERT_EQ (ks.size (), 3);
	EXPECT_EQ (ks.at (0).getName (), "default:/%/key") << "nothing done, so its not changed";
	EXPECT_EQ (ks.at (1).getName (), "default:/ignore/id") << "nothing done, so its not changed";

	i = "other";
	c.activate (i);
	ASSERT_EQ (x.getName (), "default:/other/key");
	ASSERT_EQ (x, 133);
	ASSERT_EQ (ks.lookup ("/other/key").getString (), "133") << "nothing done, so its not changed";
	ASSERT_EQ (ks.size (), 4);

	EXPECT_EQ (ks.at (0).getName (), "user:/ignore/id") << "nothing done, so its not changed";
	EXPECT_EQ (ks.at (0).getString (), "other") << "nothing done, so its not changed";
	EXPECT_EQ (ks.at (1).getName (), "default:/%/key") << "nothing done, so its not changed";
	EXPECT_EQ (ks.at (2).getName (), "default:/ignore/id") << "nothing done, so its not changed";
	EXPECT_EQ (ks.at (3).getName (), "default:/other/key");
}

TEST_F (test_contextual_update, notifyKeySetUpdate)
{
	ASSERT_EQ (ks.size (), 2);
	ks.append (Key ("default:/%/key", KEY_VALUE, "144", KEY_END));
	ASSERT_EQ (ks.size (), 2);
	EXPECT_EQ (ks.at (0).getName (), "default:/%/key");
	EXPECT_EQ (ks.at (1).getName (), "default:/ignore/id");

	c.notifyKeySetUpdate ();
	ASSERT_EQ (x.getName (), "default:/%/key");
	ASSERT_EQ (x, 144) << "reevaluated context, should have found new key";
	ASSERT_EQ (ks.lookup ("/%/key").getString (), "144");
	ASSERT_EQ (ks.size (), 2);
	EXPECT_EQ (ks.at (0).getName (), "default:/%/key");
	EXPECT_EQ (ks.at (1).getName (), "default:/ignore/id");
}

TEST_F (test_contextual_update, notifyAssignKeySetUpdate)
{
	x = 5;
	ASSERT_EQ (ks.size (), 3);
	EXPECT_EQ (ks.at (0).getName (), "user:/%/key");
	EXPECT_EQ (ks.at (0).getString (), "5");
	EXPECT_EQ (ks.at (1).getName (), "default:/%/key");
	EXPECT_EQ (ks.at (1).getString (), "33");
	EXPECT_EQ (ks.at (2).getName (), "default:/ignore/id");

	ks.append (Key ("user:/%/key", KEY_VALUE, "144", KEY_END));

	c.notifyKeySetUpdate ();
	ASSERT_EQ (x.getName (), "user:/%/key");
	ASSERT_EQ (x, 144) << "reevaluated context, should have found new key";
	ASSERT_EQ (ks.lookup ("/%/key").getString (), "144");
	ASSERT_EQ (ks.size (), 3);
	EXPECT_EQ (ks.at (0).getName (), "user:/%/key");
	EXPECT_EQ (ks.at (0).getString (), "144");
	EXPECT_EQ (ks.at (1).getName (), "default:/%/key");
	EXPECT_EQ (ks.at (1).getString (), "33");
	EXPECT_EQ (ks.at (2).getName (), "default:/ignore/id");
}

void printKs (KeySet & ks)
{
	size_t s = ks.size ();
	std::cout << "ASSERT_GE (ks.size (), " << s << ");" << std::endl;
	std::cout << "EXPECT_EQ (ks.size (), " << s << ");" << std::endl;
	for (size_t i = 0; i < s; ++i)
	{
		std::cout << "EXPECT_EQ (ks.at (" << i << ").getName (), \"" << ks.at (i).getName () << "\");" << std::endl;
		std::cout << "EXPECT_EQ (ks.at (" << i << ").getString (), \"" << ks.at (i).getString () << "\");" << std::endl;
	}
}

TEST_F (test_contextual_update, notifyAssignKeySetUpdateLayer)
{
	x = 5;
	i = "other";
	ASSERT_EQ (ks.size (), 4);
	EXPECT_EQ (ks.at (0).getName (), "user:/%/key");
	EXPECT_EQ (ks.at (0).getString (), "5");
	EXPECT_EQ (ks.at (1).getName (), "user:/ignore/id");
	EXPECT_EQ (ks.at (1).getString (), "other");
	EXPECT_EQ (ks.at (2).getName (), "default:/%/key");
	EXPECT_EQ (ks.at (2).getString (), "33");
	EXPECT_EQ (ks.at (3).getName (), "default:/ignore/id");

	ks.append (Key ("user:/other/key", KEY_VALUE, "144", KEY_END));

	const_cast<Key &> (i.getSpec ()).setMeta<std::string> ("order", "#0");
	const_cast<Key &> (x.getSpec ()).setMeta<std::string> ("order", "#1");
	c.notifyKeySetUpdate ();
	EXPECT_EQ (x.getName (), "user:/%/key") << "should be same name";
	EXPECT_EQ (x, 5) << "not activated, thus old value persists";
	EXPECT_EQ (ks.lookup ("/%/key").getString (), "5") << "should get same value";

	ASSERT_GE (ks.size (), 5);
	EXPECT_EQ (ks.size (), 5);
	EXPECT_EQ (ks.at (0).getName (), "user:/%/key");
	EXPECT_EQ (ks.at (0).getString (), "5");
	EXPECT_EQ (ks.at (1).getName (), "user:/ignore/id");
	EXPECT_EQ (ks.at (1).getString (), "other");
	EXPECT_EQ (ks.at (2).getName (), "user:/other/key");
	EXPECT_EQ (ks.at (2).getString (), "144");
	EXPECT_EQ (ks.at (3).getName (), "default:/%/key");
	EXPECT_EQ (ks.at (3).getString (), "33");
	EXPECT_EQ (ks.at (4).getName (), "default:/ignore/id");
	EXPECT_EQ (ks.at (4).getString (), "my");
}

TEST_F (test_contextual_update, notifyAssignKeySetUpdateLayerActivateOrder)
{
	c.activate (i); // activate "my"

	x = 5;
	i = "other";
	ASSERT_GE (ks.size (), 5);
	EXPECT_EQ (ks.size (), 5);
	EXPECT_EQ (ks.at (0).getName (), "user:/ignore/id");
	EXPECT_EQ (ks.at (0).getString (), "other");
	EXPECT_EQ (ks.at (1).getName (), "user:/my/key");
	EXPECT_EQ (ks.at (1).getString (), "5");
	EXPECT_EQ (ks.at (2).getName (), "default:/%/key");
	EXPECT_EQ (ks.at (2).getString (), "33");
	EXPECT_EQ (ks.at (3).getName (), "default:/ignore/id");
	EXPECT_EQ (ks.at (3).getString (), "my");
	EXPECT_EQ (ks.at (4).getName (), "default:/my/key");
	EXPECT_EQ (ks.at (4).getString (), "33");

	ks.append (Key ("user:/other/key", KEY_VALUE, "144", KEY_END));

	const_cast<Key &> (i.getSpec ()).setMeta<std::string> ("layer/order", "#0");
	const_cast<Key &> (x.getSpec ()).setMeta<std::string> ("layer/order", "#1");
	c.notifyKeySetUpdate ();
	EXPECT_EQ (x.getName (), "user:/other/key");
	EXPECT_EQ (x, 144) << "reevaluated context, should have found new key";
	EXPECT_EQ (ks.lookup ("/%/key").getString (), "33");
	ASSERT_GE (ks.size (), 6);
	EXPECT_EQ (ks.size (), 6);
	EXPECT_EQ (ks.at (0).getName (), "user:/ignore/id");
	EXPECT_EQ (ks.at (0).getString (), "other");
	EXPECT_EQ (ks.at (1).getName (), "user:/my/key");
	EXPECT_EQ (ks.at (1).getString (), "5");
	EXPECT_EQ (ks.at (2).getName (), "user:/other/key");
	EXPECT_EQ (ks.at (2).getString (), "144");
	EXPECT_EQ (ks.at (3).getName (), "default:/%/key");
	EXPECT_EQ (ks.at (3).getString (), "33");
	EXPECT_EQ (ks.at (4).getName (), "default:/ignore/id");
	EXPECT_EQ (ks.at (4).getString (), "my");
	EXPECT_EQ (ks.at (5).getName (), "default:/my/key");
	EXPECT_EQ (ks.at (5).getString (), "33");
}

TEST_F (test_contextual_update, notifyAssignKeySetUpdateLayerActivate)
{
	c.activate (i); // activate "my"

	x = 5;
	i = "other";
	ASSERT_GE (ks.size (), 5);
	EXPECT_EQ (ks.size (), 5);
	EXPECT_EQ (ks.at (0).getName (), "user:/ignore/id");
	EXPECT_EQ (ks.at (0).getString (), "other");
	EXPECT_EQ (ks.at (1).getName (), "user:/my/key");
	EXPECT_EQ (ks.at (1).getString (), "5");
	EXPECT_EQ (ks.at (2).getName (), "default:/%/key");
	EXPECT_EQ (ks.at (2).getString (), "33");
	EXPECT_EQ (ks.at (3).getName (), "default:/ignore/id");
	EXPECT_EQ (ks.at (3).getString (), "my");
	EXPECT_EQ (ks.at (4).getName (), "default:/my/key");
	EXPECT_EQ (ks.at (4).getString (), "33");

	ks.append (Key ("user:/other/key", KEY_VALUE, "144", KEY_END));

	c.notifyKeySetUpdate ();
	EXPECT_EQ (x.getName (), "user:/other/key");
	EXPECT_EQ (x, 144) << "reevaluated context, should have found new key";
	EXPECT_EQ (ks.lookup ("/%/key").getString (), "33");
	ASSERT_GE (ks.size (), 6);
	EXPECT_EQ (ks.size (), 6);
	EXPECT_EQ (ks.at (0).getName (), "user:/ignore/id");
	EXPECT_EQ (ks.at (0).getString (), "other");
	EXPECT_EQ (ks.at (1).getName (), "user:/my/key");
	EXPECT_EQ (ks.at (1).getString (), "5");
	EXPECT_EQ (ks.at (2).getName (), "user:/other/key");
	EXPECT_EQ (ks.at (2).getString (), "144");
	EXPECT_EQ (ks.at (3).getName (), "default:/%/key");
	EXPECT_EQ (ks.at (3).getString (), "33");
	EXPECT_EQ (ks.at (4).getName (), "default:/ignore/id");
	EXPECT_EQ (ks.at (4).getString (), "my");
	EXPECT_EQ (ks.at (5).getName (), "default:/my/key");
	EXPECT_EQ (ks.at (5).getString (), "33");
}

TEST_F (test_contextual_update, activateLayersByCV)
{
	EXPECT_EQ (c["doesnotexist"], "");
	EXPECT_EQ (c["id"], "");
	EXPECT_EQ (c["key"], "");
	ASSERT_EQ (x.getName (), "default:/%/key");
	EXPECT_EQ (ks.at (0).getName (), "default:/%/key");
	EXPECT_EQ (ks.at (0).getString (), "33");

	c.activate (x);
	ASSERT_EQ (x.getName (), "default:/%/key");
	EXPECT_EQ (c["key"], "33");
	EXPECT_EQ (c["id"], "");

	c.activate (i);
	ASSERT_EQ (x.getName (), "default:/my/key") << "spec should be changed";
	EXPECT_EQ (c["id"], "my");
	EXPECT_EQ (c["key"], "33") << "must be still 33, not synced";

	x = 5;
	EXPECT_EQ (c["key"], "5") << "direct assignment";
	EXPECT_EQ (c["id"], "my");

	i = "other";
	ASSERT_EQ (x.getName (), "user:/my/key") << "spec should still refer to my";
	EXPECT_EQ (c["id"], "other");
	EXPECT_EQ (c["key"], "5") << "must be still 5, not synced";

	ASSERT_GE (ks.size (), 5);
	EXPECT_EQ (ks.size (), 5);
	EXPECT_EQ (ks.at (0).getName (), "user:/ignore/id");
	EXPECT_EQ (ks.at (0).getString (), "other");
	EXPECT_EQ (ks.at (1).getName (), "user:/my/key");
	EXPECT_EQ (ks.at (1).getString (), "5");
	EXPECT_EQ (ks.at (2).getName (), "default:/%/key");
	EXPECT_EQ (ks.at (2).getString (), "33");
	EXPECT_EQ (ks.at (3).getName (), "default:/ignore/id");
	EXPECT_EQ (ks.at (3).getString (), "my");
	EXPECT_EQ (ks.at (4).getName (), "default:/my/key");
	EXPECT_EQ (ks.at (4).getString (), "33");

	c.sync ();
	ASSERT_EQ (x.getName (), "default:/other/key") << "spec should refer to other after sync";
	EXPECT_EQ (c["id"], "other");
	EXPECT_EQ (c["key"], "33") << "must be still 5, not synced";

	i = "my";
	ASSERT_EQ (x.getName (), "default:/other/key") << "sync pending";
	EXPECT_EQ (c["id"], "my");
	EXPECT_EQ (c["key"], "33");

	c.sync ();
	ASSERT_EQ (x.getName (), "user:/my/key") << "sync done";
	EXPECT_EQ (c["id"], "my");
	EXPECT_EQ (c["key"], "5") << "not synced properly";

	ks.append (Key ("user:/my/key", KEY_VALUE, "99", KEY_END));

	c.sync ();
	ASSERT_EQ (x.getName (), "user:/my/key");
	EXPECT_EQ (c["id"], "my");
	EXPECT_EQ (c["key"], "99") << "not synced properly";


	i = "";
	ASSERT_EQ (x.getName (), "user:/my/key") << "key pending";
	EXPECT_EQ (c["id"], "") << "but layer should be changed";
	EXPECT_EQ (c["key"], "99") << "still 99, not synced";

	c.sync ();
	EXPECT_EQ (c["id"], "");
	EXPECT_EQ (c["key"], "33") << "go back to old value because of sync";

	ASSERT_GE (ks.size (), 6);
	EXPECT_EQ (ks.size (), 6);
	EXPECT_EQ (ks.at (0).getName (), "user:/ignore/id");
	EXPECT_EQ (ks.at (0).getString (), "");
	EXPECT_EQ (ks.at (1).getName (), "user:/my/key");
	EXPECT_EQ (ks.at (1).getString (), "99");
	EXPECT_EQ (ks.at (2).getName (), "default:/%/key");
	EXPECT_EQ (ks.at (2).getString (), "33");
	EXPECT_EQ (ks.at (3).getName (), "default:/ignore/id");
	EXPECT_EQ (ks.at (3).getString (), "my");
	EXPECT_EQ (ks.at (4).getName (), "default:/my/key");
	EXPECT_EQ (ks.at (4).getString (), "33");
	EXPECT_EQ (ks.at (5).getName (), "default:/other/key");
	EXPECT_EQ (ks.at (5).getString (), "33");

	i = "hello";
	EXPECT_EQ (c["id"], "hello");
	EXPECT_EQ (c["key"], "33");

	c.deactivate (i);
	EXPECT_EQ (c["id"], "");
	EXPECT_EQ (c["key"], "33");

	c.deactivate (x);
	EXPECT_EQ (c["id"], "");
	EXPECT_EQ (c["key"], "");

	i = "hi_again";
	EXPECT_EQ (c["id"], "");
	EXPECT_EQ (c["key"], "");
}


TEST_F (test_contextual_update, notifyAssignKeySetUpdateMore)
{
	ThreadValue<std::string> j (
		ks, c, Key ("/%country%/language/code", KEY_META, "layer/name", "language", KEY_META, "default", "my", KEY_END));
	ThreadValue<int> y (ks, c, Key ("/%language%/%id%/key", KEY_META, "default", "55", KEY_END));
	c.activate (j); // activate language layer "my"

	ASSERT_GE (ks.size (), 5);
	EXPECT_EQ (ks.size (), 5);
	EXPECT_EQ (ks.at (0).getName (), "default:/%/%/key");
	EXPECT_EQ (ks.at (0).getString (), "55");
	EXPECT_EQ (ks.at (1).getName (), "default:/%/key");
	EXPECT_EQ (ks.at (1).getString (), "33");
	EXPECT_EQ (ks.at (2).getName (), "default:/%/language/code");
	EXPECT_EQ (ks.at (2).getString (), "my");
	EXPECT_EQ (ks.at (3).getName (), "default:/ignore/id");
	EXPECT_EQ (ks.at (3).getString (), "my");
	EXPECT_EQ (ks.at (4).getName (), "default:/my/%/key");
	EXPECT_EQ (ks.at (4).getString (), "55");

	// now in the database the language changes:
	ks.append (Key ("user:/%/language/code", KEY_VALUE, "de", KEY_END));
	ks.append (Key ("user:/de/%/key", KEY_VALUE, "155", KEY_END));

	c.notifyKeySetUpdate ();
	EXPECT_EQ (j.getName (), "user:/%/language/code");
	EXPECT_EQ (y.getName (), "user:/de/%/key");
	EXPECT_EQ (std::string (j), "de");
	EXPECT_EQ (y, 155);

	ASSERT_GE (ks.size (), 7);
	EXPECT_EQ (ks.size (), 7);
	EXPECT_EQ (ks.at (0).getName (), "user:/%/language/code");
	EXPECT_EQ (ks.at (0).getString (), "de");
	EXPECT_EQ (ks.at (1).getName (), "user:/de/%/key");
	EXPECT_EQ (ks.at (1).getString (), "155");
	EXPECT_EQ (ks.at (2).getName (), "default:/%/%/key");
	EXPECT_EQ (ks.at (2).getString (), "55");
	EXPECT_EQ (ks.at (3).getName (), "default:/%/key");
	EXPECT_EQ (ks.at (3).getString (), "33");
	EXPECT_EQ (ks.at (4).getName (), "default:/%/language/code");
	EXPECT_EQ (ks.at (4).getString (), "my");
	EXPECT_EQ (ks.at (5).getName (), "default:/ignore/id");
	EXPECT_EQ (ks.at (5).getString (), "my");
	EXPECT_EQ (ks.at (6).getName (), "default:/my/%/key");
	EXPECT_EQ (ks.at (6).getString (), "55");
}

TEST_F (test_contextual_update, notifySyncAssign)
{
	ThreadValue<std::string> j (
		ks, c, Key ("/%country%/language/code", KEY_META, "layer/name", "language", KEY_META, "default", "my", KEY_END));
	ThreadValue<int> y (ks, c, Key ("/%language%/%id%/key", KEY_META, "default", "55", KEY_END));
	c.activate (j); // activate language layer with "my"

	ASSERT_GE (ks.size (), 5);
	EXPECT_EQ (ks.size (), 5);
	EXPECT_EQ (ks.at (0).getName (), "default:/%/%/key");
	EXPECT_EQ (ks.at (0).getString (), "55");
	EXPECT_EQ (ks.at (1).getName (), "default:/%/key");
	EXPECT_EQ (ks.at (1).getString (), "33");
	EXPECT_EQ (ks.at (2).getName (), "default:/%/language/code");
	EXPECT_EQ (ks.at (2).getString (), "my");
	EXPECT_EQ (ks.at (3).getName (), "default:/ignore/id");
	EXPECT_EQ (ks.at (3).getString (), "my");
	EXPECT_EQ (ks.at (4).getName (), "default:/my/%/key");
	EXPECT_EQ (ks.at (4).getString (), "55");

	j = "de";
	ks.append (Key ("user:/de/%/key", KEY_VALUE, "155", KEY_END));

	c.sync ();
	EXPECT_EQ (j.getName (), "user:/%/language/code");
	EXPECT_EQ (y.getName (), "user:/de/%/key");
	EXPECT_EQ (std::string (j), "de");
	EXPECT_EQ (y, 155);

	ASSERT_GE (ks.size (), 7);
	EXPECT_EQ (ks.size (), 7);
	EXPECT_EQ (ks.at (0).getName (), "user:/%/language/code");
	EXPECT_EQ (ks.at (0).getString (), "de");
	EXPECT_EQ (ks.at (1).getName (), "user:/de/%/key");
	EXPECT_EQ (ks.at (1).getString (), "155");
	EXPECT_EQ (ks.at (2).getName (), "default:/%/%/key");
	EXPECT_EQ (ks.at (2).getString (), "55");
	EXPECT_EQ (ks.at (3).getName (), "default:/%/key");
	EXPECT_EQ (ks.at (3).getString (), "33");
	EXPECT_EQ (ks.at (4).getName (), "default:/%/language/code");
	EXPECT_EQ (ks.at (4).getString (), "my");
	EXPECT_EQ (ks.at (5).getName (), "default:/ignore/id");
	EXPECT_EQ (ks.at (5).getString (), "my");
	EXPECT_EQ (ks.at (6).getName (), "default:/my/%/key");
	EXPECT_EQ (ks.at (6).getString (), "55");
}

TEST_F (test_contextual_update, notifySyncCycle)
{
	ThreadValue<std::string> j (
		ks, c, Key ("/%country%/%language%/code", KEY_META, "layer/name", "language", KEY_META, "default", "my", KEY_END));
	ThreadValue<int> y (ks, c, Key ("/%language%/%country%/country", KEY_META, "default", "55", KEY_END));
	EXPECT_NO_THROW (c.activate (j)) << "also works with cycle"; // activate language layer with "my"

	ASSERT_GE (ks.size (), 6);
	EXPECT_EQ (ks.size (), 6);
	EXPECT_EQ (ks.at (0).getName (), "default:/%/%/code");
	EXPECT_EQ (ks.at (0).getString (), "my");
	EXPECT_EQ (ks.at (1).getName (), "default:/%/%/country");
	EXPECT_EQ (ks.at (1).getString (), "55");
	EXPECT_EQ (ks.at (2).getName (), "default:/%/key");
	EXPECT_EQ (ks.at (2).getString (), "33");
	EXPECT_EQ (ks.at (3).getName (), "default:/%/my/code");
	EXPECT_EQ (ks.at (3).getString (), "my");
	EXPECT_EQ (ks.at (4).getName (), "default:/ignore/id");
	EXPECT_EQ (ks.at (4).getString (), "my");
	EXPECT_EQ (ks.at (5).getName (), "default:/my/%/country");
	EXPECT_EQ (ks.at (5).getString (), "55");

	// now in the database the language changes:
	ks.append (Key ("user:/%/language/code", KEY_VALUE, "de", KEY_END));
	ks.append (Key ("user:/de/%/key", KEY_VALUE, "155", KEY_END));

	EXPECT_THROW (c.sync (), std::runtime_error);
	EXPECT_EQ (std::string (j), "my");
	EXPECT_EQ (y, 55);

	ASSERT_GE (ks.size (), 8);
	EXPECT_EQ (ks.size (), 8);
	EXPECT_EQ (ks.at (0).getName (), "user:/%/language/code");
	EXPECT_EQ (ks.at (0).getString (), "de");
	EXPECT_EQ (ks.at (1).getName (), "user:/de/%/key");
	EXPECT_EQ (ks.at (1).getString (), "155");
	EXPECT_EQ (ks.at (2).getName (), "default:/%/%/code");
	EXPECT_EQ (ks.at (2).getString (), "my");
	EXPECT_EQ (ks.at (3).getName (), "default:/%/%/country");
	EXPECT_EQ (ks.at (3).getString (), "55");
	EXPECT_EQ (ks.at (4).getName (), "default:/%/key");
	EXPECT_EQ (ks.at (4).getString (), "33");
	EXPECT_EQ (ks.at (5).getName (), "default:/%/my/code");
	EXPECT_EQ (ks.at (5).getString (), "my");
	EXPECT_EQ (ks.at (6).getName (), "default:/ignore/id");
	EXPECT_EQ (ks.at (6).getString (), "my");
	EXPECT_EQ (ks.at (7).getName (), "default:/my/%/country");
	EXPECT_EQ (ks.at (7).getString (), "55");
}
