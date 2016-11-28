/**
 * @file
 *
 * @brief tests for the search service
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <gtest/gtest.h>

#include <config.hpp>
#include <kdb_includes.hpp>
#include <model_entry.hpp>
#include <service.hpp>

/**
 * TESTS for kdbrest::service::SearchEngine
 */

TEST (kdbrestServicesSearchengineTest, FilterConfigurationsByNameCheck)
{

	using namespace kdb;
	using namespace kdbrest::service;

	std::string searchKey = "test/test";
	std::string searchKey2 = "test/test/test3";
	std::string testKey = "test/test/test1/test/test1";
	std::string testKey2 = "test/test/test2/test/test2";
	std::string testKey3 = "test/test/test3/test/test3";

	Key testKeyAbs = Key (kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.configs") + std::string ("/") + testKey,
			      KEY_VALUE, "testvalue", KEY_END);
	Key testKeyAbs2 =
		Key (kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.configs") + std::string ("/") + testKey2,
		     KEY_VALUE, "testvalue", KEY_END);
	Key testKeyAbs3 =
		Key (kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.configs") + std::string ("/") + testKey3,
		     KEY_VALUE, "testvalue", KEY_END);

	std::vector<kdbrest::model::Entry> entries;
	entries.push_back (static_cast<kdbrest::model::Entry> (testKeyAbs));
	entries.push_back (static_cast<kdbrest::model::Entry> (testKeyAbs2));
	entries.push_back (static_cast<kdbrest::model::Entry> (testKeyAbs3));

	SearchEngine::instance ().filterConfigurationsByName (entries, searchKey);
	ASSERT_EQ (entries.size (), 3);
	for (auto & elem : entries)
	{
		ASSERT_TRUE (elem.getName ().compare (testKeyAbs.getName ()) == 0 ||
			     elem.getName ().compare (testKeyAbs2.getName ()) == 0 ||
			     elem.getName ().compare (testKeyAbs3.getName ()) == 0);
	}

	SearchEngine::instance ().filterConfigurationsByName (entries, searchKey2);
	ASSERT_EQ (entries.size (), 1);
	ASSERT_TRUE (entries.at (0).getName ().compare (testKeyAbs3.getName ()) == 0);
}

TEST (kdbrestServicesSearchengineTest, FindConfigurationsByFilterCheck)
{

	using namespace kdb;
	using namespace kdbrest::service;
	using namespace kdbrest::model;

	std::string testKey = "test/test/test3/test2/test1";
	std::string testKeyDummy = "test/dummy/test/dummy/test";

	std::string search_key = "test3/test2/test1";
	std::string search_title = "find the title !!";
	std::string search_description = "find the description.";
	std::string search_author = "find the author";
	std::string search_tag = "find-the-tag";

	Entry testEntry_base = Entry (testKey);
	testEntry_base.setTitle (search_title);
	testEntry_base.setDescription (search_description);
	testEntry_base.setAuthor (search_author);
	auto tags = testEntry_base.getTags ();
	tags.push_back (search_tag);
	testEntry_base.setTags (tags);

	Entry testEntry_findKey = Entry (testEntry_base);
	Entry testEntry_findTitle = Entry (testEntry_base);
	Entry testEntry_findDescription = Entry (testEntry_base);
	Entry testEntry_findAuthor = Entry (testEntry_base);
	Entry testEntry_findTag = Entry (testEntry_base);

	Entry testEntry_dummy = Entry (testKeyDummy);
	Entry testEntry_dummy2 = Entry (testKeyDummy);
	Entry testEntry_dummy3 = Entry (testKeyDummy);
	Entry testEntry_dummy4 = Entry (testKeyDummy);
	Entry testEntry_dummy5 = Entry (testKeyDummy);

	{
		std::vector<Entry> entries;
		entries.push_back (testEntry_findKey);
		entries.push_back (testEntry_dummy);
		entries.push_back (testEntry_dummy2);
		entries.push_back (testEntry_dummy3);
		entries.push_back (testEntry_dummy4);
		entries.push_back (testEntry_dummy5);

		SearchEngine::instance ().findConfigurationsByFilter (entries, search_key);
		ASSERT_EQ (1, entries.size ());
		ASSERT_EQ (entries.at (0).getName (), testEntry_findKey.getName ());
	}

	{
		std::vector<Entry> entries;
		entries.push_back (testEntry_findKey);
		entries.push_back (testEntry_findTitle);
		entries.push_back (testEntry_findDescription);
		entries.push_back (testEntry_dummy);
		entries.push_back (testEntry_dummy2);
		entries.push_back (testEntry_dummy3);
		entries.push_back (testEntry_dummy4);
		entries.push_back (testEntry_dummy5);

		SearchEngine::instance ().findConfigurationsByFilter (entries, search_key, "key");
		ASSERT_EQ (3, entries.size ());
	}

	{
		std::vector<Entry> entries;
		entries.push_back (testEntry_findTitle);
		entries.push_back (testEntry_dummy);
		entries.push_back (testEntry_dummy2);
		entries.push_back (testEntry_dummy3);
		entries.push_back (testEntry_dummy4);
		entries.push_back (testEntry_dummy5);

		SearchEngine::instance ().findConfigurationsByFilter (entries, search_title);
		ASSERT_EQ (1, entries.size ());
		ASSERT_EQ (entries.at (0).getName (), testEntry_findTitle.getName ());
	}

	{
		std::vector<Entry> entries;
		entries.push_back (testEntry_findKey);
		entries.push_back (testEntry_findTitle);
		entries.push_back (testEntry_findDescription);
		entries.push_back (testEntry_findAuthor);
		entries.push_back (testEntry_findTag);
		entries.push_back (testEntry_dummy);
		entries.push_back (testEntry_dummy2);
		entries.push_back (testEntry_dummy3);
		entries.push_back (testEntry_dummy4);
		entries.push_back (testEntry_dummy5);

		SearchEngine::instance ().findConfigurationsByFilter (entries, search_title, "title");
		ASSERT_EQ (5, entries.size ());
		ASSERT_EQ (entries.at (0).getName (), testEntry_findTitle.getName ());
	}

	{
		std::vector<Entry> entries;
		entries.push_back (testEntry_findKey);
		entries.push_back (testEntry_findTitle);
		entries.push_back (testEntry_findDescription);
		entries.push_back (testEntry_findAuthor);
		entries.push_back (testEntry_findTag);
		entries.push_back (testEntry_dummy);
		entries.push_back (testEntry_dummy2);
		entries.push_back (testEntry_dummy3);
		entries.push_back (testEntry_dummy4);
		entries.push_back (testEntry_dummy5);

		SearchEngine::instance ().findConfigurationsByFilter (entries, search_description);
		ASSERT_EQ (5, entries.size ());
		ASSERT_EQ (entries.at (0).getName (), testEntry_findDescription.getName ());
	}

	{
		std::vector<Entry> entries;
		entries.push_back (testEntry_findDescription);
		entries.push_back (testEntry_dummy);
		entries.push_back (testEntry_dummy2);
		entries.push_back (testEntry_dummy3);
		entries.push_back (testEntry_dummy4);
		entries.push_back (testEntry_dummy5);

		SearchEngine::instance ().findConfigurationsByFilter (entries, search_description, "description");
		ASSERT_EQ (1, entries.size ());
		ASSERT_EQ (entries.at (0).getName (), testEntry_findDescription.getName ());
	}

	{
		std::vector<Entry> entries;
		entries.push_back (testEntry_findAuthor);
		entries.push_back (testEntry_dummy);
		entries.push_back (testEntry_dummy2);
		entries.push_back (testEntry_dummy3);
		entries.push_back (testEntry_dummy4);
		entries.push_back (testEntry_dummy5);

		SearchEngine::instance ().findConfigurationsByFilter (entries, search_author);
		ASSERT_EQ (1, entries.size ());
		ASSERT_EQ (entries.at (0).getName (), testEntry_findAuthor.getName ());
	}

	{
		std::vector<Entry> entries;
		entries.push_back (testEntry_findKey);
		entries.push_back (testEntry_findTitle);
		entries.push_back (testEntry_findDescription);
		entries.push_back (testEntry_findAuthor);
		entries.push_back (testEntry_findTag);
		entries.push_back (testEntry_dummy);
		entries.push_back (testEntry_dummy2);
		entries.push_back (testEntry_dummy3);
		entries.push_back (testEntry_dummy4);
		entries.push_back (testEntry_dummy5);

		SearchEngine::instance ().findConfigurationsByFilter (entries, search_author, "author");
		ASSERT_EQ (5, entries.size ());
		ASSERT_EQ (entries.at (0).getName (), testEntry_findAuthor.getName ());
	}

	{
		std::vector<Entry> entries;
		entries.push_back (testEntry_findTag);
		entries.push_back (testEntry_dummy);
		entries.push_back (testEntry_dummy2);
		entries.push_back (testEntry_dummy3);
		entries.push_back (testEntry_dummy4);
		entries.push_back (testEntry_dummy5);

		SearchEngine::instance ().findConfigurationsByFilter (entries, search_tag);
		ASSERT_EQ (1, entries.size ());
		ASSERT_EQ (entries.at (0).getName (), testEntry_findTag.getName ());
	}

	{
		std::vector<Entry> entries;
		entries.push_back (testEntry_findKey);
		entries.push_back (testEntry_findTitle);
		entries.push_back (testEntry_findDescription);
		entries.push_back (testEntry_findAuthor);
		entries.push_back (testEntry_findTag);
		entries.push_back (testEntry_dummy);
		entries.push_back (testEntry_dummy2);
		entries.push_back (testEntry_dummy3);
		entries.push_back (testEntry_dummy4);
		entries.push_back (testEntry_dummy5);

		SearchEngine::instance ().findConfigurationsByFilter (entries, search_tag, "tags");
		ASSERT_EQ (5, entries.size ());
		ASSERT_EQ (entries.at (0).getName (), testEntry_findTag.getName ());
	}

	// filter multiple times
	{
		std::vector<Entry> entries;
		entries.push_back (testEntry_findKey);
		entries.push_back (testEntry_findTitle);
		entries.push_back (testEntry_findDescription);
		entries.push_back (testEntry_findAuthor);
		entries.push_back (testEntry_findTag);
		entries.push_back (testEntry_dummy);
		entries.push_back (testEntry_dummy2);
		entries.push_back (testEntry_dummy3);
		entries.push_back (testEntry_dummy4);
		entries.push_back (testEntry_dummy5);

		SearchEngine::instance ().findConfigurationsByFilter (entries, search_key);
		SearchEngine::instance ().findConfigurationsByFilter (entries, search_title);
		SearchEngine::instance ().findConfigurationsByFilter (entries, search_description);
		SearchEngine::instance ().findConfigurationsByFilter (entries, search_author);
		SearchEngine::instance ().findConfigurationsByFilter (entries, search_tag);
		ASSERT_EQ (5, entries.size ());
	}
}

TEST (kdbrestServicesSearchengineTest, FindUsersByFilterCheck)
{

	using namespace kdb;
	using namespace kdbrest::service;
	using namespace kdbrest::model;

	std::string testUsername = "some-valid-user";
	std::string testUsernameDummy = "another-username";

	std::string search_username = "valid-user";
	std::string search_email = "a-valid-email-address@example.com";

	User testUser_base = User (testUsername);
	testUser_base.setEmail (search_email);

	User testUser_findUsername = User (testUser_base);
	User testUser_findEmail = User (testUser_base);

	User testUser_dummy = User (testUsernameDummy);
	User testUser_dummy2 = User (testUsernameDummy);
	User testUser_dummy3 = User (testUsernameDummy);
	User testUser_dummy4 = User (testUsernameDummy);
	User testUser_dummy5 = User (testUsernameDummy);

	{
		std::vector<User> users;
		users.push_back (testUser_findUsername);
		users.push_back (testUser_dummy);
		users.push_back (testUser_dummy2);
		users.push_back (testUser_dummy3);
		users.push_back (testUser_dummy4);
		users.push_back (testUser_dummy5);

		SearchEngine::instance ().findUsersByFilter (users, search_username);
		ASSERT_EQ (1, users.size ());
		ASSERT_EQ (users.at (0).getName (), testUser_findUsername.getName ());
	}

	{
		std::vector<User> users;
		users.push_back (testUser_findUsername);
		users.push_back (testUser_dummy);
		users.push_back (testUser_dummy2);
		users.push_back (testUser_dummy3);
		users.push_back (testUser_dummy4);
		users.push_back (testUser_dummy5);

		SearchEngine::instance ().findUsersByFilter (users, search_username, "username");
		ASSERT_EQ (1, users.size ());
		ASSERT_EQ (users.at (0).getName (), testUser_findUsername.getName ());
	}

	{
		std::vector<User> users;
		users.push_back (testUser_findEmail);
		users.push_back (testUser_dummy);
		users.push_back (testUser_dummy2);
		users.push_back (testUser_dummy3);
		users.push_back (testUser_dummy4);
		users.push_back (testUser_dummy5);

		SearchEngine::instance ().findUsersByFilter (users, search_email);
		ASSERT_EQ (1, users.size ());
		ASSERT_EQ (users.at (0).getName (), testUser_findEmail.getName ());
	}

	{
		std::vector<User> users;
		users.push_back (testUser_findEmail);
		users.push_back (testUser_dummy);
		users.push_back (testUser_dummy2);
		users.push_back (testUser_dummy3);
		users.push_back (testUser_dummy4);
		users.push_back (testUser_dummy5);

		SearchEngine::instance ().findUsersByFilter (users, search_email, "email");
		ASSERT_EQ (1, users.size ());
		ASSERT_EQ (users.at (0).getName (), testUser_findEmail.getName ());
	}
}

int main (int argc, char * argv[])
{
	testing::InitGoogleTest (&argc, argv);
	cppcms::json::value config = kdbrest::service::ConfigEngine::instance ().loadApplicationConfiguration ();
	(void)kdbrest::Config::instance ().initializeConfiguration (config);
	return RUN_ALL_TESTS ();
}
