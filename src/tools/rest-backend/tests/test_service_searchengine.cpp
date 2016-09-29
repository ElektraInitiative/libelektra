#include <gtest/gtest.h>

#include "../kdb_includes.hpp"
#include "../model_entry.hpp"
#include "../service.hpp"


/**
 * TESTS for kdbrest::service::SearchEngine
 */

TEST (kdbrestServicesSearchengineTest, FilterByNameCheck)
{

	using namespace kdb;
	using namespace kdbrest::service;

	std::string searchKey = "test/test";
	std::string searchKey2 = "test/test/test3";
	std::string testKey = "test/test/test1/test/test1";
	std::string testKey2 = "test/test/test2/test/test2";
	std::string testKey3 = "test/test/test3/test/test3";

	Key testKeyAbs = Key (ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/") + testKey, KEY_VALUE, "testvalue", KEY_END);
	Key testKeyAbs2 = Key (ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/") + testKey2, KEY_VALUE, "testvalue", KEY_END);
	Key testKeyAbs3 = Key (ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/") + testKey3, KEY_VALUE, "testvalue", KEY_END);

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

TEST (kdbrestServicesSearchengineTest, FilterBySearchStringCheck)
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
		entries.push_back (testEntry_findDescription);
		entries.push_back (testEntry_dummy);
		entries.push_back (testEntry_dummy2);
		entries.push_back (testEntry_dummy3);
		entries.push_back (testEntry_dummy4);
		entries.push_back (testEntry_dummy5);

		SearchEngine::instance ().findConfigurationsByFilter (entries, search_description);
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
