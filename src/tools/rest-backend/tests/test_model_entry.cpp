/**
 * @file
 *
 * @brief tests for the entry model
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <gtest/gtest.h>

#include <model_entry.hpp>

/**
 *  TESTS for kdbrest::models::Entry
 */

TEST (kdbrestModelsEntryTest, ConstructorByKeyReference)
{
	kdb::Key key ("proc/test/newkey1");
	kdbrest::model::Entry entry (key);
	ASSERT_EQ (entry.getName (), key.getName ());
}

TEST (kdbrestModelsEntryTest, GetPublicNameErasing)
{
	kdb::Key key (ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/test/key1"));
	kdbrest::model::Entry entry (key);
	ASSERT_NE (entry.getPublicName (), entry.getName ());
	ASSERT_NE (entry.getPublicName (), key.getName ());

	std::string entryName2 ("test/key1");
	kdbrest::model::Entry entry2 (entryName2);
	ASSERT_NE (entry2.getPublicName (), entry2.getName ());
}

TEST (kdbrestModelsEntryTest, GetPublicNamePartsCheck)
{
	kdb::Key key (ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/test/test2/test3/test4"));
	kdbrest::model::Entry entry (key);

	ASSERT_EQ ("test", entry.getOrganization ());
	ASSERT_EQ ("test2", entry.getApplication ());
	ASSERT_EQ ("test3", entry.getScope ());
	ASSERT_EQ ("test4", entry.getSlug ());

	std::vector<std::string> publicNameParts = entry.getPublicNameParts ();

	ASSERT_EQ ("test", publicNameParts.at (0));
	ASSERT_EQ ("test2", publicNameParts.at (1));
	ASSERT_EQ ("test3", publicNameParts.at (2));
	ASSERT_EQ ("test4", publicNameParts.at (3));
}

TEST (kdbrestModelsEntryTest, ConstructorByString)
{
	std::string keyStr ("test/key1");
	kdbrest::model::Entry entry (keyStr);
	ASSERT_EQ (entry.getName (), ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/") + keyStr);
	ASSERT_NE (entry.getName (), keyStr);
	ASSERT_EQ (entry.getPublicName (), keyStr);
}

TEST (kdbrestModelsEntryTest, ConstructorByParts)
{
	std::string organization ("the-organization");
	std::string application ("some-application");
	std::string scope ("app-scope-here");
	std::string slug ("and-finally-a-unique-slug");

	kdbrest::model::Entry entry (organization, application, scope, slug);

	ASSERT_EQ (entry.getName (), ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/") + organization + std::string ("/") +
					     application + std::string ("/") + scope + std::string ("/") + slug);
	ASSERT_NE (entry.getName (), organization + std::string ("/") + application + std::string ("/") + scope + std::string ("/") + slug);
	ASSERT_EQ (entry.getPublicName (),
		   organization + std::string ("/") + application + std::string ("/") + scope + std::string ("/") + slug);
}

TEST (kdbrestModelsEntryTest, GetNameCorrectPrefix)
{

	kdb::Key key (ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/test/key1"));
	kdbrest::model::Entry entry (key);
	ASSERT_EQ (entry.getPublicName (), "test/key1");
}

TEST (kdbrestModelsEntryTest, SetAndGetAuthor)
{

	kdb::Key key (ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/test/key1"));
	kdbrest::model::Entry entry (key);
	std::string author ("the author");
	entry.setAuthor (author);
	ASSERT_EQ (author, entry.getAuthor ());
}

TEST (kdbrestModelsEntryTest, SetAndGetCreatedAt)
{

	kdb::Key key (ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/test/key1"));
	kdbrest::model::Entry entry (key);
	long created_at = 128372904;
	entry.setCreatedAt (created_at);
	ASSERT_EQ (created_at, entry.getCreatedAt ());
}

TEST (kdbrestModelsEntryTest, SetAndGetTitle)
{

	kdb::Key key (ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/test/key1"));
	kdbrest::model::Entry entry (key);
	std::string title ("the title of this entry");
	entry.setTitle (title);
	ASSERT_EQ (title, entry.getTitle ());
}

TEST (kdbrestModelsEntryTest, SetAndGetDescription)
{

	kdb::Key key (ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/test/key1"));
	kdbrest::model::Entry entry (key);
	std::string desc ("a beautiful, but still somewhat random description of this entry");
	entry.setDescription (desc);
	ASSERT_EQ (desc, entry.getDescription ());
}

TEST (kdbrestModelsEntryTest, SetAndGetUploadPlugin)
{

	kdb::Key key (ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/test/key1"));
	kdbrest::model::Entry entry (key);
	std::string plugin ("simpleini");
	entry.setUploadPlugin (plugin);
	ASSERT_EQ (plugin, entry.getUploadPlugin ());
}

TEST (kdbrestModelsEntryTest, SetAndGetViews)
{

	kdb::Key key (ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/test/key1"));
	kdbrest::model::Entry entry (key);
	long views = 823712903;
	entry.setViews (views);
	ASSERT_EQ (views, entry.getViews ());
	views += 123;
	entry.addViews (123);
	ASSERT_EQ (views, entry.getViews ());
}

TEST (kdbrestModelsEntryTest, SetAndGetAndHasTags)
{

	kdb::Key key (ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/test/key1"));
	kdbrest::model::Entry entry (key);
	const char * taglist[] = { "one", "two", "three", "four", "five" };
	std::vector<std::string> tags (taglist, std::end (taglist));
	entry.setTags (tags);

	std::vector<std::string> retTags = entry.getTags ();

	for (int i = 0; i < 5; i++)
	{
		ASSERT_TRUE (std::find (retTags.begin (), retTags.end (), taglist[i]) != retTags.end ());
	}

	std::string one (taglist[0]);
	std::string three (taglist[2]);
	ASSERT_TRUE (entry.hasTag (one));
	ASSERT_TRUE (entry.hasTag (three));

	std::string seven ("seven");
	std::string fou ("fou");
	std::string five_uc ("FIVE");
	ASSERT_FALSE (entry.hasTag (seven));
	ASSERT_FALSE (entry.hasTag (fou));
	ASSERT_FALSE (entry.hasTag (five_uc));
}

TEST (kdbrestModelsEntryTest, SetAndGetAndAddSubkeys)
{

	kdb::Key key (ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/test/key1"));
	kdb::Key key2 (ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/test/key1/subkey1"));
	kdb::Key key3 (ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/test/key1/subkey1/subkey2"));
	kdb::Key key4 (ELEKTRA_REST_CONFIG_REPOSITORY_PATH + std::string ("/test/key2/subkey1"));

	kdbrest::model::Entry entry (key);

	entry.addSubkey (key2);
	ASSERT_EQ (entry.getSubkeys ().size (), 1);
	entry.addSubkey (key3);
	ASSERT_EQ (entry.getSubkeys ().size (), 2);
	entry.addSubkey (key4);
	ASSERT_EQ (entry.getSubkeys ().size (), 2);

	ASSERT_TRUE (entry.getSubkeys ().lookup (key2));
	ASSERT_TRUE (entry.getSubkeys ().lookup (key3));

	kdb::KeySet ks;
	ks.append (key2);
	ks.append (key3);
	ks.append (key4);
	kdbrest::model::Entry entry2 (key);

	entry2.addSubkeys (ks.begin (), ks.end ());
	ASSERT_EQ (entry2.getSubkeys ().size (), 2);

	ASSERT_TRUE (entry2.getSubkeys ().lookup (key2));
	ASSERT_TRUE (entry2.getSubkeys ().lookup (key3));
}
