/**
 * @file
 *
 * @brief tests for the storage service
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <gtest/gtest.h>

#include <config.hpp>
#include <kdb_includes.hpp>
#include <model_entry.hpp>
#include <model_user.hpp>
#include <service.hpp>

/**
 * TESTS for kdbrest::service::StorageEngine
 */

TEST (kdbrestServicesStorageengineTest, CreateEntryCheck)
{

	using namespace kdb;
	using namespace kdbrest::model;
	using namespace kdbrest::service;

	std::string testKey = "test/test/test/entry1";
	std::string testSubKey1 = kdbrest::Config::kdb_path_configs + std::string ("/") + "test/test/test/entry1/confkey1";
	std::string testSubKey2 = kdbrest::Config::kdb_path_configs + std::string ("/") + "test/test/test/entry1/conf/iguration/key1";

	Key subKey1 (testSubKey1, KEY_END);
	Key subKey2 (testSubKey2, KEY_END);

	Entry testEntry (testKey);
	testEntry.addSubkey (subKey1);
	testEntry.addSubkey (subKey2);
	testEntry.setTitle ("test-title");
	testEntry.setDescription ("test-description");
	testEntry.setAuthor ("test-author");
	auto tags = testEntry.getTags ();
	tags.push_back ("test-tag");
	tags.push_back ("test-tag2");
	testEntry.setTags (tags);

	// ensure entry is not in database
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_configs);
		ks.cut (testEntry);
		kdb.set (ks, kdbrest::Config::kdb_path_configs);
	}

	// do storage
	StorageEngine::instance ().createEntry (testEntry);

	// ensure that entry has been saved
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_configs);
		Key k = ks.lookup (testEntry);
		if (k)
		{
			ASSERT_EQ (k.getName (), testEntry.getName ());
			ASSERT_TRUE (k.hasMeta (ELEKTRA_REST_MODEL_ENTRY_META_AUTHOR));
			ASSERT_TRUE (k.hasMeta (ELEKTRA_REST_MODEL_ENTRY_META_TITLE));
			ASSERT_TRUE (k.hasMeta (ELEKTRA_REST_MODEL_ENTRY_META_DESCRIPTION));
			ASSERT_TRUE (k.hasMeta (ELEKTRA_REST_MODEL_ENTRY_META_TAGS));
			ASSERT_EQ (k.getMeta<std::string> (ELEKTRA_REST_MODEL_ENTRY_META_AUTHOR), testEntry.getAuthor ());
			ASSERT_EQ (k.getMeta<std::string> (ELEKTRA_REST_MODEL_ENTRY_META_TITLE), testEntry.getTitle ());
			ASSERT_EQ (k.getMeta<std::string> (ELEKTRA_REST_MODEL_ENTRY_META_DESCRIPTION), testEntry.getDescription ());
			ASSERT_EQ (k.getMeta<std::string> (ELEKTRA_REST_MODEL_ENTRY_META_TAGS),
				   testEntry.getTags ().at (0) + " " + testEntry.getTags ().at (1));
			ASSERT_TRUE (ks.lookup (subKey1));
			ASSERT_TRUE (ks.lookup (subKey2));
		}
		else
		{
			ASSERT_TRUE (false); // force error
		}
	}

	// delete entry after test
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_configs);
		ks.cut (testEntry);
		kdb.set (ks, kdbrest::Config::kdb_path_configs);
	}
}

TEST (kdbrestServicesStorageengineTest, UpdateEntryCheck)
{

	using namespace kdb;
	using namespace kdbrest::exception;
	using namespace kdbrest::model;
	using namespace kdbrest::service;

	std::string testKey = "test/test/test/entry1";
	std::string testSubKey1 = kdbrest::Config::kdb_path_configs + std::string ("/") + "test/test/test/entry1/confkey1";
	std::string testSubKey2 = kdbrest::Config::kdb_path_configs + std::string ("/") + "test/test/test/entry1/conf/iguration/key1";

	std::string testSubKeyNew1 = kdbrest::Config::kdb_path_configs + std::string ("/") + "test/test/test/entry1/configuration/k1";
	std::string testSubKeyNew2 =
		kdbrest::Config::kdb_path_configs + std::string ("/") + "test/test/test/entry1/co/nf/ig/u/ra/ti/on/key1";

	Key subKey1 (testSubKey1, KEY_END);
	Key subKey2 (testSubKey2, KEY_END);

	Key newSubKey1 (testSubKeyNew1, KEY_END);
	Key newSubKey2 (testSubKeyNew2, KEY_END);

	Entry testEntry (testKey);
	testEntry.addSubkey (subKey1);
	testEntry.addSubkey (subKey2);
	testEntry.setTitle ("test-title");
	testEntry.setAuthor ("test-author");

	// ensure entry is not in database
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_configs);
		ks.cut (testEntry);
		kdb.set (ks, kdbrest::Config::kdb_path_configs);
	}

	// create entry
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_configs);
		ks.append (testEntry);
		ks.append (testEntry.getSubkeys ());
		kdb.set (ks, kdbrest::Config::kdb_path_configs);
	}

	// ensure that entry has been saved
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_configs);
		Key k = ks.lookup (testEntry);
		if (!k)
		{
			ASSERT_TRUE (false); // force error
		}
	}

	// force re-fetching of entry cache
	(void)StorageEngine::instance ().getAllEntriesRef (true);

	// update the entry
	testEntry.setTitle ("new-test-title");
	testEntry.setAuthor ("new-test-author");
	testEntry.getSubkeys ().clear ();
	ASSERT_EQ (testEntry.getSubkeys ().size (), 0);
	testEntry.addSubkey (newSubKey1);
	testEntry.addSubkey (newSubKey2);

	try
	{
		StorageEngine::instance ().updateEntry (testEntry);
	}
	catch (EntryNotFoundException & e)
	{
		ASSERT_TRUE (false);
	}

	// ensure that entry has been updated
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_configs);
		Key k = ks.lookup (testEntry);
		if (k)
		{
			ASSERT_EQ (k.getName (), testEntry.getName ());
			ASSERT_TRUE (k.hasMeta (ELEKTRA_REST_MODEL_ENTRY_META_TITLE));
			ASSERT_TRUE (k.hasMeta (ELEKTRA_REST_MODEL_ENTRY_META_AUTHOR));
			ASSERT_EQ (k.getMeta<std::string> (ELEKTRA_REST_MODEL_ENTRY_META_TITLE), testEntry.getTitle ());
			ASSERT_EQ (k.getMeta<std::string> (ELEKTRA_REST_MODEL_ENTRY_META_AUTHOR), testEntry.getAuthor ());
			ASSERT_TRUE (ks.lookup (newSubKey1));
			ASSERT_TRUE (ks.lookup (newSubKey2));
		}
		else
		{
			ASSERT_TRUE (false); // force error
		}
	}

	// delete entry after test
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_configs);
		ks.cut (testEntry);
		kdb.set (ks, kdbrest::Config::kdb_path_configs);
	}
}

TEST (kdbrestServicesStorageengineTest, DeleteEntryCheck)
{

	using namespace kdb;
	using namespace kdbrest::model;
	using namespace kdbrest::service;

	std::string testKey = "test/test/test/entry1";
	std::string testSubKey1 = kdbrest::Config::kdb_path_configs + std::string ("/") + "test/test/test/entry1/confkey1";
	std::string testSubKey2 = kdbrest::Config::kdb_path_configs + std::string ("/") + "test/test/test/entry1/conf/iguration/key1";

	Key subKey1 (testSubKey1, KEY_END);
	Key subKey2 (testSubKey2, KEY_END);

	Entry testEntry (testKey);
	testEntry.addSubkey (subKey1);
	testEntry.addSubkey (subKey2);
	testEntry.setAuthor ("test-author");

	// ensure entry is not in database
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_configs);
		ks.cut (testEntry);
		kdb.set (ks, kdbrest::Config::kdb_path_configs);
	}

	// force re-fetching of entry cache
	(void)StorageEngine::instance ().getAllEntriesRef (true);

	// do storage
	StorageEngine::instance ().createEntry (testEntry);

	// ensure that entry has been saved
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_configs);
		Key k = ks.lookup (testEntry);
		if (k)
		{
			ASSERT_EQ (k.getName (), testEntry.getName ());
			ASSERT_TRUE (k.hasMeta (ELEKTRA_REST_MODEL_ENTRY_META_AUTHOR));
			ASSERT_EQ (k.getMeta<std::string> (ELEKTRA_REST_MODEL_ENTRY_META_AUTHOR), testEntry.getAuthor ());
			ASSERT_TRUE (ks.lookup (subKey1));
			ASSERT_TRUE (ks.lookup (subKey2));
		}
		else
		{
			ASSERT_TRUE (false); // force error
		}
	}

	// delete entry by storage engine
	StorageEngine::instance ().deleteEntry (testEntry);

	// ensure entry is not in database
	ASSERT_FALSE (StorageEngine::instance ().entryExists (testEntry.getPublicName ()));
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_configs);
		ks.cut (testEntry);
		kdb.set (ks, kdbrest::Config::kdb_path_configs);
	}
}

TEST (kdbrestServicesStorageengineTest, EntryExistsCheck)
{

	using namespace kdb;
	using namespace kdbrest::service;

	std::string testKey = "test/test/test/entry1";
	Key testKeyAbs = Key (kdbrest::Config::kdb_path_configs + std::string ("/") + testKey, KEY_VALUE, "testvalue", KEY_END);

	// ensure entry is not in database
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_configs);
		ks.cut (testKeyAbs);
		kdb.set (ks, kdbrest::Config::kdb_path_configs);
	}

	// create key
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_configs);
		ks.append (testKeyAbs);
		kdb.set (ks, kdbrest::Config::kdb_path_configs);
	}

	// make sure that entry cache is re-fetched
	(void)StorageEngine::instance ().getAllEntriesRef (true);

	// check if exists now
	ASSERT_TRUE (StorageEngine::instance ().entryExists (testKey));

	// delete test key
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_configs);
		ks.lookup (testKeyAbs, KDB_O_POP);
		kdb.set (ks, kdbrest::Config::kdb_path_configs);
	}
}

TEST (kdbrestServicesStorageengineTest, GetEntryCheck)
{

	using namespace kdb;
	using namespace kdbrest::service;

	std::string testKey = "test/test/test/entry1";
	Key testKeyAbs = Key (kdbrest::Config::kdb_path_configs + std::string ("/") + testKey, KEY_VALUE, "testvalue", KEY_END);

	// create key
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_configs);
		ks.append (testKeyAbs);
		kdb.set (ks, kdbrest::Config::kdb_path_configs);
	}

	// make sure that entry cache is re-fetched
	(void)StorageEngine::instance ().getAllEntriesRef (true);

	// find entry, check function
	try
	{
		kdbrest::model::Entry entry = StorageEngine::instance ().getEntry (testKey);
		ASSERT_TRUE (entry.isValid ());
		ASSERT_EQ (entry.getName (), testKeyAbs.getName ());
		ASSERT_EQ (entry.get<std::string> (), testKeyAbs.get<std::string> ());
	}
	catch (kdbrest::exception::EntryNotFoundException & e)
	{
		ASSERT_TRUE (false);
	}

	// delete test key
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_configs);
		ks.lookup (testKeyAbs, KDB_O_POP);
		kdb.set (ks, kdbrest::Config::kdb_path_configs);
	}
}

TEST (kdbrestServicesStorageengineTest, CreateUserCheck)
{

	using namespace kdb;
	using namespace kdbrest::model;
	using namespace kdbrest::service;

	std::string username = "test-username";
	std::string passwordHash = "p455w0rdh45h";
	std::string email = "random@email.org";
	int rank = 2;
	long created_at = 123872923;

	User testUser (username);
	testUser.setPasswordHash (passwordHash);
	testUser.setEmail (email);
	testUser.setRank (rank);
	testUser.setCreatedAt (created_at);

	// ensure entry is not in database
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_users);
		ks.cut (testUser);
		kdb.set (ks, kdbrest::Config::kdb_path_users);
	}

	// do storage
	StorageEngine::instance ().createUser (testUser);

	// ensure that entry has been saved
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_users);
		Key k = ks.lookup (testUser);
		if (k)
		{
			ASSERT_EQ (k.getName (), testUser.getName ());
			ASSERT_EQ (k.get<std::string> (), passwordHash);
			ASSERT_TRUE (ks.lookup (testUser.getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_EMAIL));
			ASSERT_TRUE (ks.lookup (testUser.getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_RANK));
			ASSERT_TRUE (ks.lookup (testUser.getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_CREATEDAT));
			ASSERT_EQ (ks.lookup (testUser.getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_EMAIL)
					   .get<std::string> (),
				   email);
			ASSERT_EQ (ks.lookup (testUser.getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_RANK).get<int> (),
				   rank);
			ASSERT_EQ (
				ks.lookup (testUser.getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_CREATEDAT).get<long> (),
				created_at);
		}
		else
		{
			ASSERT_TRUE (false); // force error
		}
	}

	// delete entry after test
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_users);
		ks.cut (testUser);
		kdb.set (ks, kdbrest::Config::kdb_path_users);
	}
}

TEST (kdbrestServicesStorageengineTest, UpdateUserCheck)
{

	using namespace kdb;
	using namespace kdbrest::model;
	using namespace kdbrest::service;

	std::string username = "test-username";
	std::string passwordHash = "p455w0rdh45h";
	std::string email = "random@email.org";
	int rank = 2;
	long created_at = 123872923;

	User testUser (username);
	testUser.setPasswordHash (passwordHash);
	testUser.setEmail (email);
	testUser.setRank (rank);
	testUser.setCreatedAt (created_at);

	// ensure entry is not in database
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_users);
		ks.cut (testUser);
		kdb.set (ks, kdbrest::Config::kdb_path_users);
	}

	// force re-fetching of user cache
	(void)StorageEngine::instance ().getAllUsersRef (true);

	// do storage
	StorageEngine::instance ().createUser (testUser);

	// ensure that entry has been saved
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_users);
		Key k = ks.lookup (testUser);
		if (!k)
		{
			ASSERT_TRUE (false); // force error
		}
	}

	// change user data
	std::string newPasswordHash = "alskdjgasdlkj";
	std::string newEmail = "new@email.org";
	int newRank = 1;
	long newCreated_at = 1238799237;

	testUser.setPasswordHash (newPasswordHash);
	testUser.setEmail (newEmail);
	testUser.setRank (newRank);
	testUser.setCreatedAt (newCreated_at);

	StorageEngine::instance ().updateUser (testUser);

	// check that update has been successful
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_users);
		Key k = ks.lookup (testUser);
		if (k)
		{
			ASSERT_EQ (k.getName (), testUser.getName ());
			ASSERT_EQ (k.get<std::string> (), newPasswordHash);
			ASSERT_TRUE (ks.lookup (testUser.getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_EMAIL));
			ASSERT_TRUE (ks.lookup (testUser.getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_RANK));
			ASSERT_TRUE (ks.lookup (testUser.getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_CREATEDAT));
			ASSERT_EQ (ks.lookup (testUser.getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_EMAIL)
					   .get<std::string> (),
				   newEmail);
			ASSERT_EQ (ks.lookup (testUser.getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_RANK).get<int> (),
				   newRank);
			ASSERT_EQ (
				ks.lookup (testUser.getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_CREATEDAT).get<long> (),
				newCreated_at);
		}
		else
		{
			ASSERT_TRUE (false); // force error
		}
	}

	// delete entry after test
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_users);
		ks.cut (testUser);
		kdb.set (ks, kdbrest::Config::kdb_path_users);
	}
}

TEST (kdbrestServicesStorageengineTest, DeleteUserCheck)
{

	using namespace kdb;
	using namespace kdbrest::model;
	using namespace kdbrest::service;

	std::string username = "test-username";
	std::string passwordHash = "p455w0rdh45h";
	std::string email = "random@email.org";
	int rank = 2;
	long created_at = 123872923;

	User testUser (username);
	testUser.setPasswordHash (passwordHash);
	testUser.setEmail (email);
	testUser.setRank (rank);
	testUser.setCreatedAt (created_at);

	// ensure entry is not in database
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_users);
		ks.cut (testUser);
		kdb.set (ks, kdbrest::Config::kdb_path_users);
	}

	// force re-fetching of user cache
	(void)StorageEngine::instance ().getAllUsersRef (true);

	// do storage
	StorageEngine::instance ().createUser (testUser);

	// ensure that entry has been saved
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_users);
		Key k = ks.lookup (testUser);
		if (k)
		{
			ASSERT_EQ (k.getName (), testUser.getName ());
			ASSERT_EQ (k.get<std::string> (), passwordHash);
			ASSERT_TRUE (ks.lookup (testUser.getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_EMAIL));
			ASSERT_TRUE (ks.lookup (testUser.getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_RANK));
			ASSERT_TRUE (ks.lookup (testUser.getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_CREATEDAT));
			ASSERT_EQ (ks.lookup (testUser.getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_EMAIL)
					   .get<std::string> (),
				   email);
			ASSERT_EQ (ks.lookup (testUser.getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_RANK).get<int> (),
				   rank);
			ASSERT_EQ (
				ks.lookup (testUser.getName () + std::string ("/") + ELEKTRA_REST_MODEL_USER_META_CREATEDAT).get<long> (),
				created_at);
		}
		else
		{
			ASSERT_TRUE (false); // force error
		}
	}

	// delete user by storage engine
	StorageEngine::instance ().deleteUser (testUser);

	// ensure entry is not in database
	ASSERT_FALSE (StorageEngine::instance ().userExists (testUser.getUsername ()));
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_users);
		ks.cut (testUser);
		kdb.set (ks, kdbrest::Config::kdb_path_users);
	}
}

TEST (kdbrestServicesStorageengineTest, UserExistsCheck)
{

	using namespace kdb;
	using namespace kdbrest::service;

	std::string username = "test-username";
	kdbrest::model::User user (username);

	// ensure entry is not in database
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_users);
		ks.cut (user);
		kdb.set (ks, kdbrest::Config::kdb_path_users);
	}

	// force re-fetching of user cache
	(void)StorageEngine::instance ().getAllUsersRef (true);

	// check exists before creating key
	ASSERT_FALSE (StorageEngine::instance ().userExists (username));

	// create key
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_users);
		ks.append (user);
		kdb.set (ks, kdbrest::Config::kdb_path_users);
	}

	// force re-fetching of user cache
	(void)StorageEngine::instance ().getAllUsersRef (true);

	// check if exists now
	ASSERT_TRUE (StorageEngine::instance ().userExists (username));

	// delete test key
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_users);
		ks.cut (user);
		kdb.set (ks, kdbrest::Config::kdb_path_users);
	}
}

TEST (kdbrestServicesStorageengineTest, GetUserCheck)
{

	using namespace kdb;
	using namespace kdbrest::service;

	std::string username = "test-username";
	std::string passwordHash = "p455w0rdh45h";
	std::string email = "random@email.org";
	int rank = 2;
	long created_at = 123872923;

	kdbrest::model::User user (username);
	user.setPasswordHash (passwordHash);
	user.setEmail (email);
	user.setRank (rank);
	user.setCreatedAt (created_at);

	// ensure entry is not in database
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_users);
		ks.cut (user);
		kdb.set (ks, kdbrest::Config::kdb_path_users);
	}

	// force re-fetching of user cache
	(void)StorageEngine::instance ().getAllUsersRef (true);

	// check exists before creating key
	ASSERT_FALSE (StorageEngine::instance ().userExists (username));

	// create key
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_users);
		ks.append (user);
		ks.append (user.getSubkeys ());
		kdb.set (ks, kdbrest::Config::kdb_path_users);
	}

	// force re-fetching of user cache
	(void)StorageEngine::instance ().getAllUsersRef (true);

	// check if exists now
	ASSERT_TRUE (StorageEngine::instance ().userExists (username));

	// find entry, check function
	kdbrest::model::User findUser = StorageEngine::instance ().getUser (username);
	ASSERT_TRUE (findUser.isValid ());
	ASSERT_EQ (findUser.getName (), user.getName ());
	ASSERT_EQ (findUser.getPasswordHash (), passwordHash);
	ASSERT_EQ (findUser.getEmail (), email);
	ASSERT_EQ (findUser.getRank (), rank);
	ASSERT_EQ (findUser.getCreatedAt (), created_at);

	// delete test key
	{
		KDB kdb;
		KeySet ks;
		kdb.get (ks, kdbrest::Config::kdb_path_users);
		ks.cut (user);
		kdb.set (ks, kdbrest::Config::kdb_path_users);
	}
}

int main (int argc, char * argv[])
{
	testing::InitGoogleTest (&argc, argv);

	// initialize test config
	cppcms::json::value config = kdbrest::service::ConfigEngine::instance ().loadApplicationConfiguration ();
	(void)kdbrest::Config::initializeConfiguration (config);
	// force default config
	kdbrest::Config::kdb_path_configs = std::string (ELEKTRA_REST_DEFAULT_PATH_CONFIGS);
	kdbrest::Config::kdb_path_users = std::string (ELEKTRA_REST_DEFAULT_PATH_USERS);

	return RUN_ALL_TESTS ();
}
