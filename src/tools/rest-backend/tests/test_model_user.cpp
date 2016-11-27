/**
 * @file
 *
 * @brief tests for the user model
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <gtest/gtest.h>

#include <config.hpp>
#include <model_user.hpp>
#include <service.hpp>

/**
 * TESTS for kdbrest::models::User
 */

TEST (kdbrestModelsUserTest, ConstructorKeyReferenceCheck)
{

	std::string username = kdbrest::Config::kdb_path_users + std::string ("/") + "user-name";
	kdb::Key userKey (username, KEY_END);
	kdbrest::model::User user (userKey);
	ASSERT_EQ (user.getUsername (), "user-name");
}

TEST (kdbrestModelsUserTest, ConstructorAndGetUsername)
{

	std::string username = "user-name";
	kdbrest::model::User user (username);
	ASSERT_EQ (user.getUsername (), username);
}

TEST (kdbrestModelsUserTest, SetAndGetPasswordHash)
{

	std::string username = "user-name";
	std::string passwordHash = "p455w0rdh45h";
	kdbrest::model::User user (username);

	user.setPasswordHash (passwordHash);
	ASSERT_EQ (user.getPasswordHash (), passwordHash);
}

TEST (kdbrestModelsUserTest, SetAndGetAndCheckEmail)
{

	std::string username = "user-name";
	std::string email = "random@test.org";
	kdbrest::model::User user (username);

	try
	{
		user.getSubkey (ELEKTRA_REST_MODEL_USER_META_EMAIL);
		ASSERT_TRUE (false);
	}
	catch (kdbrest::exception::SubkeyNotFoundException & e)
	{
		ASSERT_TRUE (true);
	}
	user.setEmail (email);
	ASSERT_EQ (user.getEmail (), email);
	ASSERT_TRUE (user.getSubkey (ELEKTRA_REST_MODEL_USER_META_EMAIL));
}

TEST (kdbrestModelsUserTest, SetAndGetRank)
{

	std::string username = "user-name";
	int rank = 2;
	kdbrest::model::User user (username);

	try
	{
		user.getSubkey (ELEKTRA_REST_MODEL_USER_META_RANK);
		ASSERT_TRUE (false);
	}
	catch (kdbrest::exception::SubkeyNotFoundException & e)
	{
		ASSERT_TRUE (true);
	}
	user.setRank (rank);
	ASSERT_EQ (user.getRank (), rank);
	ASSERT_TRUE (user.getSubkey (ELEKTRA_REST_MODEL_USER_META_RANK));
}

TEST (kdbrestModelsUserTest, SetAndGetCreatedat)
{

	std::string username = "user-name";
	long created_at = 12398172;
	kdbrest::model::User user (username);

	try
	{
		user.getSubkey (ELEKTRA_REST_MODEL_USER_META_CREATEDAT);
		ASSERT_TRUE (false);
	}
	catch (kdbrest::exception::SubkeyNotFoundException & e)
	{
		ASSERT_TRUE (true);
	}
	user.setCreatedAt (created_at);
	ASSERT_EQ (user.getCreatedAt (), created_at);
	ASSERT_TRUE (user.getSubkey (ELEKTRA_REST_MODEL_USER_META_CREATEDAT));
}

int main (int argc, char * argv[])
{
	testing::InitGoogleTest (&argc, argv);
	cppcms::json::value config = kdbrest::service::ConfigEngine::instance ().loadApplicationConfiguration ();
	(void)kdbrest::Config::initializeConfiguration (config);
	return RUN_ALL_TESTS ();
}
