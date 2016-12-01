/**
 * @file
 *
 * @brief tests for the configformat model
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <regex>

#include <gtest/gtest.h>

#include <service.hpp>

/**
 * TESTS for kdbrest::models::ConfigFormat
 */

TEST (kdbrestServiceConfigengineTest, ArrayKeyRegexCheck)
{

	std::regex regex (kdbrest::service::REGEX_CHECK_KEY_IS_ARRAY);

	ASSERT_FALSE (std::regex_match ("firstpart", regex));
	ASSERT_FALSE (std::regex_match ("firstpart.second", regex));
	ASSERT_FALSE (std::regex_match ("firstpart.second.third", regex));

	ASSERT_TRUE (std::regex_match ("#0", regex));
	ASSERT_TRUE (std::regex_match ("#5", regex));
	ASSERT_TRUE (std::regex_match ("#_12", regex));
	ASSERT_TRUE (std::regex_match ("#_2038", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.#0", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.#_12", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.second.third.#0", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.second.third.#_12", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.second.third.#0.with.trailing.path", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.second.third.#_12.with.trailing.path", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.second.third.#_12.with.trailing.array.#0", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.second.third.#_12.with.trailing.array.#_12", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.second.third.#_12.with.trailing.array.#0.and.path", regex));
	ASSERT_TRUE (std::regex_match ("firstpart.second.third.#_12.with.trailing.array.#_12.and.path", regex));
}

TEST (kdbrestServiceConfigengineTest, TransformKeysetToJsonValue)
{

	std::string conf_root = "/sw/elektra/restbackend/#0/current";

	kdb::KeySet testKeySet (15, *kdb::Key (conf_root + "/nested/object/var", KEY_VALUE, "simple string", KEY_END),
				*kdb::Key (conf_root + "/nested/object/var2", KEY_VALUE, "12345", KEY_END),
				*kdb::Key (conf_root + "/nested/object/var3", KEY_VALUE, "1", KEY_END),
				*kdb::Key (conf_root + "/nested/object/var4", KEY_VALUE, "0", KEY_END),
				*kdb::Key (conf_root + "/object/array/#0", KEY_VALUE, "first element", KEY_END),
				*kdb::Key (conf_root + "/object/array/#1", KEY_VALUE, "second element", KEY_END),
				*kdb::Key (conf_root + "/object/array2/#0/var", KEY_VALUE, "first var", KEY_END),
				*kdb::Key (conf_root + "/object/array2/#0/var2", KEY_VALUE, "second var", KEY_END),
				*kdb::Key (conf_root + "/object/array2/#0/var3", KEY_VALUE, "third var", KEY_END),
				*kdb::Key (conf_root + "/object/array2/#1/var", KEY_VALUE, "first var", KEY_END),
				*kdb::Key (conf_root + "/object/array2/#2/var", KEY_VALUE, "first var", KEY_END),
				*kdb::Key (conf_root + "/object/array2/#2/var2", KEY_VALUE, "54321", KEY_END),
				*kdb::Key (conf_root + "/object/array3/#0/var/#0", KEY_VALUE, "arr0-in-arr0", KEY_END),
				*kdb::Key (conf_root + "/object/array3/#0/var/#1", KEY_VALUE, "arr1-in-arr0", KEY_END),
				*kdb::Key (conf_root + "/object/array3/#1/var/#0", KEY_VALUE, "arr0-in-arr1", KEY_END), KS_END);

	cppcms::json::value expected;
	expected["nested"]["object"]["var"] = "simple string";
	expected["nested"]["object"]["var2"] = 12345;
	expected["nested"]["object"]["var3"] = true;
	expected["nested"]["object"]["var4"] = false;
	expected["object"]["array"][0] = "first element";
	expected["object"]["array"][1] = "second element";
	expected["object"]["array2"][0]["var"] = "first var";
	expected["object"]["array2"][0]["var2"] = "second var";
	expected["object"]["array2"][0]["var3"] = "third var";
	expected["object"]["array2"][1]["var"] = "first var";
	expected["object"]["array2"][2]["var"] = "first var";
	expected["object"]["array2"][2]["var2"] = 54321;
	expected["object"]["array3"][0]["var"][0] = "arr0-in-arr0";
	expected["object"]["array3"][0]["var"][1] = "arr1-in-arr0";
	expected["object"]["array3"][1]["var"][0] = "arr0-in-arr1";

	cppcms::json::value result = kdbrest::service::ConfigEngine::instance ().transformKeysetToJsonValue (testKeySet, conf_root);

	ASSERT_TRUE (result == expected);
}

TEST (kdbrestServiceConfigengineTest, TransformKeysetToJsonValue2)
{

	std::string conf_root = "system/sw/elektra/restbackend/#0/current";
	std::string conf_root_cascading = "/sw/elektra/restbackend/#0/current";

	kdb::KeySet testKeySet (
		23, *kdb::Key (conf_root + "/backend/api/description/html", KEY_VALUE, "https://libelektra.org", KEY_END),
		*kdb::Key (conf_root + "/backend/api/description/raw", KEY_VALUE,
			   "https://raw.githubusercontent.com/ElektraInitiative/libelektra/master/doc/api_blueprints/snippet-sharing.apib",
			   KEY_END),
		*kdb::Key (conf_root + "/backend/jwt/encryption/secret", KEY_VALUE, "some_random_string", KEY_END),
		*kdb::Key (conf_root + "/backend/jwt/validity", KEY_VALUE, "7200", KEY_END),
		*kdb::Key (conf_root + "/backend/kdb/path/configs", KEY_VALUE, "dir/configs", KEY_END),
		*kdb::Key (conf_root + "/backend/kdb/path/users", KEY_VALUE, "dir/users", KEY_END),
		*kdb::Key (conf_root + "/backend/output/default/entry/filterby", KEY_VALUE, "all", KEY_END),
		*kdb::Key (conf_root + "/backend/output/default/entry/sort", KEY_VALUE, "asc", KEY_END),
		*kdb::Key (conf_root + "/backend/output/default/entry/sortby", KEY_VALUE, "key", KEY_END),
		*kdb::Key (conf_root + "/backend/output/default/user/filterby", KEY_VALUE, "all", KEY_END),
		*kdb::Key (conf_root + "/backend/output/default/user/sort", KEY_VALUE, "asc", KEY_END),
		*kdb::Key (conf_root + "/backend/output/default/user/sortby", KEY_VALUE, "username", KEY_END),
		*kdb::Key (conf_root + "/backend/permissions/entry/create", KEY_VALUE, "10", KEY_END),
		*kdb::Key (conf_root + "/backend/permissions/entry/delete", KEY_VALUE, "50", KEY_END),
		*kdb::Key (conf_root + "/backend/permissions/entry/edit", KEY_VALUE, "50", KEY_END),
		*kdb::Key (conf_root + "/backend/permissions/rank/default", KEY_VALUE, "10", KEY_END),
		*kdb::Key (conf_root + "/backend/permissions/user/delete", KEY_VALUE, "100", KEY_END),
		*kdb::Key (conf_root + "/backend/permissions/user/edit", KEY_VALUE, "100", KEY_END),
		*kdb::Key (conf_root + "/backend/permissions/user/view", KEY_VALUE, "100", KEY_END),
		*kdb::Key (conf_root + "/cppcms/http/script_names/#0", KEY_VALUE, "/", KEY_END),
		*kdb::Key (conf_root + "/cppcms/service/api", KEY_VALUE, "http", KEY_END),
		*kdb::Key (conf_root + "/cppcms/service/ip", KEY_VALUE, "0.0.0.0", KEY_END),
		*kdb::Key (conf_root + "/cppcms/service/port", KEY_VALUE, "8080", KEY_END), KS_END);

	cppcms::json::value expected;
	expected["backend"]["api"]["description"]["html"] = "https://libelektra.org";
	expected["backend"]["api"]["description"]["raw"] =
		"https://raw.githubusercontent.com/ElektraInitiative/libelektra/master/doc/api_blueprints/snippet-sharing.apib";
	expected["backend"]["jwt"]["encryption"]["secret"] = "some_random_string";
	expected["backend"]["jwt"]["validity"] = 7200;
	expected["backend"]["kdb"]["path"]["configs"] = "dir/configs";
	expected["backend"]["kdb"]["path"]["users"] = "dir/users";
	expected["backend"]["output"]["default"]["entry"]["filterby"] = "all";
	expected["backend"]["output"]["default"]["entry"]["sort"] = "asc";
	expected["backend"]["output"]["default"]["entry"]["sortby"] = "key";
	expected["backend"]["output"]["default"]["user"]["filterby"] = "all";
	expected["backend"]["output"]["default"]["user"]["sort"] = "asc";
	expected["backend"]["output"]["default"]["user"]["sortby"] = "username";
	expected["backend"]["permissions"]["entry"]["create"] = 10;
	expected["backend"]["permissions"]["entry"]["edit"] = 50;
	expected["backend"]["permissions"]["entry"]["delete"] = 50;
	expected["backend"]["permissions"]["user"]["view"] = 100;
	expected["backend"]["permissions"]["user"]["edit"] = 100;
	expected["backend"]["permissions"]["user"]["delete"] = 100;
	expected["backend"]["permissions"]["rank"]["default"] = 10;
	expected["cppcms"]["http"]["script_names"][0] = "/";
	expected["cppcms"]["service"]["api"] = "http";
	expected["cppcms"]["service"]["ip"] = "0.0.0.0";
	expected["cppcms"]["service"]["port"] = 8080;

	cppcms::json::value result =
		kdbrest::service::ConfigEngine::instance ().transformKeysetToJsonValue (testKeySet, conf_root_cascading);

	ASSERT_TRUE (result == expected);
}

TEST (kdbrestServiceConfigengineTest, TransformKeysetToJsonValue3)
{

	std::string conf_root_dir = "dir/sw/elektra/restbackend/#0/current";
	std::string conf_root_proc = "proc/sw/elektra/restbackend/#0/current";
	std::string conf_root_spec = "spec/sw/elektra/restbackend/#0/current";
	std::string conf_root_system = "system/sw/elektra/restbackend/#0/current";
	std::string conf_root_user = "user/sw/elektra/restbackend/#0/current";
	std::string conf_root_cascading = "/sw/elektra/restbackend/#0/current";

	kdb::KeySet testKeySet (
		18, *kdb::Key (conf_root_spec + "/some/test/var", KEY_META, "default", "spec-default-val", KEY_END),
		*kdb::Key (conf_root_system + "/some/test/var", KEY_VALUE, "test-var-system", KEY_END),
		*kdb::Key (conf_root_user + "/some/test/var", KEY_VALUE, "test-var-user", KEY_END),
		*kdb::Key (conf_root_dir + "/some/test/var", KEY_VALUE, "test-var-dir", KEY_END),
		*kdb::Key (conf_root_proc + "/some/test/var", KEY_VALUE, "test-var-proc", KEY_END),

		*kdb::Key (conf_root_spec + "/some/other/var", KEY_META, "default", "spec-default-val", KEY_END),
		*kdb::Key (conf_root_system + "/some/other/var", KEY_END), *kdb::Key (conf_root_user + "/some/other/var", KEY_END),
		*kdb::Key (conf_root_dir + "/some/other/var", KEY_END), *kdb::Key (conf_root_proc + "/some/other/var", KEY_END),

		*kdb::Key (conf_root_spec + "/some/special/var", KEY_META, "default", "spec-default-val", KEY_END),
		*kdb::Key (conf_root_dir + "/some/special/var", KEY_VALUE, "test-var-dir", KEY_END),

		*kdb::Key (conf_root_spec + "/some/default/var", KEY_META, "default", "spec-default-val", KEY_END),

		*kdb::Key (conf_root_spec + "/some/array/var/#0", KEY_META, "default", "spec-default-val", KEY_END),
		*kdb::Key (conf_root_user + "/some/array/var/#0", KEY_VALUE, "test-var-user", KEY_END),
		*kdb::Key (conf_root_spec + "/some/array/var/#1", KEY_META, "default", "spec-default-val", KEY_END),

		*kdb::Key (conf_root_system + "/no/spec/var/", KEY_VALUE, "test-var-system", KEY_END),
		*kdb::Key (conf_root_user + "/no/spec/var/", KEY_VALUE, "test-var-user", KEY_END), KS_END);

	cppcms::json::value expected;
	expected["some"]["test"]["var"] = "test-var-proc";
	expected["some"]["other"]["var"] = "";
	expected["some"]["special"]["var"] = "test-var-dir";
	expected["some"]["default"]["var"] = "spec-default-val";
	expected["some"]["array"]["var"][0] = "test-var-user";
	expected["some"]["array"]["var"][1] = "spec-default-val";
	expected["no"]["spec"]["var"] = "test-var-user";

	cppcms::json::value result =
		kdbrest::service::ConfigEngine::instance ().transformKeysetToJsonValue (testKeySet, conf_root_cascading);

	ASSERT_TRUE (result == expected);
}

int main (int argc, char * argv[])
{
	testing::InitGoogleTest (&argc, argv);
	return RUN_ALL_TESTS ();
}
