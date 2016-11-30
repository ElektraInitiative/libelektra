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

	kdb::Key k1 (conf_root + "/nested/object/var", KEY_CASCADING_NAME, KEY_VALUE, "simple string", KEY_END);
	kdb::Key k2 (conf_root + "/nested/object/var2", KEY_CASCADING_NAME, KEY_VALUE, "12345", KEY_END);
	kdb::Key k3 (conf_root + "/nested/object/var3", KEY_CASCADING_NAME, KEY_VALUE, "1", KEY_END);
	kdb::Key k4 (conf_root + "/nested/object/var4", KEY_CASCADING_NAME, KEY_VALUE, "0", KEY_END);
	kdb::Key k5 (conf_root + "/object/array/#0", KEY_CASCADING_NAME, KEY_VALUE, "first element", KEY_END);
	kdb::Key k6 (conf_root + "/object/array/#1", KEY_CASCADING_NAME, KEY_VALUE, "second element", KEY_END);
	kdb::Key k7 (conf_root + "/object/array2/#0/var", KEY_CASCADING_NAME, KEY_VALUE, "first var", KEY_END);
	kdb::Key k8 (conf_root + "/object/array2/#0/var2", KEY_CASCADING_NAME, KEY_VALUE, "second var", KEY_END);
	kdb::Key k9 (conf_root + "/object/array2/#0/var3", KEY_CASCADING_NAME, KEY_VALUE, "third var", KEY_END);
	kdb::Key k10 (conf_root + "/object/array2/#1/var", KEY_CASCADING_NAME, KEY_VALUE, "first var", KEY_END);
	kdb::Key k11 (conf_root + "/object/array2/#2/var", KEY_CASCADING_NAME, KEY_VALUE, "first var", KEY_END);
	kdb::Key k12 (conf_root + "/object/array2/#2/var2", KEY_CASCADING_NAME, KEY_VALUE, "54321", KEY_END);
	kdb::Key k13 (conf_root + "/object/array3/#0/var/#0", KEY_CASCADING_NAME, KEY_VALUE, "arr0-in-arr0", KEY_END);
	kdb::Key k14 (conf_root + "/object/array3/#0/var/#1", KEY_CASCADING_NAME, KEY_VALUE, "arr1-in-arr0", KEY_END);
	kdb::Key k15 (conf_root + "/object/array3/#1/var/#0", KEY_CASCADING_NAME, KEY_VALUE, "arr0-in-arr1", KEY_END);

	kdb::KeySet testKeySet (15, *k1, *k2, *k3, *k4, *k5, *k6, *k7, *k8, *k9, *k10, *k11, *k12, *k13, *k14, *k15, KS_END);

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
	expected["object"]["array3"][0]["var"][0] = "arr0-in-arr1";

	cppcms::json::value result = kdbrest::service::ConfigEngine::instance ().transformKeysetToJsonValue (testKeySet, conf_root);

	ASSERT_TRUE (result == expected);
}

int main (int argc, char * argv[])
{
	testing::InitGoogleTest (&argc, argv);
	return RUN_ALL_TESTS ();
}
