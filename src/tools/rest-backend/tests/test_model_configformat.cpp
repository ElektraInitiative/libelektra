/**
 * @file
 *
 * @brief tests for the configformat model
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <gtest/gtest.h>

#include <model_configformat.hpp>
#include <model_pluginformat.hpp>

/**
 * TESTS for kdbrest::models::ConfigFormat
 */

TEST (kdbrestModelsConfigformatTest, ConstructorValueCheck)
{

	kdbrest::model::PluginFormat pf ("xml", "xmltool");
	kdbrest::model::ConfigFormat cf (pf, "<test-string>", true);

	ASSERT_EQ (cf.getPluginformat ().getFileformat (), pf.getFileformat ());
	ASSERT_EQ (cf.getPluginformat ().getPluginname (), pf.getPluginname ());
	ASSERT_EQ (cf.getConfig (), "<test-string>");
	ASSERT_EQ (cf.isValidated (), true);

	cf.setValidated (false);
	ASSERT_EQ (cf.isValidated (), false);
}
