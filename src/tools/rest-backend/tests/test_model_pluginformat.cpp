/**
 * @file
 *
 * @brief tests for the pluginformat model
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <gtest/gtest.h>

#include <config.hpp>
#include <model_pluginformat.hpp>
#include <service.hpp>

/**
 * TESTS for kdbrest::models::PluginFormat
 */

TEST (kdbrestModelsPluginformatTest, ConstructorCheck)
{

	try
	{
		kdbrest::model::PluginFormat pf ("xml", "xmltool");
	}
	catch (kdbrest::exception::FileformatPluginException & e)
	{
		ASSERT_TRUE (false);
	}

	kdbrest::model::PluginFormat pf ("xml", "xmltool");
	try
	{
		kdbrest::model::PluginFormat pf2 (pf);
	}
	catch (kdbrest::exception::FileformatPluginException & e)
	{
		ASSERT_TRUE (false);
	}
}

TEST (kdbrestModelsPluginformatTest, ValueCheck)
{

	kdbrest::model::PluginFormat pf2 ("xml", "xmltool");
	ASSERT_EQ (pf2.getFileformat (), "xml");
	ASSERT_EQ (pf2.getPluginname (), "xmltool");

	kdbrest::model::PluginFormat pf3 (pf2);
	ASSERT_EQ (pf3.getFileformat (), "xml");
	ASSERT_EQ (pf3.getPluginname (), "xmltool");
	ASSERT_EQ (pf2.getFileformat (), pf3.getFileformat ());
	ASSERT_EQ (pf2.getPluginname (), pf3.getPluginname ());
}

int main (int argc, char * argv[])
{
	testing::InitGoogleTest (&argc, argv);
	cppcms::json::value config = kdbrest::service::ConfigEngine::instance ().loadApplicationConfiguration ();
	(void)kdbrest::Config::instance ().initializeConfiguration (config);
	return RUN_ALL_TESTS ();
}
