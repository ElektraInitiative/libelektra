/**
 * @file
 *
 * @brief tests for the convert service
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <boost/algorithm/string/replace.hpp>
#include <gtest/gtest.h>

#include <config.hpp>
#include <kdb_includes.hpp>
#include <plugindatabase.hpp>
#include <pluginspec.hpp>
#include <service.hpp>

/**
 * TESTS for kdbrest::service::ConvertEngine
 */

TEST (kdbrestServicesConvertengineTest, FindSuitablePluginCheck)
{

	using namespace kdb::tools;
	using namespace kdbrest;

	ModulesPluginDatabase db;
	std::vector<std::string> allPlugins = db.listAllPlugins ();

	if (std::find (allPlugins.begin (), allPlugins.end (), "dump") != allPlugins.end ())
	{
		std::string formatDump = "dump";
		model::PluginFormat pfDump = service::ConvertEngine::instance ().findSuitablePlugin (formatDump);
		ASSERT_EQ ("dump", pfDump.getFileformat ());
		ASSERT_EQ ("dump", pfDump.getPluginname ());
	}

	if (std::find (allPlugins.begin (), allPlugins.end (), "line") != allPlugins.end ())
	{
		std::string formatLine = "line";
		model::PluginFormat pfLine = service::ConvertEngine::instance ().findSuitablePlugin (formatLine);
		ASSERT_EQ ("line", pfLine.getFileformat ());
		ASSERT_EQ ("line", pfLine.getPluginname ());
	}

	if (std::find (allPlugins.begin (), allPlugins.end (), "simpleini") != allPlugins.end ())
	{
		std::string formatSimpleini = "simpleini";
		model::PluginFormat pfSimpleini = service::ConvertEngine::instance ().findSuitablePlugin (formatSimpleini);
		ASSERT_EQ ("ini", pfSimpleini.getFileformat ());
		ASSERT_EQ ("simpleini", pfSimpleini.getPluginname ());
	}
}

TEST (kdbrestServicesConvertengineTest, ExportToFormatCheck)
{

	using namespace kdb::tools;
	using namespace kdbrest;

	ModulesPluginDatabase db;
	std::vector<std::string> allPlugins = db.listAllPlugins ();

	if (std::find (allPlugins.begin (), allPlugins.end (), "simpleini") != allPlugins.end ())
	{
		std::string conf_root = kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.configs");
		std::string entryName = "org/app/scope/slug";
		model::Entry entry (entryName);
		entry.addSubkey (kdb::Key (conf_root + "/" + entryName + "/obj/var1", KEY_VALUE, "value1", KEY_END));
		entry.addSubkey (kdb::Key (conf_root + "/" + entryName + "/obj/var2", KEY_VALUE, "value2", KEY_END));
		entry.addSubkey (kdb::Key (conf_root + "/" + entryName + "/var", KEY_VALUE, "value", KEY_END));

		ASSERT_EQ (3, entry.getSubkeys ().size ());

		model::ConfigFormat cf = service::ConvertEngine::instance ().exportTo ("simpleini", entry);

		ASSERT_EQ (cf.getPluginformat ().getFileformat (), "ini");
		ASSERT_EQ (cf.getPluginformat ().getPluginname (), "simpleini");
		ASSERT_TRUE (cf.isValidated ());

		std::string expected_output = "obj/var1=value1\nobj/var2=value2\nvar=value";
		std::string retrieved_output = cf.getConfig ();
		boost::replace_all (retrieved_output, " ", "");
		if (retrieved_output.at (retrieved_output.length () - 1) == '\n')
		{
			retrieved_output = retrieved_output.substr (0, retrieved_output.length () - 1);
		}
		ASSERT_EQ (expected_output, retrieved_output);
	}
}

TEST (kdbrestServicesConvertengineTest, ExportToFormatWithConfigCheck)
{

	using namespace kdb::tools;
	using namespace kdbrest;

	ModulesPluginDatabase db;
	std::vector<std::string> allPlugins = db.listAllPlugins ();

	if (std::find (allPlugins.begin (), allPlugins.end (), "simpleini") != allPlugins.end ())
	{
		std::string conf_root = kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.configs");
		std::string entryName = "org/app/scope/slug";
		model::Entry entry (entryName);
		entry.addSubkey (kdb::Key (conf_root + "/" + entryName + "/obj/var1", KEY_VALUE, "value1", KEY_END));
		entry.addSubkey (kdb::Key (conf_root + "/" + entryName + "/obj/var2", KEY_VALUE, "value2", KEY_END));
		entry.addSubkey (kdb::Key (conf_root + "/" + entryName + "/var", KEY_VALUE, "value", KEY_END));

		ASSERT_EQ (3, entry.getSubkeys ().size ());

		model::ConfigFormat cf = service::ConvertEngine::instance ().exportTo ("simpleini format=%|||%", entry);

		ASSERT_EQ (cf.getPluginformat ().getFileformat (), "ini");
		ASSERT_EQ (cf.getPluginformat ().getPluginname (), "simpleini");
		ASSERT_EQ (cf.getPluginformat ().getConfig ().size (), 1);

		std::string expected_output = "obj/var1|||value1\nobj/var2|||value2\nvar|||value";
		std::string retrieved_output = cf.getConfig ();
		boost::replace_all (retrieved_output, " ", "");
		if (retrieved_output.at (retrieved_output.length () - 1) == '\n')
		{
			retrieved_output = retrieved_output.substr (0, retrieved_output.length () - 1);
		}
		ASSERT_EQ (expected_output, retrieved_output);
	}
}

TEST (kdbrestServicesConvertengineTest, ExportToFormatCheck2)
{

	using namespace kdb::tools;
	using namespace kdbrest;

	ModulesPluginDatabase db;
	std::vector<std::string> allPlugins = db.listAllPlugins ();

	if (std::find (allPlugins.begin (), allPlugins.end (), "yajl") != allPlugins.end ())
	{
		std::string conf_root = kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.configs");
		std::string entryName = "";
		model::Entry entry (entryName);
		entry.addSubkey (kdb::Key (conf_root + "/obj/var1", KEY_VALUE, "value1", KEY_END));
		entry.addSubkey (kdb::Key (conf_root + "/obj/var2", KEY_VALUE, "value2", KEY_END));
		entry.addSubkey (kdb::Key (conf_root + "/var", KEY_VALUE, "value", KEY_END));

		ASSERT_EQ (3, entry.getSubkeys ().size ());

		model::ConfigFormat cf = service::ConvertEngine::instance ().exportTo ("yajl", entry);

		ASSERT_EQ (cf.getPluginformat ().getFileformat (), "json");
		ASSERT_EQ (cf.getPluginformat ().getPluginname (), "yajl");
		ASSERT_FALSE (cf.isValidated ());

		std::string expected_output = "{\"obj\":{\"var1\":\"value1\",\"var2\":\"value2\"},\"var\":\"value\"}";
		std::string retrieved_output = cf.getConfig ();
		boost::replace_all (retrieved_output, " ", "");
		boost::replace_all (retrieved_output, "\n", "");
		ASSERT_EQ (expected_output, retrieved_output);
	}
}

TEST (kdbrestServicesConvertengineTest, ExportToPluginCheck)
{

	using namespace kdb::tools;
	using namespace kdbrest;

	ModulesPluginDatabase db;
	std::vector<std::string> allPlugins = db.listAllPlugins ();

	if (std::find (allPlugins.begin (), allPlugins.end (), "simpleini") != allPlugins.end ())
	{
		std::string conf_root = kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.configs");
		std::string entryName = "org/app/scope/slug";
		model::Entry entry (entryName);
		entry.addSubkey (kdb::Key (conf_root + "/" + entryName + "/obj/var1", KEY_VALUE, "value1", KEY_END));
		entry.addSubkey (kdb::Key (conf_root + "/" + entryName + "/obj/var2", KEY_VALUE, "value2", KEY_END));
		entry.addSubkey (kdb::Key (conf_root + "/" + entryName + "/var", KEY_VALUE, "value", KEY_END));

		ASSERT_EQ (3, entry.getSubkeys ().size ());

		model::PluginFormat pf ("ini", "simpleini");
		model::ConfigFormat cf = service::ConvertEngine::instance ().exportTo (pf, entry);

		ASSERT_EQ (cf.getPluginformat ().getFileformat (), "ini");
		ASSERT_EQ (cf.getPluginformat ().getPluginname (), "simpleini");
		ASSERT_TRUE (cf.isValidated ());

		std::string expected_output = "obj/var1=value1\nobj/var2=value2\nvar=value";
		std::string retrieved_output = cf.getConfig ();
		boost::replace_all (retrieved_output, " ", "");
		if (retrieved_output.at (retrieved_output.length () - 1) == '\n')
		{
			retrieved_output = retrieved_output.substr (0, retrieved_output.length () - 1);
		}
		ASSERT_EQ (expected_output, retrieved_output);
	}
}

TEST (kdbrestServicesConvertengineTest, ExportToPluginCheck2)
{

	using namespace kdb::tools;
	using namespace kdbrest;

	ModulesPluginDatabase db;
	std::vector<std::string> allPlugins = db.listAllPlugins ();

	if (std::find (allPlugins.begin (), allPlugins.end (), "simpleini") != allPlugins.end ())
	{
		std::string conf_root = kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.configs");
		std::string entryName = "";
		model::Entry entry (entryName);
		entry.addSubkey (kdb::Key (conf_root + "/obj/var1", KEY_VALUE, "value1", KEY_END));
		entry.addSubkey (kdb::Key (conf_root + "/obj/var2", KEY_VALUE, "value2", KEY_END));
		entry.addSubkey (kdb::Key (conf_root + "/var", KEY_VALUE, "value", KEY_END));

		ASSERT_EQ (3, entry.getSubkeys ().size ());

		model::PluginFormat pf ("ini", "simpleini");
		model::ConfigFormat cf = service::ConvertEngine::instance ().exportTo (pf, entry);

		ASSERT_EQ (cf.getPluginformat ().getFileformat (), "ini");
		ASSERT_EQ (cf.getPluginformat ().getPluginname (), "simpleini");
		ASSERT_TRUE (cf.isValidated ());

		std::string expected_output = "obj/var1=value1\nobj/var2=value2\nvar=value";
		std::string retrieved_output = cf.getConfig ();
		boost::replace_all (retrieved_output, " ", "");
		if (retrieved_output.at (retrieved_output.length () - 1) == '\n')
		{
			retrieved_output = retrieved_output.substr (0, retrieved_output.length () - 1);
		}
		ASSERT_EQ (expected_output, retrieved_output);
	}
}

TEST (kdbrestServicesConvertengineTest, ImportCheck)
{

	using namespace kdb::tools;
	using namespace kdbrest;

	ModulesPluginDatabase db;
	std::vector<std::string> allPlugins = db.listAllPlugins ();

	if (std::find (allPlugins.begin (), allPlugins.end (), "simpleini") != allPlugins.end ())
	{
		std::string conf_root = kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.configs");
		std::string entryName = "org/app/scope/slug";
		model::Entry entry (entryName);
		std::string input_config = "obj/var1 = value1\nobj/var2 = value2\nvar = value";

		model::ImportedConfig icfg = service::ConvertEngine::instance ().import (input_config, "simpleini", entry);

		ASSERT_EQ ("ini", icfg.getPluginformat ().getFileformat ());
		ASSERT_EQ ("simpleini", icfg.getPluginformat ().getPluginname ());
		ASSERT_EQ (3, icfg.getKeySet ().size ());

		ASSERT_TRUE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/" + entryName + "/obj/var1", KEY_END)));
		ASSERT_TRUE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/" + entryName + "/obj/var2", KEY_END)));
		ASSERT_TRUE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/" + entryName + "/var", KEY_END)));

		ASSERT_FALSE (icfg.getKeySet ().lookup (kdb::Key (entryName + "/something", KEY_END)));
		ASSERT_FALSE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/" + entryName + "/something", KEY_END)));
		ASSERT_FALSE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/" + entryName + "/something/else", KEY_END)));
	}
}

TEST (kdbrestServicesConvertengineTest, ImportCheck2)
{

	using namespace kdb::tools;
	using namespace kdbrest;

	ModulesPluginDatabase db;
	std::vector<std::string> allPlugins = db.listAllPlugins ();

	if (std::find (allPlugins.begin (), allPlugins.end (), "ini") != allPlugins.end ())
	{
		std::string conf_root = kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.configs");
		std::string entryName = "org/app/scope/slug";
		model::Entry entry (entryName);
		std::string input_config = "obj/var1 = value1\nobj/var2 = value2\nvar = value";

		model::ImportedConfig icfg = service::ConvertEngine::instance ().import (input_config, "ini", entry);

		ASSERT_EQ ("ini", icfg.getPluginformat ().getFileformat ());
		ASSERT_EQ ("ini", icfg.getPluginformat ().getPluginname ());
		ASSERT_EQ (3, icfg.getKeySet ().size ());

		ASSERT_TRUE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/" + entryName + "/obj/var1", KEY_END)));
		ASSERT_TRUE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/" + entryName + "/obj/var2", KEY_END)));
		ASSERT_TRUE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/" + entryName + "/var", KEY_END)));

		ASSERT_FALSE (icfg.getKeySet ().lookup (kdb::Key (entryName + "/something", KEY_END)));
		ASSERT_FALSE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/" + entryName + "/something", KEY_END)));
		ASSERT_FALSE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/" + entryName + "/something/else", KEY_END)));
	}
}

TEST (kdbrestServicesConvertengineTest, ImportCheck3)
{

	using namespace kdb::tools;
	using namespace kdbrest;

	ModulesPluginDatabase db;
	std::vector<std::string> allPlugins = db.listAllPlugins ();

	if (std::find (allPlugins.begin (), allPlugins.end (), "simpleini") != allPlugins.end ())
	{
		std::string conf_root = kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.configs");
		std::string entryName = "";
		model::Entry entry (entryName);
		std::string input_config = "obj/var1 = value1\nobj/var2 = value2\nvar = value";

		model::ImportedConfig icfg = service::ConvertEngine::instance ().import (input_config, "simpleini", entry);

		ASSERT_EQ ("ini", icfg.getPluginformat ().getFileformat ());
		ASSERT_EQ ("simpleini", icfg.getPluginformat ().getPluginname ());
		ASSERT_EQ (3, icfg.getKeySet ().size ());

		ASSERT_TRUE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/obj/var1", KEY_END)));
		ASSERT_TRUE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/obj/var2", KEY_END)));
		ASSERT_TRUE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/var", KEY_END)));

		ASSERT_FALSE (icfg.getKeySet ().lookup (kdb::Key (entryName + "/something", KEY_END)));
		ASSERT_FALSE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/" + entryName + "/something", KEY_END)));
		ASSERT_FALSE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/something/else", KEY_END)));
	}
}

TEST (kdbrestServicesConvertengineTest, ImportCheck4)
{

	using namespace kdb::tools;
	using namespace kdbrest;

	ModulesPluginDatabase db;
	std::vector<std::string> allPlugins = db.listAllPlugins ();

	if (std::find (allPlugins.begin (), allPlugins.end (), "yajl") != allPlugins.end ())
	{
		std::string conf_root = kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.configs");
		std::string entryName = "";
		model::Entry entry (entryName);
		std::string input_config = "{\"obj\":{\"var1\":\"value1\",\"var2\":\"value2\"},\"var\":\"value\"}";

		model::ImportedConfig icfg = service::ConvertEngine::instance ().import (input_config, "yajl", entry);

		ASSERT_EQ ("json", icfg.getPluginformat ().getFileformat ());
		ASSERT_EQ ("yajl", icfg.getPluginformat ().getPluginname ());
		ASSERT_TRUE (3 <= icfg.getKeySet ().size ());

		ASSERT_TRUE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/obj/var1", KEY_END)));
		ASSERT_TRUE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/obj/var2", KEY_END)));
		ASSERT_TRUE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/var", KEY_END)));

		ASSERT_FALSE (icfg.getKeySet ().lookup (kdb::Key (entryName + "/something", KEY_END)));
		ASSERT_FALSE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/" + entryName + "/something", KEY_END)));
		ASSERT_FALSE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/something/else", KEY_END)));
	}
}

TEST (kdbrestServicesConvertengineTest, ImportWithConfigCheck)
{

	using namespace kdb::tools;
	using namespace kdbrest;

	ModulesPluginDatabase db;
	std::vector<std::string> allPlugins = db.listAllPlugins ();

	if (std::find (allPlugins.begin (), allPlugins.end (), "simpleini") != allPlugins.end ())
	{
		std::string conf_root = kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.configs");
		std::string entryName = "";
		model::Entry entry (entryName);
		std::string input_config = "obj/var1 value1\nobj/var2 value2\nvar value";

		model::ImportedConfig icfg = service::ConvertEngine::instance ().import (input_config, "simpleini format=% %", entry);

		ASSERT_EQ ("ini", icfg.getPluginformat ().getFileformat ());
		ASSERT_EQ ("simpleini", icfg.getPluginformat ().getPluginname ());
		ASSERT_EQ (1, icfg.getPluginformat ().getConfig ().size ());
		ASSERT_EQ (3, icfg.getKeySet ().size ());

		ASSERT_TRUE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/obj/var1", KEY_END)));
		ASSERT_TRUE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/obj/var2", KEY_END)));
		ASSERT_TRUE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/var", KEY_END)));

		ASSERT_FALSE (icfg.getKeySet ().lookup (kdb::Key (entryName + "/something", KEY_END)));
		ASSERT_FALSE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/" + entryName + "/something", KEY_END)));
		ASSERT_FALSE (icfg.getKeySet ().lookup (kdb::Key (conf_root + "/something/else", KEY_END)));
	}
}

TEST (kdbrestServicesConvertengineTest, FullConversionCheck)
{

	using namespace kdb::tools;
	using namespace kdbrest;

	ModulesPluginDatabase db;
	std::vector<std::string> allPlugins = db.listAllPlugins ();

	if (std::find (allPlugins.begin (), allPlugins.end (), "simpleini") != allPlugins.end () &&
	    std::find (allPlugins.begin (), allPlugins.end (), "yajl") != allPlugins.end ())
	{
		std::string conf_root = kdbrest::Config::instance ().getConfig ().get<std::string> ("kdb.path.configs");
		std::string entryName = "";
		model::Entry entry (entryName);
		std::string input_config = "obj/var1 value1\nobj/var2 value2\nvar value";

		// import
		model::ImportedConfig icfg = service::ConvertEngine::instance ().import (input_config, "simpleini format=% %", entry);
		ASSERT_EQ ("ini", icfg.getPluginformat ().getFileformat ());
		ASSERT_EQ ("simpleini", icfg.getPluginformat ().getPluginname ());
		ASSERT_EQ (1, icfg.getPluginformat ().getConfig ().size ());
		ASSERT_EQ (3, icfg.getKeySet ().size ());
		kdb::KeySet ks = icfg.getKeySet ();
		entry.addSubkeys (ks);
		ASSERT_EQ (3, entry.getSubkeys ().size ());

		// export
		model::ConfigFormat cf = service::ConvertEngine::instance ().exportTo ("yajl", entry);
		ASSERT_EQ (cf.getPluginformat ().getFileformat (), "json");
		ASSERT_EQ (cf.getPluginformat ().getPluginname (), "yajl");

		std::string expected_output = "{\"obj\":{\"var1\":\"value1\",\"var2\":\"value2\"},\"var\":\"value\"}";
		std::string retrieved_output = cf.getConfig ();
		boost::replace_all (retrieved_output, " ", "");
		boost::replace_all (retrieved_output, "\n", "");
		ASSERT_EQ (expected_output, retrieved_output);
	}
}

int main (int argc, char * argv[])
{
	testing::InitGoogleTest (&argc, argv);
	cppcms::json::value config = kdbrest::service::ConfigEngine::instance ().loadApplicationConfiguration ();
	(void)kdbrest::Config::instance ().initializeConfiguration (config);
	kdbrest::Config::instance ().setValue<std::string> ("kdb.path.users", "dir/users");
	kdbrest::Config::instance ().setValue<std::string> ("kdb.path.configs", "dir/configs");
	return RUN_ALL_TESTS ();
}
