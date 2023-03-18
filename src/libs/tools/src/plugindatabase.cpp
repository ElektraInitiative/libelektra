/**
 * @file
 *
 * @brief Implementation of PluginDatabase(s)
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <plugindatabase.hpp>

#include <modules.hpp>

#include <set>

#include <algorithm>
#include <helper/keyhelper.hpp>
#include <internal/kdb/config.h>
#include <internal/utility/logger.h>
#include <internal/macros/utils.h>

#ifdef HAVE_GLOB
#include <glob.h>
#endif

namespace kdb
{

namespace tools
{

class ModulesPluginDatabase::Impl
{
public:
	Impl ()
	{
	}
	~Impl ()
	{
	}
	Modules modules;
};

ModulesPluginDatabase::ModulesPluginDatabase () : impl (new ModulesPluginDatabase::Impl ())
{
}

ModulesPluginDatabase::~ModulesPluginDatabase ()
{
}

std::vector<std::string> ModulesPluginDatabase::listAllPlugins () const
{
	std::vector<std::string> ret;
#ifdef ELEKTRA_SHARED
#ifdef HAVE_GLOB
	std::set<std::string> toIgnore = {
		"proposal", "core", "ease", "meta", "plugin", "full", "kdb", "static",
	};
	glob_t pglob;
	if (glob (BUILTIN_PLUGIN_FOLDER "/libelektra-*", GLOB_NOSORT, NULL, &pglob) == 0)
	{
		ELEKTRA_LOG ("has glob %zd", pglob.gl_pathc);
		for (size_t i = 0; i < pglob.gl_pathc; ++i)
		{
			std::string fn (pglob.gl_pathv[i]);
			size_t start = fn.find_last_of ('-');
			if (start == std::string::npos) continue; // ignore wrong file
			std::string name = fn.substr (start + 1);
			size_t end = name.find_first_of ('.');
			name = name.substr (0, end);
			if (end == std::string::npos) continue;		       // ignore wrong file
			if (toIgnore.find (name) != toIgnore.end ()) continue; // ignore
			ret.push_back (name);
		}
		globfree (&pglob);
	}
#endif
	if (!ret.empty ())
	{
		std::sort (ret.begin (), ret.end ());
		return ret;
	}
// if we did not find plugins, return buildinPlugins
// (even if they might be wrong for ELEKTRA_SHARED)
#endif
	std::string buildinPlugins = ELEKTRA_PLUGINS;
	std::istringstream ss (buildinPlugins);
	std::string plugin;
	while (getline (ss, plugin, ';'))
	{
		ret.push_back (plugin);
	}
	// remove duplicates:
	std::sort (ret.begin (), ret.end ());
	ret.erase (std::unique (ret.begin (), ret.end ()), ret.end ());
	return ret;
}


namespace
{

bool hasProvides (PluginDatabase const & pd, std::string which)
{
	std::vector<std::string> allPlugins = pd.listAllPlugins ();
	std::string errors;

	for (auto const & plugin : allPlugins)
	{
		try
		{
			std::istringstream ss (pd.lookupInfo (
				PluginSpec (plugin,
					    KeySet (5,
						    *Key ("system:/module", KEY_VALUE, "this plugin was loaded without a config", KEY_END),
						    KS_END)),
				"provides"));
			std::string provide;
			while (ss >> provide)
			{
				if (provide == which)
				{
					return true;
				}
			}
		}
		catch (std::exception const & e)
		{
			errors += e.what ();
			errors += ",";
		}
	}

	if (errors.empty ())
		return false;
	else
		throw NoPlugin ("No plugin that provides " + which + " could be found, got errors: " + errors);
}
} // namespace


// TODO: directly use data from CONTRACT.ini
const std::map<std::string, int> PluginDatabase::statusMap = {
	// clang-format off
   {"default",      64000},
   {"recommended",  32000},
   {"productive",    8000},
   {"maintained",    4000},
   {"reviewed",      4000},
   {"conformant",    2000},
   {"compatible",    2000},
   {"coverage",      2000},
   {"specific",      1000},

   {"unittest",      1000},
   {"shelltest",     1000},
   {"tested",         500},
   {"nodep",          250},
   {"libc",           250},
   {"configurable",    50},
   {"final",           50},
   {"global",           1},
   {"readonly",         0},
   {"writeonly",        0},
   {"preview",        -50},
   {"memleak",       -250},
   {"experimental",  -500},
   {"difficult",     -500},
   {"limited",       -750},
   {"unfinished",   -1000},
   {"old",          -1000},
   {"nodoc",        -1000},
   {"concept",      -2000},
   {"orphan",       -4000},
   {"obsolete",     -4000},
   {"discouraged", -32000},

	// clang-format on
};


int PluginDatabase::calculateStatus (std::string statusString)
{
	int ret = 0;
	std::istringstream ss (statusString);
	std::string status;
	while (ss >> status)
	{
		auto it = statusMap.find (status);
		if (it != statusMap.end ())
		{
			ret += it->second;
		}
		else
		{
			try
			{
				ret += stoi (status);
			}
			catch (std::invalid_argument const &)
			{
			}
		}
	}
	return ret;
}

PluginDatabase::Status ModulesPluginDatabase::status (PluginSpec const & spec) const
{
	PluginPtr plugin;
	try
	{
		KeySet conf = spec.getConfig ();
		conf.append (Key ("system:/module", KEY_VALUE, "this plugin was loaded for the status", KEY_END));
		plugin = impl->modules.load (spec.getName (), conf);
		return real;
	}
	catch (...)
	{
		if (hasProvides (*this, spec.getName ()))
		{
			return provides;
		}
		else
		{
			return missing;
		}
	}
}

std::string ModulesPluginDatabase::lookupInfo (PluginSpec const & spec, std::string const & which) const
{
	KeySet conf = spec.getConfig ();
	conf.append (Key ("system:/module", KEY_VALUE, "this plugin was loaded for the status", KEY_END));
	PluginPtr plugin = impl->modules.load (spec.getName (), conf);
	return plugin->lookupInfo (which);
}

PluginDatabase::func_t ModulesPluginDatabase::getSymbol (PluginSpec const & spec, std::string const & which) const
{
	try
	{
		PluginPtr plugin = impl->modules.load (spec.getName (), spec.getConfig ());
		return plugin->getSymbol (which);
	}
	catch (...)
	{
		return NULL;
	}
}

PluginSpec ModulesPluginDatabase::lookupMetadata (std::string const & which) const
{
	std::vector<std::string> allPlugins = listAllPlugins ();
	std::map<int, PluginSpec> foundPlugins;

	std::string errors;
	// collect possible plugins
	for (auto const & plugin : allPlugins)
	{
		try
		{
			// TODO remove /module hack
			std::istringstream ss (lookupInfo (
				PluginSpec (plugin,
					    KeySet (5,
						    *Key ("system:/module", KEY_VALUE, "this plugin was loaded without a config", KEY_END),
						    KS_END)),
				"metadata"));
			std::string metadata;
			while (ss >> metadata)
			{
				if (metadata == which)
				{
					int s = calculateStatus (lookupInfo (
						PluginSpec (plugin, KeySet (5,
									    *Key ("system:/module", KEY_VALUE,
										  "this plugin was loaded without a config", KEY_END),
									    KS_END)),
						"status"));
					foundPlugins.insert (std::make_pair (s, PluginSpec (plugin)));
					break;
				}
			}
		}
		catch (std::exception const & e)
		{
			errors += e.what ();
			errors += ",";
		} // assume not loaded
	}

	if (foundPlugins.empty ())
	{
		if (!errors.empty ())
			throw NoPlugin ("No plugin that provides metadata " + which + " could be found, got errors: " + errors);
		else
			throw NoPlugin ("No plugin that provides metadata " + which + " could be found");
	}

	// the largest element of the map contains the best-suited plugin:
	return foundPlugins.rbegin ()->second;
}

PluginSpec ModulesPluginDatabase::lookupProvides (std::string const & which) const
{
	// check if plugin with provider name exists:
	if (status (PluginSpec (which)) == real)
	{
		return PluginSpec (which);
	}

	std::map<int, PluginSpec> foundPlugins;
	try
	{
		foundPlugins = lookupAllProvidesWithStatus (which);
	}
	catch (kdb::tools::NoPlugin const & e)
	{
		throw;
	}

	// the largest element of the map contains the best-suited plugin:
	return foundPlugins.rbegin ()->second;
}

std::map<int, PluginSpec> ModulesPluginDatabase::lookupAllProvidesWithStatus (std::string const & which) const
{
	std::string errors;
	std::vector<std::string> allPlugins = listAllPlugins ();
	std::map<int, PluginSpec> foundPlugins;
	for (auto const & plugin : allPlugins)
	{
		// TODO: make sure (non)-equal plugins (i.e. with same/different contract) are handled correctly
		try
		{
			PluginSpec spec = PluginSpec (
				plugin,
				KeySet (5, *Key ("system:/module", KEY_VALUE, "this plugin was loaded without a config", KEY_END), KS_END));

			// lets see if there is a plugin named after the required provider
			if (plugin == which)
			{
				int s = calculateStatus (lookupInfo (spec, "status"));
				foundPlugins.insert (std::make_pair (s, PluginSpec (plugin)));
				continue; // we are done with this plugin
			}

			// TODO: support for generic plugins with config
			std::istringstream ss (lookupInfo (spec, "provides"));
			std::string provide;
			while (ss >> provide)
			{
				if (provide == which)
				{
					int s = calculateStatus (lookupInfo (spec, "status"));
					foundPlugins.insert (std::make_pair (s, PluginSpec (plugin)));
				}
			}
		}
		catch (std::exception const & e)
		{
			errors += e.what ();
			errors += ",";
		} // assume not loaded
	}

	if (foundPlugins.empty ())
	{
		if (!errors.empty ())
			throw NoPlugin ("No plugin that provides " + which + " could be found, got errors: " + errors);
		else
			throw NoPlugin ("No plugin that provides " + which + " could be found");
	}

	return foundPlugins;
}

std::vector<PluginSpec> ModulesPluginDatabase::lookupAllProvides (std::string const & which) const
{
	try
	{
		const std::map<int, PluginSpec> foundPlugins = lookupAllProvidesWithStatus (which);

		// we found some plugins, lets convert the map into a vector
		std::vector<PluginSpec> plugins;
		plugins.reserve (foundPlugins.size ());
		std::for_each (foundPlugins.begin (), foundPlugins.end (),
			       [&plugins] (const std::map<int, PluginSpec>::value_type & elem) { plugins.push_back (elem.second); });
		return plugins;
	}
	catch (kdb::tools::NoPlugin const & e)
	{
		// if no plugins were found, return an empty vector
		return std::vector<PluginSpec> ();
	}
}


class PluginVariantDatabase::VariantImpl
{
public:
	explicit VariantImpl (const KeySet & conf) : pluginconf (conf)
	{
	}
	~VariantImpl ()
	{
	}
	KeySet pluginconf;
};

PluginVariantDatabase::PluginVariantDatabase (const KeySet & conf)
: ModulesPluginDatabase (), variantImpl (new PluginVariantDatabase::VariantImpl (conf))
{
}

PluginVariantDatabase::~PluginVariantDatabase ()
{
}

std::vector<std::string> PluginVariantDatabase::listAllPlugins () const
{
	std::vector<std::string> plugins (ModulesPluginDatabase::listAllPlugins ());
	plugins.erase (std::remove_if (plugins.begin (), plugins.end (),
				       [this] (const std::string & elem) {
					       Key k ("system:/elektra/plugins", KEY_END);
					       k.addBaseName (elem);
					       k.addBaseName ("disable");
					       Key res = this->variantImpl->pluginconf.lookup (k);
					       return res && res.getString () == "1";
				       }),
		       plugins.end ());
	return plugins;
}

std::vector<PluginSpec> PluginVariantDatabase::getPluginVariants (PluginSpec const & whichplugin) const
{
	PluginPtr plugin = this->impl->modules.load (whichplugin);
	KeySet ksSysconf (this->variantImpl->pluginconf);
	KeySet ksGenconf;

	// read plugin variants via genconf
	try
	{
		auto funcGenconf = reinterpret_cast<void (*) (ckdb::KeySet *, ckdb::Key *)> (plugin->getSymbol ("genconf"));
		funcGenconf (ksGenconf.getKeySet (), 0);
	}
	catch (kdb::tools::MissingSymbol const & e)
	{
		// no genconf, but maybe sysconf variants
		KeySet placeholder;
		return this->getPluginVariantsFromSysconf (whichplugin, ksSysconf, placeholder);
	}

	// get plugin variants from genconf, but also consider sysconf for disable/override
	return this->getPluginVariantsFromGenconf (whichplugin, ksGenconf, ksSysconf);
}

std::vector<PluginSpec> PluginVariantDatabase::getPluginVariantsFromSysconf (PluginSpec const & whichplugin, KeySet const & sysconf,
									     KeySet const & genconfToIgnore) const
{
	std::vector<PluginSpec> result;

	KeySet ksSysconf (sysconf);

	// first find possible variants
	Key kVariantBase ("system:/elektra/plugins", KEY_END);
	kVariantBase.addBaseName (whichplugin.getName ());
	kVariantBase.addBaseName ("variants");

	KeySet ksPluginVariantSysconf (ksSysconf.cut (kVariantBase));
	KeySet ksToIterate (ksPluginVariantSysconf);
	for (auto kCurrent : ksToIterate)
	{
		Key kCurrentTest (kVariantBase);
		kCurrentTest.addBaseName (kCurrent.getBaseName ());
		if (kCurrentTest == kCurrent)
		{
			PluginSpec variant (whichplugin);
			KeySet ksVariantConfToAdd;

			// new base for plugin conf
			Key kVariantPluginConf ("system:/", KEY_END);

			// add system conf for plugin variant
			Key kVariantSysconf (this->buildVariantSysconfKey (whichplugin, kCurrent.getBaseName (), "config"));
			this->addKeysBelowKeyToConf (kVariantSysconf, ksPluginVariantSysconf, kVariantPluginConf, ksVariantConfToAdd);

			// check if the variant was disabled : system:/elektra/plugins/simpleini/variants/space/disable
			Key kDisable = sysconf.lookup (this->buildVariantSysconfKey (whichplugin, kCurrent.getBaseName (), "disable"));
			if (kDisable && kDisable.getString () == "1")
			{
				continue; // skip this variant
			}

			// check if the variant is in the genconfToIgnore list
			Key kGenconfVariant (kVariantPluginConf);
			kGenconfVariant.addBaseName (kCurrent.getBaseName ());
			Key kIgnore = genconfToIgnore.lookup (kGenconfVariant);
			if (kIgnore)
			{
				continue; // this variant was added by genconf already
			}

			if (ksVariantConfToAdd.size () == 0)
			{
				continue; // no config means no variant
			}

			variant.appendConfig (ksVariantConfToAdd);
			result.push_back (variant);
		}
	}

	return result;
}

std::vector<PluginSpec> PluginVariantDatabase::getPluginVariantsFromGenconf (PluginSpec const & whichplugin, KeySet const & genconf,
									     KeySet const & sysconf) const
{
	std::vector<PluginSpec> result;

	KeySet ksToIterate (genconf);
	for (auto kCurrent : ksToIterate)
	{
		Key kCurrentTest ("/", KEY_END);
		kCurrentTest.setNamespace (kCurrent.getNamespace ());
		kCurrentTest.addBaseName (kCurrent.getBaseName ()); // e.g. system:/space
		if (kCurrentTest == kCurrent)
		{
			PluginSpec variant (whichplugin);
			KeySet ksVariantConfToAdd;

			// new base for plugin conf
			Key kVariantPluginConf ("system:/", KEY_END);

			// take variant config from genconf and transform it to proper plugin conf,
			// e.g. system:/space/config/format -> system:/format
			Key kVariantConf (kCurrentTest);
			kVariantConf.addBaseName ("config"); // e.g. system:/space/config
			this->addKeysBelowKeyToConf (kVariantConf, genconf, kVariantPluginConf, ksVariantConfToAdd);

			// TODO plugin infos

			// check if the variant was disabled : system:/elektra/plugins/simpleini/variants/space/disable
			Key kDisable = sysconf.lookup (this->buildVariantSysconfKey (whichplugin, kCurrent.getBaseName (), "disable"));
			if (kDisable && kDisable.getString () == "1")
			{
				continue; // skip this variant
			}

			// check if an override is available : system:/elektra/plugins/simpleini/variants/space/override
			Key kOverride = sysconf.lookup (this->buildVariantSysconfKey (whichplugin, kCurrent.getBaseName (), "override"));
			if (kOverride && kOverride.getString () == "1")
			{
				// first delete config from genconf entirely
				ksVariantConfToAdd.clear ();
				Key kVariantSysconf (this->buildVariantSysconfKey (whichplugin, kCurrent.getBaseName (), "config"));
				this->addKeysBelowKeyToConf (kVariantSysconf, sysconf, kVariantPluginConf, ksVariantConfToAdd);
			}

			if (ksVariantConfToAdd.size () == 0)
			{
				continue; // no config means no variant
			}

			variant.appendConfig (ksVariantConfToAdd);
			result.push_back (variant);
		}
	}

	std::vector<PluginSpec> resFromSysconf (this->getPluginVariantsFromSysconf (whichplugin, sysconf, genconf));
	result.insert (result.end (), resFromSysconf.begin (), resFromSysconf.end ());

	return result;
}

Key PluginVariantDatabase::buildVariantSysconfKey (PluginSpec const & whichplugin, std::string const & variant,
						   const std::string attr) const
{
	Key result ("system:/elektra/plugins", KEY_END);
	result.addBaseName (whichplugin.getName ());
	result.addBaseName ("variants");
	result.addBaseName (variant);
	result.addBaseName (attr);
	return result;
}

void PluginVariantDatabase::addKeysBelowKeyToConf (Key const & below, KeySet const & conf, Key const & newbase, KeySet & targetconf) const
{
	KeySet confCp (conf);
	KeySet ksVariantSysConf = confCp.cut (below);
	for (auto kVariantCurrent : ksVariantSysConf)
	{
		if (!kVariantCurrent.isBelow (below)) continue;
		targetconf.append (helper::rebaseKey (kVariantCurrent, below, newbase));
	}
}


std::vector<std::string> MockPluginDatabase::listAllPlugins () const
{
	std::vector<std::string> plugins;
	for (auto const & plugin : data)
	{
		plugins.push_back (plugin.first.getName ());
	}
	return plugins;
}

PluginDatabase::Status MockPluginDatabase::status (PluginSpec const & spec) const
{
	auto it = data.find (spec);
	if (it != data.end ())
	{
		return real;
	}

	if (hasProvides (*this, spec.getName ()))
	{
		return provides;
	}

	return missing;
}


std::string MockPluginDatabase::lookupInfo (PluginSpec const & spec, std::string const & which) const
{
	auto it = data.find (spec);
	if (it != data.end ())
	{
		return it->second[which];
	}

	return "";
}

PluginDatabase::func_t MockPluginDatabase::getSymbol (PluginSpec const & spec ELEKTRA_UNUSED, std::string const & which) const
{
	if (which == "checkconf")
	{
		return reinterpret_cast<func_t> (checkconf);
	}
	return NULL;
}

void MockPluginDatabase::setCheckconfFunction (const MockPluginDatabase::checkConfPtr newCheckconf)
{
	checkconf = newCheckconf;
}
} // namespace tools
} // namespace kdb
