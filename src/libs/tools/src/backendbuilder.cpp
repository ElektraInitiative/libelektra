/**
 * @file
 *
 * @brief Implementation of backend builder
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include <backend.hpp>
#include <backendbuilder.hpp>
#include <backendparser.hpp>
#include <backends.hpp>
#include <plugindatabase.hpp>
#include <pluginspec.hpp>


#include <helper/keyhelper.hpp>
#include <kdbmodule.h>
#include <kdbplugin.h>
#include <kdbprivate.h>

#include <algorithm>
#include <functional>
#include <set>
#include <unordered_set>

#include <cassert>
#include <kdb.hpp>
#include <kdbmeta.h>


using namespace std;


namespace kdb
{


namespace tools
{


BackendBuilderInit::BackendBuilderInit () : pluginDatabase (make_shared<ModulesPluginDatabase> ()), backendFactory ("backend")
{
}


BackendBuilderInit::BackendBuilderInit (PluginDatabasePtr const & plugins) : pluginDatabase (plugins), backendFactory ("backend")
{
}

BackendBuilderInit::BackendBuilderInit (BackendFactory const & bf)
: pluginDatabase (make_shared<ModulesPluginDatabase> ()), backendFactory (bf)
{
}

BackendBuilderInit::BackendBuilderInit (PluginDatabasePtr const & plugins, BackendFactory const & bf)
: pluginDatabase (plugins), backendFactory (bf)
{
}

BackendBuilderInit::BackendBuilderInit (BackendFactory const & bf, PluginDatabasePtr const & plugins)
: pluginDatabase (plugins), backendFactory (bf)
{
}


BackendBuilder::BackendBuilder (BackendBuilderInit const & bbi)
: pluginDatabase (bbi.getPluginDatabase ()), backendFactory (bbi.getBackendFactory ())
{
}


BackendBuilder::~BackendBuilder ()
{
}

MountBackendBuilder::MountBackendBuilder (BackendBuilderInit const & bbi) : BackendBuilder (bbi)
{
}

/**
 * @brief Makes sure that ordering constraints are fulfilled.
 *
 * @pre a sorted list except of the last element to be inserted
 * @post the last element will be moved to a place where it does not produce an order violation
 *
 * @note its still possible that an order violation is present in the case
 *       of order violation in the other direction (no cycle detection).
 */
void BackendBuilder::sort ()
{
	KeySet deps;
	size_t i = 0;
	for (auto const & ps : toAdd)
	{
		Key dep ("/" + ps.getName (), KEY_END);
		if (ps.getName () != ps.getRefName ())
		{
			dep.addBaseName (ps.getRefName ());
		}
		deps.append (dep);
		dep.set<size_t> (i);
		dep.setMeta<size_t> ("order", i);
		++i;
	}

	std::unordered_set<std::string> addedDeps;
	for (auto const & ps : toAdd)
	{
		std::stringstream ss (pluginDatabase->lookupInfo (ps, "ordering"));
		std::string order;
		while (ss >> order)
		{
			if (addedDeps.find (order) != addedDeps.end ())
			{
				continue;
			}

			addedDeps.insert (order);

			// check if dependency is relevant (occurs in KeySet)
			for (auto const & self : deps)
			{
				const size_t jumpSlash = 1;
				std::string n = self.getName ();
				std::string name (n.begin () + jumpSlash, n.end ());

				bool hasProvides = false;
				/* TODO: should also take care of provides
				   implementation below would self-conflict on multiple same providers
				std::string provides = pluginDatabase->lookupInfo (PluginSpec(name), "provides");
				std::istringstream ss2 (provides);
				std::string provide;
				while (ss2 >> provide)
				{
					if (provide == name)
					{
						hasProvides = true;
					}
				}
				*/

				if ((name.length () >= order.length () && std::equal (order.begin (), order.end (), name.begin ())) ||
				    hasProvides)
				{
					// is relevant, add this instance of dep to every other key
					// add reverse dep of every key to self
					for (auto const & k : deps)
					{
						if (k == self) continue;
						ckdb::elektraMetaArrayAdd (*self, "dep", (k.getName ()).c_str ());
					}
				}
			}
		}
	}

	// now sort by the given topology
	std::vector<ckdb::Key *> ordered;
	ordered.resize (deps.size ());
	int ret = elektraSortTopology (deps.getKeySet (), &ordered[0]);
	if (ret == 0) throw CyclicOrderingViolation ();
	if (ret == -1) throw std::logic_error ("elektraSortTopology was used wrongly");

	PluginSpecVector copy (toAdd);

	// now swap everything in toAdd as we have the indizes given in ordered
	i = 0;
	for (auto const & o : ordered)
	{
		toAdd[i] = copy[atoi (ckdb::keyString (o))];
		++i;
	}
}

void BackendBuilder::needMetadata (std::string addMetadata)
{
	std::istringstream is (addMetadata);
	std::string md;
	while (is >> md)
	{
		if (md.substr (0, sizeof ("meta:/") - 1) != "meta:/")
		{
			md = "meta:/" + md;
		}

		std::string nd;
		Key k (md.c_str (), KEY_END);
		for (auto && elem : k)
		{
			if (elem[0] == KEY_NS_META)
			{
				continue;
			}
			else if (!elem.empty () && elem[0] == '#')
			{
				// reduce array entries to #
				nd += '#';
			}
			else
			{
				nd += elem;
			}
			nd += "/";
		}

		if (!nd.empty ())
		{
			// remove last "/"
			nd = nd.substr (0, nd.size () - 1);
			metadata.insert (nd);
		}
		// ignore if it does not work! (i.e. metadata already present)
	}
}

/**
 * @brief Collect what is needed
 *
 * @param [out] needs are added here
 */
void BackendBuilder::collectNeeds (std::vector<std::string> & needs) const
{
	for (auto const & ps : toAdd)
	{
		std::stringstream ss (pluginDatabase->lookupInfo (ps, "needs"));
		std::string need;
		while (ss >> need)
		{
			needs.push_back (need);
		}
	}
}

/**
 * @brief Collect what is recommended
 *
 * @param [out] needs are added here
 */
void BackendBuilder::collectRecommends (std::vector<std::string> & recommends) const
{
	for (auto const & ps : toAdd)
	{
		std::stringstream ss (pluginDatabase->lookupInfo (ps, "recommends"));
		std::string r;
		while (ss >> r)
		{
			recommends.push_back (r);
		}
	}
}

void BackendBuilder::removeProvided (std::vector<std::string> & needs) const
{
	for (auto const & ps : toAdd)
	{
		// remove the needed plugins that are already inserted
		needs.erase (std::remove (needs.begin (), needs.end (), ps.getName ()), needs.end ());

		// remove what is already provided
		std::string provides = pluginDatabase->lookupInfo (ps, "provides");
		std::istringstream ss (provides);
		std::string toRemove;
		while (ss >> toRemove)
		{
			needs.erase (std::remove (needs.begin (), needs.end (), toRemove), needs.end ());
		}
	}
}


void BackendBuilder::removeMetadata (std::set<std::string> & needsMetadata) const
{
	for (auto const & ps : toAdd)
	{
		// remove metadata that already is provided
		std::string md = pluginDatabase->lookupInfo (ps, "metadata");
		std::istringstream ss (md);
		std::string toRemove;
		while (ss >> toRemove)
		{
			needsMetadata.erase (toRemove);
		}
	}
}

namespace
{
void removeMissing (std::vector<std::string> & recommendedPlugins, std::vector<std::string> const & missingPlugins)
{
	for (auto const & mp : missingPlugins)
	{
		recommendedPlugins.erase (std::remove (recommendedPlugins.begin (), recommendedPlugins.end (), mp));
	}
}

std::string removeArray (std::string s)
{
	/*
	std::regex e ("#_*[0-9]*");
	std::string result;
	std::regex_replace (std::back_inserter(result), s.begin(), s.end(), e, "#");
	return result;
	*/
	return s;
}

/*
TEST(Backend, x)
{
	EXPECT_EQ(removeArray("should/be/unchanged"), "should/be/unchanged");
	EXPECT_EQ(removeArray("should/be/#_12"), "should/be/#");
	EXPECT_EQ(removeArray("should/be/#__200"), "should/be/#");
	EXPECT_EQ(removeArray("should/#_20/abc/#__200"), "should/#/abc/#");
	EXPECT_EQ(removeArray("should/#_20/abc/#__204"), "should/#/abc/#");
	EXPECT_EQ(removeArray("should/_20/abc/__204"), "should/_20/abc/__204");
}
*/
} // namespace

/**
 * @brief resolve all needs that were not resolved by adding plugins.
 *
 * @warning Must only be used once after all plugins/recommends are added.
 *
 * @return the missing recommended plugins
 * @retval empty if addRecommends was false
 *
 * @see addPlugin()
 */
std::vector<std::string> BackendBuilder::resolveNeeds (bool addRecommends)
{
	// load dependency-plugins immediately
	for (auto const & ps : toAdd)
	{
		auto plugins = parseArguments (pluginDatabase->lookupInfo (ps, "plugins"));
		for (auto const & plugin : plugins)
		{
			addPlugin (plugin);
		}
	}

	std::vector<std::string> missingRecommends;

	do
	{
		collectNeeds (neededPlugins);
		collectRecommends (recommendedPlugins);

		removeProvided (neededPlugins);
		removeProvided (recommendedPlugins);
		removeMissing (recommendedPlugins, missingRecommends);
		removeMetadata (metadata);

		// leftover in needs(Metadata) is what is still needed
		// lets add first one:
		if (!neededPlugins.empty ())
		{
			addPlugin (PluginSpec (neededPlugins[0]));
			neededPlugins.erase (neededPlugins.begin ());
		}
		else if (!metadata.empty ())
		{
			std::string first = (*metadata.begin ());
			first = removeArray (first);
			addPlugin (pluginDatabase->lookupMetadata (first));
			metadata.erase (first);
		}
		else if (!recommendedPlugins.empty () && addRecommends)
		{
			PluginSpec rp (recommendedPlugins[0]);
			if (pluginDatabase->status (rp) != PluginDatabase::missing)
			{
				addPlugin (rp);
			}
			else
			{
				missingRecommends.push_back (recommendedPlugins[0]);
			}
			recommendedPlugins.erase (recommendedPlugins.begin ());
		}
	} while (!neededPlugins.empty () || !metadata.empty () || (!recommendedPlugins.empty () && addRecommends));

	return missingRecommends;
}

void BackendBuilder::needPlugin (std::string name)
{
	std::stringstream ss (name);
	std::string n;
	while (ss >> n)
	{
		neededPlugins.push_back (n);
	}
}

void BackendBuilder::recommendPlugin (std::string name)
{
	std::stringstream ss (name);
	std::string n;
	while (ss >> n)
	{
		recommendedPlugins.push_back (n);
	}
}

/**
 * @brief Add a plugin.
 *
 * @pre Needs to be a unique new name (use refname if you want to add the same module multiple times)
 *
 * Will automatically resolve virtual plugins to actual plugins.
 *
 * Also calls the checkconf function if provided by the plugin. The checkconf function has the
 * following signature: int checkconf (Key * errorKey, KeySet * config) and allows a plugin to
 * verify its configuration at mount time.
 *
 * @see resolveNeeds()
 * @param plugin
 */
void BackendBuilder::addPlugin (PluginSpec const & plugin)
{
	typedef int (*checkConfPtr) (ckdb::Key *, ckdb::KeySet *);

	for (auto & p : toAdd)
	{
		if (p.getFullName () == plugin.getFullName ())
		{
			throw PluginAlreadyInserted (plugin.getFullName ());
		}
	}

	PluginSpec newPlugin = plugin;

	// if the plugin is actually a provider use it (otherwise we will get our name back):
	PluginSpec provides = pluginDatabase->lookupProvides (plugin.getName ());
	if (provides.getName () != newPlugin.getName ())
	{
		// keep our config and refname
		newPlugin.setName (provides.getName ());
		newPlugin.appendConfig (provides.getConfig ());
	}

	// call plugin's checkconf function (if provided)
	// this enables a plugin to verify its configuration at mount time
	checkConfPtr checkConfFunction = reinterpret_cast<checkConfPtr> (pluginDatabase->getSymbol (newPlugin, "checkconf"));
	if (checkConfFunction)
	{
		ckdb::Key * errorKey = ckdb::keyNew ("/", KEY_END);

		// merge plugin config and backend config together
		ckdb::KeySet * pluginConfig = newPlugin.getConfig ().dup ();
		ckdb::ksAppend (pluginConfig, backendConf.getKeySet ());

		// call the plugin's checkconf function
		int checkResult = checkConfFunction (errorKey, pluginConfig);
		if (checkResult == -1)
		{
			ckdb::ksDel (pluginConfig);
			throw PluginConfigInvalid (errorKey);
		}
		else if (checkResult == 1)
		{
			// separate plugin config from the backend config
			ckdb::Key * backendParent = ckdb::keyNew ("system:/", KEY_END);
			ckdb::KeySet * newBackendConfig = ckdb::ksCut (pluginConfig, backendParent);

			// take over the new configuration
			KeySet modifiedPluginConfig = KeySet (pluginConfig);
			KeySet modifiedBackendConfig = KeySet (newBackendConfig);

			newPlugin.setConfig (modifiedPluginConfig);
			setBackendConfig (modifiedBackendConfig);

			ckdb::keyDel (backendParent);
		}
		else
		{
			ckdb::ksDel (pluginConfig);
		}
		ckdb::keyDel (errorKey);
	}

	toAdd.push_back (newPlugin);
	sort ();
}

void BackendBuilder::remPlugin (PluginSpec const & plugin)
{
	using namespace std::placeholders;
	PluginSpecFullName cmp;
	toAdd.erase (std::remove_if (toAdd.begin (), toAdd.end (), std::bind (cmp, plugin, _1)));
}

void BackendBuilder::fillPlugins (BackendInterface & b) const
{
	for (auto const & plugin : toAdd)
	{
		b.addPlugin (plugin);
	}
}

void BackendBuilder::setBackendConfig (KeySet const & ks)
{
	backendConf = ks;
}

KeySet BackendBuilder::getBackendConfig ()
{
	return backendConf;
}

GlobalPluginsBuilder::GlobalPluginsBuilder (BackendBuilderInit const & bbi) : BackendBuilder (bbi)
{
}

void GlobalPluginsBuilder::serialize (kdb::KeySet & ret)
{
	GlobalPlugins gp;
	fillPlugins (gp);
	return gp.serialize (ret);
}

/**
 * @brief Below this path is the configuration for global plugins
 */
const char * const GlobalPluginsBuilder::globalPluginsPath = "system:/elektra/globalplugins";


void MountBackendBuilder::status (std::ostream & os) const
{
	try
	{
		MountBackendInterfacePtr b = getBackendFactory ().create ();
		fillPlugins (*b);
		return b->status (os);
	}
	catch (std::exception const & pce)
	{
		os << "Could not successfully add plugin: " << pce.what () << std::endl;
	}
}

bool MountBackendBuilder::validated () const
{
	try
	{
		MountBackendInterfacePtr b = getBackendFactory ().create ();
		fillPlugins (*b);
		return b->validated ();
	}
	catch (...)
	{
		return false;
	}
}

void MountBackendBuilder::setMountpoint (Key mountpoint_, KeySet mountConf_)
{
	mountpoint = mountpoint_;
	mountConf = mountConf_;

	MountBackendInterfacePtr mbi = getBackendFactory ().create ();
	mbi->setMountpoint (mountpoint, mountConf);
}

std::string MountBackendBuilder::getMountpoint () const
{
	return mountpoint.getName ();
}

void MountBackendBuilder::setBackendConfig (KeySet const & ks)
{
	BackendBuilder::setBackendConfig (ks);
}

void MountBackendBuilder::useConfigFile (std::string file)
{
	configfile = file;

	MountBackendInterfacePtr b = getBackendFactory ().create ();
	bool checkPossible = false;
	for (auto const & p : *this)
	{
		if ("resolver" == getPluginDatabase ()->lookupInfo (p, "provides"))
		{
			checkPossible = true;
		}
	}

	if (!checkPossible) return;
	fillPlugins (*b);
	b->useConfigFile (configfile);
}

std::string MountBackendBuilder::getConfigFile () const
{
	return configfile;
}

void MountBackendBuilder::serialize (kdb::KeySet & ret)
{
	MountBackendInterfacePtr mbi = getBackendFactory ().create ();
	fillPlugins (*mbi);
	mbi->setMountpoint (mountpoint, mountConf);
	mbi->setBackendConfig (backendConf);
	mbi->useConfigFile (configfile);
	mbi->serialize (ret);
}
} // namespace tools
} // namespace kdb
