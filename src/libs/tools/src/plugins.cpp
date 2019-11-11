/**
 * @file
 *
 * @brief Implementation of set/get/error plugins
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include <helper/keyhelper.hpp>
#include <plugins.hpp>

#include <kdbprivate.h>

#include <algorithm>
#include <iterator>

using namespace std;

namespace kdb
{

namespace tools
{

Plugins::Plugins () : plugins (NR_OF_SET_PLUGINS), nrStoragePlugins (0), nrResolverPlugins (0)
{
	placementInfo["prerollback"] = ERROR_PREROLLBACK;
	placementInfo["rollback"] = ERROR_ROLLBACK;
	placementInfo["postrollback"] = ERROR_POSTROLLBACK;

	placementInfo["getresolver"] = GET_RESOLVER;
	placementInfo["pregetstorage"] = GET_PREGETSTORAGE;
	placementInfo["getstorage"] = GET_GETSTORAGE;
	placementInfo["postgetstorage"] = GET_POSTGETSTORAGE;

	placementInfo["setresolver"] = SET_SETRESOLVER;
	placementInfo["presetstorage"] = SET_PRESETSTORAGE;
	placementInfo["setstorage"] = SET_SETSTORAGE;
	placementInfo["precommit"] = SET_PRECOMMIT;
	placementInfo["commit"] = SET_COMMIT;
	placementInfo["postcommit"] = SET_POSTCOMMIT;
}

void Plugins::addInfo (Plugin & plugin)
{
	{
		std::string provide;
		std::stringstream ss (plugin.lookupInfo ("provides"));
		while (ss >> provide)
		{
			alreadyProvided.push_back (provide);
		}
		/* Push back the name of the plugin itself */
		alreadyProvided.push_back (plugin.name ());
	}

	{
		std::string need;
		std::stringstream ss (plugin.lookupInfo ("needs"));
		while (ss >> need)
		{
			needed.push_back (need);
		}
	}

	{
		std::string recommend;
		std::stringstream ss (plugin.lookupInfo ("recommends"));
		while (ss >> recommend)
		{
			recommended.push_back (recommend);
		}
	}

	{
		std::string conflict;
		std::stringstream ss (plugin.lookupInfo ("conflicts"));
		while (ss >> conflict)
		{
			alreadyConflict.push_back (conflict);
		}
	}
}

void Plugins::addPlugin (Plugin & plugin, std::string which)
{
	if (!plugin.findInfo (which, "placements")) return;

	std::string stacking = plugin.lookupInfo ("stacking");

	if (which == "postgetstorage" && stacking == "")
	{
		if (!plugins[placementInfo[which]])
		{
			plugins[placementInfo[which]] = static_cast<Slot *> (ckdb::elektraMalloc (sizeof (Slot)));
			plugins[placementInfo[which]]->value=&plugin;
			plugins[placementInfo[which]]->next = NULL;
			return;
		}
		Slot * cur = static_cast<Slot *> (ckdb::elektraMalloc (sizeof (Slot)));
		cur->value=&plugin;
		cur->next = plugins[placementInfo[which]];
		plugins[placementInfo[which]] = cur;
		return;
	}

	addPluginToSlot (&plugin, which);
}

/**
 * @brief check if this plugin can be placed in the unfortunately
 * limited number of slots
 *
 * @param plugin the plugin to check
 * @param which placementInfo it is
 *
 * @retval true if it should be added
 * @retval false no placements (will not be added)
 */
bool Plugins::checkPlacement (Plugin & plugin, std::string which)
{
	if (!plugin.findInfo (which, "placements")) return false; // nothing to check, won't be added anyway

	return true;
}

bool Plugins::validateProvided () const
{
	return getNeededMissing ().empty ();
}

std::vector<std::string> Plugins::getNeededMissing () const
{
	std::vector<std::string> ret;
	for (auto & elem : needed)
	{
		std::string need = elem;
		if (std::find (alreadyProvided.begin (), alreadyProvided.end (), need) == alreadyProvided.end ())
		{
			ret.push_back (need);
		}
	}
	return ret;
}

std::vector<std::string> Plugins::getRecommendedMissing () const
{
	std::vector<std::string> ret;
	for (auto & elem : recommended)
	{
		std::string recommend = elem;
		if (std::find (alreadyProvided.begin (), alreadyProvided.end (), recommend) == alreadyProvided.end ())
		{
			ret.push_back (recommend);
		}
	}
	return ret;
}

void Plugins::checkStorage (Plugin & plugin)
{
	if (plugin.findInfo ("storage", "provides"))
	{
		++nrStoragePlugins;
	}

	if (nrStoragePlugins > 1)
	{
		--nrStoragePlugins;
		throw StoragePlugin ();
	}
}

void Plugins::checkResolver (Plugin & plugin)
{
	if (plugin.findInfo ("resolver", "provides"))
	{
		++nrResolverPlugins;
	}


	if (nrResolverPlugins > 1)
	{
		--nrResolverPlugins;
		throw ResolverPlugin ();
	}
}


/** Check ordering of plugins.
 */
void Plugins::checkOrdering (Plugin & plugin)
{
	std::string order;
	std::stringstream ss (plugin.lookupInfo ("ordering"));
	while (ss >> order)
	{
		/* Simple look in the already provided names.
		 * Because both plugin names + provided names are
		 * there.
		 * If it is found, we have an ordering violation.
		 */
		if (std::find (alreadyProvided.begin (), alreadyProvided.end (), order) != alreadyProvided.end ())
		{
			throw OrderingViolation ();
		}
	}
}

/** Check conflicts of plugins.
 */
void Plugins::checkConflicts (Plugin & plugin)
{
	{
		std::string order;
		std::stringstream ss (plugin.lookupInfo ("conflicts"));
		while (ss >> order)
		{
			/* Simple look in the already provided names.
			 * Because both plugin names + provided names are
			 * there.
			 * If one is found, we have an conflict.
			 */
			if (std::find (alreadyProvided.begin (), alreadyProvided.end (), order) != alreadyProvided.end ())
			{
				throw ConflictViolation ();
			}
		}
	}

	/* Is there a conflict against the name? */
	if (std::find (alreadyConflict.begin (), alreadyConflict.end (), plugin.name ()) != alreadyConflict.end ())
	{
		throw ConflictViolation ();
	}

	/* Is there a conflict against what it provides? */
	std::string order;
	std::stringstream ss (plugin.lookupInfo ("provides"));
	while (ss >> order)
	{
		if (std::find (alreadyConflict.begin (), alreadyConflict.end (), order) != alreadyConflict.end ())
		{
			throw ConflictViolation ();
		}
	}
}
void Plugins::addPluginToSlot (Plugin * plugin, std::string which)
{
	if (!plugins[placementInfo[which]])
	{
		plugins[placementInfo[which]] = static_cast<Slot *> (ckdb::elektraMalloc (sizeof (Slot)));
		plugins[placementInfo[which]]->value=plugin;
		plugins[placementInfo[which]]->next = NULL;

		return;
	}
	Slot * cur = plugins[placementInfo[which]];
	while (cur->next)
	{
		cur=cur->next;
	}
	cur->next = static_cast<Slot *> (ckdb::elektraMalloc (sizeof (Slot)));
	cur->next->value=plugin;
	cur->next->next = NULL;
}


void ErrorPlugins::tryPlugin (Plugin & plugin)
{
	checkOrdering (plugin);
	checkConflicts (plugin);

	bool willBeAdded = false;
	willBeAdded |= checkPlacement (plugin, "prerollback");
	willBeAdded |= checkPlacement (plugin, "rollback");
	willBeAdded |= checkPlacement (plugin, "postrollback");
	if (!willBeAdded) return;

	if (!plugin.getSymbol ("error"))
	{
		throw MissingSymbol ("error");
	}

	checkResolver (plugin);
}


void GetPlugins::tryPlugin (Plugin & plugin)
{
	bool willBeAdded = false;
	willBeAdded |= checkPlacement (plugin, "getresolver");
	willBeAdded |= checkPlacement (plugin, "pregetstorage");
	willBeAdded |= checkPlacement (plugin, "getstorage");
	willBeAdded |= checkPlacement (plugin, "postgetstorage");
	if (!willBeAdded) return;

	if (!plugin.getSymbol ("get"))
	{
		throw MissingSymbol ("get");
	}

	checkStorage (plugin);
	checkResolver (plugin);
}

void SetPlugins::tryPlugin (Plugin & plugin)
{
	bool willBeAdded = false;
	willBeAdded |= checkPlacement (plugin, "setresolver");
	willBeAdded |= checkPlacement (plugin, "presetstorage");
	willBeAdded |= checkPlacement (plugin, "setstorage");
	willBeAdded |= checkPlacement (plugin, "precommit");
	willBeAdded |= checkPlacement (plugin, "commit");
	willBeAdded |= checkPlacement (plugin, "postcommit");
	if (!willBeAdded) return;

	if (!plugin.getSymbol ("set"))
	{
		throw MissingSymbol ("set");
	}


	checkStorage (plugin);
	checkResolver (plugin);
}


void ErrorPlugins::addPlugin (Plugin & plugin)
{
	Plugins::addPlugin (plugin, "prerollback");
	Plugins::addPlugin (plugin, "rollback");
	Plugins::addPlugin (plugin, "postrollback");

	Plugins::addInfo (plugin);
}

void GetPlugins::addPlugin (Plugin & plugin)
{
	Plugins::addPlugin (plugin, "getresolver");
	Plugins::addPlugin (plugin, "pregetstorage");
	Plugins::addPlugin (plugin, "getstorage");
	Plugins::addPlugin (plugin, "postgetstorage");
}

void SetPlugins::addPlugin (Plugin & plugin)
{
	Plugins::addPlugin (plugin, "setresolver");
	Plugins::addPlugin (plugin, "presetstorage");
	Plugins::addPlugin (plugin, "setstorage");
	Plugins::addPlugin (plugin, "precommit");
	Plugins::addPlugin (plugin, "commit");
	Plugins::addPlugin (plugin, "postcommit");
}


void ErrorPlugins::status (std::ostream & os) const
{
	std::vector<std::string> n = getNeededMissing ();
	if (!n.empty ())
	{
		os << "Needed plugins that are missing are: ";
		std::copy (n.begin (), n.end (), std::ostream_iterator<std::string> (os, " "));
		os << std::endl;
	}
	std::vector<std::string> r = getRecommendedMissing ();
	if (!r.empty ())
	{
		os << "Recommendations that are not fulfilled are: ";
		std::copy (r.begin (), r.end (), std::ostream_iterator<std::string> (os, " "));
		os << std::endl;
	}
}


bool ErrorPlugins::validated () const
{
	return nrResolverPlugins == 1 && validateProvided ();
}

bool GetPlugins::validated () const
{
	return nrStoragePlugins == 1 && nrResolverPlugins == 1;
}

bool SetPlugins::validated () const
{
	return nrStoragePlugins == 1 && nrResolverPlugins == 1;
}


namespace
{
void serializeConfig (std::string name, KeySet const & ks, KeySet & ret)
{
	if (!ks.size ()) return;

	Key oldParent ("user", KEY_END);
	Key newParent (name + "/config", KEY_END);

	ret.append (newParent);

	for (KeySet::iterator i = ks.begin (); i != ks.end (); ++i)
	{
		Key k (i->dup ());
		if (k.getNamespace () == "user") ret.append (kdb::tools::helper::rebaseKey (k, oldParent, newParent));
	}
}
} // namespace


void ErrorPlugins::serialise (Key & baseKey, KeySet & ret)
{
	ret.append (*Key (baseKey.getName () + "/error", KEY_COMMENT, "List of plugins to use", KEY_END));

	for (int i = 0; i < NR_OF_ERROR_PLUGINS; ++i)
	{
		if (!plugins[i]) continue;

		std::string roleName;

		switch (i)
		{
		case ERROR_PREROLLBACK:
			roleName = "prerollback";
			break;
		case ERROR_ROLLBACK:
			roleName = "rollback";
			break;
		default:
			roleName = "postrollback";
		}

		std::string comment = "List of plugins fulfilling the role " + roleName;

		ret.append (*Key (baseKey.getName () + "/error/" + roleName, KEY_COMMENT, comment.c_str (), KEY_END));

		int position = 0;
		Slot * current = plugins[i];
		while(current)
		{
			if(current->value)
			{
				bool fr = current->value->firstRef;

				std::ostringstream posNumber;
				posNumber << position++;

				std::string name = baseKey.getName () + "/error/" + roleName + "/#" + posNumber.str ();
				std::string refName = current->value->refname ();
				std::string pluginName = current->value->name ();

				comment = "Plugin fulfilling the role " + roleName + " at position " + posNumber.str ();

				ret.append (*Key (name, KEY_COMMENT, comment.c_str (), KEY_END));

				if (fr)
				{
					comment = "Label of plugin fulfilling the role " + roleName + " at position " + posNumber.str ();
					ret.append (*Key (name + "/label", KEY_VALUE, refName.c_str (), KEY_COMMENT, comment.c_str (), KEY_END));
					comment = "Name of plugin fulfilling the role " + roleName + " at position " + posNumber.str ();
					ret.append (*Key (name + "/name", KEY_VALUE, pluginName.c_str (), KEY_COMMENT, comment.c_str (), KEY_END));
				}
				else
				{
					comment = "Reference name of plugin fulfilling the role " + roleName + " at position " + posNumber.str ();
					ret.append (*Key (name + "/reference", KEY_VALUE, refName.c_str (), KEY_COMMENT, comment.c_str (), KEY_END));
				}

				if (fr) serializeConfig (name, current->value->getConfig (), ret);
			}
			current = current->next;
		}
	}
}

void GetPlugins::serialise (Key & baseKey, KeySet & ret)
{
	ret.append (*Key (baseKey.getName () + "/get", KEY_COMMENT, "List of plugins to use", KEY_END));

	for (int i = 0; i < NR_OF_GET_PLUGINS; ++i)
	{
		if (!plugins[i]) continue;

		std::string roleName;

		switch (i)
		{
		case GET_RESOLVER:
			roleName = "getresolver";
			break;
		case GET_PREGETSTORAGE:
			roleName = "pregetstorage";
			break;
		case GET_GETSTORAGE:
			roleName = "getstorage";
			break;
		default:
			roleName = "postgetstorage";
		}

		std::string comment = "List of plugins fulfilling the role " + roleName;

		ret.append (*Key (baseKey.getName () + "/get/" + roleName, KEY_COMMENT, comment.c_str (), KEY_END));

		int position = 0;
		Slot * current = plugins[i];
		while(current)
		{
			if(current->value)
			{
				bool fr = current->value->firstRef;

				std::ostringstream posNumber;
				posNumber << position++;

				std::string name = baseKey.getName () + "/get/" + roleName + "/#" + posNumber.str ();
				std::string refName = current->value->refname ();
				std::string pluginName = current->value->name ();

				comment = "Plugin fulfilling the role " + roleName + " at position " + posNumber.str ();
				ret.append (*Key (name, KEY_COMMENT, comment.c_str (), KEY_END));
				if (fr)
				{
					comment = "Label of plugin fulfilling the role " + roleName + " at position " + posNumber.str ();
					ret.append (*Key (name + "/label", KEY_VALUE, refName.c_str (), KEY_COMMENT, comment.c_str (), KEY_END));
					comment = "Name of plugin fulfilling the role " + roleName + " at position " + posNumber.str ();
					ret.append (*Key (name + "/name", KEY_VALUE, pluginName.c_str (), KEY_COMMENT, comment.c_str (), KEY_END));
				}
				else
				{
					comment = "Reference name of plugin fulfilling the role " + roleName + " at position " + posNumber.str ();
					ret.append (*Key (name + "/reference", KEY_VALUE, refName.c_str (), KEY_COMMENT, comment.c_str (), KEY_END));
				}
				if (fr) serializeConfig (name, current->value->getConfig (), ret);
			}
			current = current->next;
		}
	}
}


void SetPlugins::serialise (Key & baseKey, KeySet & ret)
{
	ret.append (*Key (baseKey.getName () + "/set", KEY_COMMENT, "List of plugins to use", KEY_END));

	for (int i = 0; i < NR_OF_SET_PLUGINS; ++i)
	{
		if (!plugins[i]) continue;

		std::string roleName;

		switch (i)
		{
		case SET_SETRESOLVER:
			roleName = "setresolver";
			break;
		case SET_PRESETSTORAGE:
			roleName = "presetstorage";
			break;
		case SET_SETSTORAGE:
			roleName = "setstorage";
			break;
		case SET_PRECOMMIT:
			roleName = "precommit";
			break;
		case SET_COMMIT:
			roleName = "commit";
			break;
		default:
			roleName = "postcommit";
		}

		std::string comment = "List of plugins fulfilling the role " + roleName;

		ret.append (*Key (baseKey.getName () + "/set/" + roleName, KEY_COMMENT, comment.c_str (), KEY_END));

		int position = 0;
		Slot * current = plugins[i];
		while(current)
		{
			if(current->value)
			{
				bool fr = current->value->firstRef;

				std::ostringstream posNumber;
				posNumber << position++;

				std::string name = baseKey.getName () + "/set/" + roleName + "/#" + posNumber.str ();
				std::string refName = current->value->refname ();
				std::string pluginName = current->value->name ();

				comment = "Plugin fulfilling the role " + roleName + " at position " + posNumber.str ();
				ret.append (*Key (name, KEY_COMMENT, comment.c_str (), KEY_END));
				if (fr)
				{
					comment = "Label of plugin fulfilling the role " + roleName + " at position " + posNumber.str ();
					ret.append (*Key (name + "/label", KEY_VALUE, refName.c_str (), KEY_COMMENT, comment.c_str (), KEY_END));
					comment = "Name of plugin fulfilling the role " + roleName + " at position " + posNumber.str ();
					ret.append (*Key (name + "/name", KEY_VALUE, pluginName.c_str (), KEY_COMMENT, comment.c_str (), KEY_END));
				}
				else
				{
					comment = "Reference name of plugin fulfilling the role " + roleName + " at position " + posNumber.str ();
					ret.append (*Key (name + "/reference", KEY_VALUE, refName.c_str (), KEY_COMMENT, comment.c_str (), KEY_END));
				}
				if (fr) serializeConfig (name, current->value->getConfig (), ret);
			}
			current = current->next;
		}
	}
}
} // namespace tools
} // namespace kdb
