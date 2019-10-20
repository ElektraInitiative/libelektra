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

Plugins::Plugins () : plugins (NR_OF_PLUGINS), nrStoragePlugins (0), nrResolverPlugins (0)
{
	placementInfo["prerollback"] = Place (ERROR_PREROLLBACK, ERROR_PREROLLBACK);
	placementInfo["rollback"] = Place (ERROR_ROLLBACK, ERROR_ROLLBACK);
	placementInfo["postrollback"] = Place (ERROR_POSTROLLBACK, ERROR_POSTROLLBACK);

	placementInfo["resolver"] = Place (GET_RESOLVER, GET_RESOLVER);
	placementInfo["prestorage"] = Place (GET_PRESTORAGE, GET_PRESTORAGE);
	placementInfo["storage"] = Place (GET_STORAGE, GET_STORAGE);
	placementInfo["poststorage"] = Place (GET_POSTSTORAGE, GET_POSTSTORAGE);
	revPostGet = NR_OF_PLUGINS - 1;

	placementInfo["resolver"] = Place (SET_RESOLVER, SET_RESOLVER);
	placementInfo["precommit"] = Place (SET_PRECOMMIT, SET_PRECOMMIT);
	placementInfo["commit"] = Place (SET_COMMIT, SET_COMMIT);
	placementInfo["postcommit"] = Place (SET_POSTCOMMIT, SET_POSTCOMMIT);
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

	Slot * current;
	if (which == "poststorage" && stacking == "")
	{
		current = plugins[revPostGet--];
	}
	else
	{
		current = plugins[placementInfo[which].current++];
	}

	while(current != nullptr)
	{
		current = current->next;
	}
	current = static_cast<Slot*>(malloc (sizeof (Slot)));
	current->value = &plugin;
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


void ErrorPlugins::tryPlugin (Plugin & plugin)
{
	checkOrdering (plugin);
	checkConflicts (plugin);

	if (!plugin.getSymbol ("error"))
	{
		throw MissingSymbol ("error");
	}

	checkResolver (plugin);
}


void GetPlugins::tryPlugin (Plugin & plugin)
{

	if (!plugin.getSymbol ("get"))
	{
		throw MissingSymbol ("get");
	}

	checkStorage (plugin);
	checkResolver (plugin);
}

void SetPlugins::tryPlugin (Plugin & plugin)
{

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
	Plugins::addPlugin (plugin, "resolver");
	Plugins::addPlugin (plugin, "prestorage");
	Plugins::addPlugin (plugin, "storage");
	Plugins::addPlugin (plugin, "poststorage");
}

void SetPlugins::addPlugin (Plugin & plugin)
{
	Plugins::addPlugin (plugin, "resolver");
	Plugins::addPlugin (plugin, "prestorage");
	Plugins::addPlugin (plugin, "storage");
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
		if (plugins[i] == nullptr) continue;

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
		while(current != nullptr)
		{
			if(current->value != nullptr)
			{
				bool fr = current->value->firstRef;

				std::ostringstream posNumber;
				posNumber << position++;

				std::string name = baseKey.getName () + "/error/" + roleName + "/#" + posNumber.str ();
				std::string refName = current->value->refname ();

				comment = "Plugin fulfilling the role " + roleName + " at position " + posNumber.str ();
				ret.append (*Key (name, KEY_COMMENT, comment.c_str (), KEY_END));
				comment = "Reference name of plugin fulfilling the role " + roleName + " at position " + posNumber.str ();
				ret.append (*Key (name + "/reference", KEY_VALUE, refName.c_str (), KEY_COMMENT, comment.c_str (), KEY_END));
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
		if (plugins[i] == nullptr) continue;

		std::string roleName;

		switch (i)
		{
		case GET_RESOLVER:
			roleName = "resolver";
			break;
		case GET_PRESTORAGE:
			roleName = "prestorage";
			break;
		case GET_STORAGE:
			roleName = "storage";
			break;
		default:
			roleName = "poststorage";
		}

		std::string comment = "List of plugins fulfilling the role " + roleName;

		ret.append (*Key (baseKey.getName () + "/get/" + roleName, KEY_COMMENT, comment.c_str (), KEY_END));

		int position = 0;
		Slot * current = plugins[i];
		while(current != nullptr)
		{
			if(current->value != nullptr)
			{
				bool fr = current->value->firstRef;

				std::ostringstream posNumber;
				posNumber << position++;

				std::string name = baseKey.getName () + "/get/" + roleName + "/#" + posNumber.str ();
				std::string refName = current->value->refname ();

				comment = "Plugin fulfilling the role " + roleName + " at position " + posNumber.str ();
				ret.append (*Key (name, KEY_COMMENT, comment.c_str (), KEY_END));
				comment = "Reference name of plugin fulfilling the role " + roleName + " at position " + posNumber.str ();
				ret.append (*Key (name + "/reference", KEY_VALUE, refName.c_str (), KEY_COMMENT, comment.c_str (), KEY_END));
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
		if (plugins[i] == nullptr) continue;

		std::string roleName;

		switch (i)
		{
		case SET_RESOLVER:
			roleName = "resolver";
			break;
		case SET_PRESTORAGE:
			roleName = "prestorage";
			break;
		case SET_STORAGE:
			roleName = "storage";
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
		while(current != nullptr)
		{
			if(current->value != nullptr)
			{
				bool fr = current->value->firstRef;

				std::ostringstream posNumber;
				posNumber << position++;

				std::string name = baseKey.getName () + "/set/" + roleName + "/#" + posNumber.str ();
				std::string refName = current->value->refname ();

				comment = "Plugin fulfilling the role " + roleName + " at position " + posNumber.str ();
				ret.append (*Key (name, KEY_COMMENT, comment.c_str (), KEY_END));
				comment = "Reference name of plugin fulfilling the role " + roleName + " at position " + posNumber.str ();
				ret.append (*Key (name + "/reference", KEY_VALUE, refName.c_str (), KEY_COMMENT, comment.c_str (), KEY_END));
				if (fr) serializeConfig (name, current->value->getConfig (), ret);
			}
			current = current->next;
		}
	}
}
} // namespace tools
} // namespace kdb
