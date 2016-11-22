/**
 * @file
 *
 * @brief Source for simplespeclang plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include "simplespeclang.hpp"

using namespace ckdb;

#include <kdbease.h>
#include <kdberrors.h>

#include <kdbmeta.h>

#include <algorithm>
#include <fstream>
#include <iostream>
#include <sstream>


namespace
{

std::string getConfigEnum (Plugin * handle)
{
	Key * k = ksLookupByName (elektraPluginGetConfig (handle), "/keyword/enum", 0);
	if (!k) return "enum";

	return keyString (k);
}

std::string getConfigAssign (Plugin * handle)
{
	Key * k = ksLookupByName (elektraPluginGetConfig (handle), "/keyword/assign", 0);
	if (!k) return "=";

	return keyString (k);
}

bool startsWith (std::string const & str, std::string const & start)
{
	return std::equal (start.begin (), start.end (), str.begin ());
}

int serialise (std::ostream & os, ckdb::Key * parentKey, ckdb::KeySet * ks, Plugin * handle)
{
	ckdb::Key * cur;

	ksRewind (ks);
	while ((cur = ksNext (ks)) != nullptr)
	{
		const ckdb::Key * meta = 0;
		if (!keyCmp (cur, parentKey))
		{
			meta = keyGetMeta (cur, "mountpoint");
			if (meta)
			{
				os << "mountpoint " << keyString (meta) << "\n";
			}
			meta = keyGetMeta (cur, "infos/plugins");
			if (meta)
			{
				os << "plugins " << keyString (meta) << "\n";
			}
			continue;
		}
		meta = ckdb::keyGetMeta (cur, "check/enum");
		if (!meta) continue;

		os << getConfigEnum (handle) << " ";
		os << elektraKeyGetRelativeName (cur, parentKey) << " ";
		os << getConfigAssign (handle);
		keyRewindMeta (cur);
		while ((meta = keyNextMeta (cur)))
		{
			if (startsWith (keyName (meta), "check/enum/#"))
			{
				os << " " << ckdb::keyString (meta);
			}
		}
		os << "\n";
	}

	return 1;
}

int unserialise (std::istream & is, ckdb::Key * errorKey, ckdb::KeySet * ks, Plugin * handle)
{
	Key * cur = nullptr;

	Key * parent = keyNew (keyName (errorKey), KEY_END);

	std::string line;
	while (std::getline (is, line))
	{
		std::string read;
		std::stringstream ss (line);
		ss >> read;
		if (read == "mountpoint")
		{
			ss >> read;
			keySetMeta (parent, "mountpoint", read.c_str ());
			continue;
		}
		else if (read == "plugins")
		{
			std::string plugins;
			while (ss >> read)
			{
				// replace (read.begin(), read.end(), '_', ' ');
				plugins += read + " ";
			}
			keySetMeta (parent, "infos/plugins", plugins.c_str ());
			continue;
		}
		else if (read != getConfigEnum (handle))
		{
			ELEKTRA_LOG ("not an enum %s", read.c_str ());
			continue;
		}

		std::string name;
		ss >> name;
		ELEKTRA_LOG ("key name for enum is %s", name.c_str ());
		cur = keyNew (keyName (errorKey), KEY_END);
		keyAddName (cur, name.c_str ());

		ss >> read;
		if (read != getConfigAssign (handle))
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_PARSE, errorKey, "Expected assignment (%s), but got (%s)",
					    getConfigAssign (handle).c_str (), read.c_str ());
			continue;
		}

		int added = 0;
		while (ss >> read)
		{
			++added;
			elektraMetaArrayAdd (cur, "check/enum", read.c_str ());
		}

		if (added)
		{
			ELEKTRA_LOG ("%s is enum with %d entries (last char is %c)", name.c_str (), added, name.back ());
			keySetMeta (cur, "check/enum", "#");
			if (name.back () == '*')
			{
				keySetMeta (cur, "check/enum/multi", " ");
			}
		}

		keySetMeta (cur, "require", "1");
		keySetMeta (cur, "conflict/set/missing", "ERROR");
		keySetMeta (cur, "mandatory", "1");

		ckdb::ksAppendKey (ks, cur);
	}

	ckdb::ksAppendKey (ks, parent);

	return 1;
}
}

extern "C" {

int elektraSimplespeclangGet (Plugin * handle, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/simplespeclang"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/simplespeclang", KEY_VALUE,
					   "simplespeclang plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/simplespeclang/exports", KEY_END),
			       keyNew ("system/elektra/modules/simplespeclang/exports/get", KEY_FUNC, elektraSimplespeclangGet, KEY_END),
			       keyNew ("system/elektra/modules/simplespeclang/exports/set", KEY_FUNC, elektraSimplespeclangSet, KEY_END),
			       keyNew ("system/elektra/modules/simplespeclang/exports/checkconf", KEY_FUNC,
				       elektraSimplespeclangCheckConfig, KEY_END),
#include ELEKTRA_README (simplespeclang)
			       keyNew ("system/elektra/modules/simplespeclang/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}

	std::ifstream ofs (keyString (parentKey), std::ios::binary);
	if (!ofs.is_open ())
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		return -1;
	}

	return unserialise (ofs, parentKey, returned, handle);
}

int elektraSimplespeclangSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	ELEKTRA_LOG ("opening file %s", keyString (parentKey));
	std::ofstream ofs (keyString (parentKey), std::ios::binary);
	if (!ofs.is_open ())
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		return -1;
	}

	return serialise (ofs, parentKey, returned, handle);
}

int elektraSimplespeclangCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (simplespeclang)
{
	// clang-format off
	return elektraPluginExport ("simplespeclang",
		ELEKTRA_PLUGIN_GET,	&elektraSimplespeclangGet,
		ELEKTRA_PLUGIN_SET,	&elektraSimplespeclangSet,
		ELEKTRA_PLUGIN_END);
}

}
