/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include <errno.h>
#include <regex.h>

#include <fstream>
#include <streambuf>
#include <string>

#include "regexstore.h"

using namespace ckdb;

#include <kdberrors.h>

int elektraRegexstoreOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	/* plugin initialization logic */

	return 1; /* success */
}

int elektraRegexstoreClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	/* free all plugin resources and shut it down */

	return 1; /* success */
}

std::string elektraRegexstorePos (std::string const & str, int offset, std::string const & text, regmatch_t * offsets, char index,
				  Key * parentKey)
{
	int pos = index - '0';
	if (pos < 0 || pos > 9)
	{
		ELEKTRA_ADD_WARNINGF (96, parentKey, "Regex Group %d for %s not between 0 and 9 read from %s", pos, text.c_str (), &index);
		return std::string ("");
	}
	if (offsets[pos].rm_so == -1)
	{
		ELEKTRA_ADD_WARNINGF (96, parentKey, "Regex Group %d missing for %s", pos, text.c_str ());
		return std::string ("");
	}
	return str.substr (offset + offsets[pos].rm_so, offsets[pos].rm_eo - offsets[pos].rm_so);
}

/** e.g.
configKey = keyNew ("user/map/#2",
		KEY_VALUE, "#1 map ([^ \n]*) ([^ \n]*)",
		KEY_META, "meta", "1",
		KEY_END);
*/
Key * elektraRegexstoreProcess (Key * configKey, int * offset, std::string const & str, Key * parentKey)
{
	regex_t regex;
	size_t nmatch = 10;
	regmatch_t offsets[10];
	std::string configString = keyString (configKey);
	if (configString.length () < 3 && configString[0] != '#' && (configString[1] < '0' || configString[1] > '9') &&
	    configString[2] != ' ')
	{
		ELEKTRA_ADD_WARNINGF (96, parentKey, "String %s of %s did not start with #<number><space>", configString.c_str (),
				      keyName (configKey));
		return nullptr;
	}

	int ret = regcomp (&regex, configString.c_str () + 3, REG_EXTENDED);

	if (ret != 0)
	{
		char buffer[1000];
		regerror (ret, &regex, buffer, 999);
		ELEKTRA_ADD_WARNINGF (96, parentKey, "Could not compile regex %s, because: %s", configString.c_str () + 3, buffer);
		regfree (&regex);
		return nullptr;
	}


	ret = regexec (&regex, str.c_str () + *offset, nmatch, offsets, 0);

	if (ret == REG_NOMATCH)
	{
		return nullptr;
	}

	if (ret != 0)
	{
		char buffer[1000];
		regerror (ret, &regex, buffer, 999);
		ELEKTRA_ADD_WARNINGF (96, parentKey, "Regex exec returned error (not in manual for linux), because: %s", buffer);
		regfree (&regex);
		return nullptr;
	}

	std::string keyname;
	// skip user/regexstore ..
	if (!strncmp (keyName (configKey), "user", 4))
	{
		keyname = keyName (configKey) + 15;
	}
	else if (!strncmp (keyName (configKey), "system", 6))
	{
		keyname = keyName (configKey) + 17;
	}
	std::string newkeyname;

	for (size_t i = 0; i < keyname.size (); ++i)
	{
		if (keyname[i] == '#')
		{
			++i;
			newkeyname +=
				elektraRegexstorePos (str, *offset, std::string ("keyname ") + keyname, offsets, keyname[i], parentKey);
		}
		else
		{
			newkeyname += keyname[i];
		}
	}

	Key * toAppend = keyNew (keyName (parentKey), KEY_END);
	keyAddName (toAppend, newkeyname.c_str ());

	keySetString (toAppend,
		      elektraRegexstorePos (str, *offset, "keystring of " + newkeyname, offsets, configString[1], parentKey).c_str ());

	keyRewindMeta (configKey);
	while (keyNextMeta (configKey))
	{
		keySetMeta (toAppend, keyName (keyCurrentMeta (configKey)),
			    elektraRegexstorePos (str, *offset,
						  std::string ("meta") + keyName (keyCurrentMeta (configKey)) + " of " + newkeyname,
						  offsets, keyString (keyCurrentMeta (configKey))[1], parentKey)
				    .c_str ());
	}

	// update offset for next iteration
	*offset += offsets[0].rm_eo;

	regfree (&regex);
	return toAppend;
}


int elektraRegexstoreGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	static std::string d = "system/elektra/modules/regexstore";
	if (keyName (parentKey) == d)
	{
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/regexstore", KEY_VALUE, "regexstore plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/regexstore/exports", KEY_END),
			keyNew ("system/elektra/modules/regexstore/exports/open", KEY_FUNC, elektraRegexstoreOpen, KEY_END),
			keyNew ("system/elektra/modules/regexstore/exports/close", KEY_FUNC, elektraRegexstoreClose, KEY_END),
			keyNew ("system/elektra/modules/regexstore/exports/get", KEY_FUNC, elektraRegexstoreGet, KEY_END),
			keyNew ("system/elektra/modules/regexstore/exports/set", KEY_FUNC, elektraRegexstoreSet, KEY_END),
#include ELEKTRA_README (regexstore)
			keyNew ("system/elektra/modules/regexstore/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}

	int errnosave = errno;
	std::ifstream t (keyString (parentKey));
	if (!t.is_open ())
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return -1;
	}
	std::string str ((std::istreambuf_iterator<char> (t)), std::istreambuf_iterator<char> ());

	KeySet * conf = elektraPluginGetConfig (handle);

	ksRewind (conf);
	Key * confParent = ksLookupByName (conf, "/regexstore", 0);
	if (!confParent)
	{
		ELEKTRA_SET_ERROR (95, parentKey, "Key /regexstore not found in configuration");
		return -1;
	}
	ksNext (conf); // skip root
	do
	{
		int offset = 0;
		Key * toAppend = nullptr;
		do
		{
			toAppend = elektraRegexstoreProcess (ksCurrent (conf), &offset, str, parentKey);
			ksAppendKey (returned, toAppend);
		} while (toAppend);
	} while (ksNext (conf) && keyIsBelow (confParent, ksCurrent (conf)));

	return 1; /* success */
}

int elektraRegexstoreSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}

int elektraRegexstoreError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}


extern "C" Plugin * ELEKTRA_PLUGIN_EXPORT (regexstore)
{
	// clang-format off
	return elektraPluginExport("regexstore",
		ELEKTRA_PLUGIN_OPEN,	&elektraRegexstoreOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraRegexstoreClose,
		ELEKTRA_PLUGIN_GET,	&elektraRegexstoreGet,
		ELEKTRA_PLUGIN_SET,	&elektraRegexstoreSet,
		ELEKTRA_PLUGIN_END);
}

