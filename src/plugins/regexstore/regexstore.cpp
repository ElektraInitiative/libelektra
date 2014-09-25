/***************************************************************************
                     regexstore.c  -  Skeleton of a plugin
                             -------------------
    begin                : Fri May 21 2010
    copyright            : (C) 2010 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This is the skeleton of the methods you'll have to implement in order *
 *   to provide a valid plugin.                                            *
 *   Simple fill the empty functions with your code and you are            *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <regex.h>

#include <string>
#include <fstream>
#include <streambuf>

#include "regexstore.h"

using namespace ckdb;

#include <kdberrors.h>
#include <kdbproposal.h>

int elektraRegexstoreOpen(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	/* plugin initialization logic */

	return 1; /* success */
}

int elektraRegexstoreClose(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	/* free all plugin resources and shut it down */

	return 1; /* success */
}

std::string elektraRegexstorePos(std::string const & str,
		std::string const & text,
		regmatch_t *offsets,
		char index,
		Key *parentKey)
{
	int pos = index - '0';
	if (pos < 0 || pos > 9)
	{
		ELEKTRA_ADD_WARNINGF (96,
			parentKey,
			"Regex Group %d for %s not between 0 and 9 read from %s",
			pos, text.c_str(), &index);
		return std::string("");
	}
	if (offsets[pos].rm_so == -1)
	{
		ELEKTRA_ADD_WARNINGF (96,
			parentKey,
			"Regex Group %d missing for %s",
			pos, text.c_str());
		return std::string("");
	}
	std::string const & ret = str.substr(offsets[pos].rm_so,
		offsets[pos].rm_eo - offsets[pos].rm_so);
	ELEKTRA_ADD_WARNINGF (96,
		parentKey,
		"Regex Group %d ok for %s; GOT:%s",
		pos, text.c_str(), ret.c_str());
	return ret;
}

#include <iostream>

/** e.g.
configKey = keyNew ("user/map/#2",
		KEY_VALUE, "#1 map ([^ \n]*) ([^ \n]*)",
		KEY_META, "meta", "1",
		KEY_END);
*/
Key *elektraRegexstoreProcess(Key *configKey,
		std::string const & str,
		Key *parentKey)
{
	regex_t regex;
	size_t nmatch = 10;
	regmatch_t offsets[10];
	std::string configString = keyString(configKey);
	if (configString.length() < 3 &&
		configString[0] != '#' &&
		(configString[1] < '0' || configString[1] > '9') &&
		configString[2] != ' ')
	{
		ELEKTRA_ADD_WARNINGF (96,
			parentKey,
			"String %s of %s did not start with #<number><space>",
			configString.c_str(), keyName(configKey));
		return 0;
	}

	int ret = regcomp(&regex, configString.c_str()+3, REG_EXTENDED);

	if (ret != 0)
	{
		char buffer [1000];
		regerror (ret, &regex, buffer, 999);
		ELEKTRA_ADD_WARNINGF (96,
			parentKey,
			"Could not compile regex %s, because: %s",
			configString.c_str()+3, buffer);
		regfree (&regex);
		return 0;
	}

	ret = regexec(&regex, str.c_str(), nmatch, offsets, 0);

	if (ret != 0) /* e.g. REG_NOMATCH */
	{
		char buffer [1000];
		regerror (ret, &regex, buffer, 999);
		ELEKTRA_ADD_WARNINGF (96,
			parentKey,
			"No regex match, because: %s",
			buffer);
		regfree (&regex);
		return 0;
	}

	std::string keyname;
	// skip user/regexstore ..
	if (!strncmp(keyName(configKey), "user", 4))
	{
		keyname = keyName(configKey)+15;
	} else if (!strncmp(keyName(configKey), "system", 6))
	{
		keyname = keyName(configKey)+17;
	}
	std::string newkeyname;

	for (size_t i=0; i<keyname.size(); ++i)
	{
		if (keyname[i] == '#')
		{
			++i;
			newkeyname += elektraRegexstorePos(str,
					std::string("keyname ")+keyname,
					offsets,
					keyname[i],
					parentKey);
		}
		else
		{
			newkeyname += keyname[i];
		}
	}

	Key *toAppend = keyNew (keyName(parentKey), KEY_END);
	keyAddName(toAppend, newkeyname.c_str());

	keySetString(toAppend,
			elektraRegexstorePos(str,
				"keystring of "+newkeyname,
				offsets,
				configString[1],
				parentKey).c_str());

	keyRewindMeta(configKey);
	while (keyNextMeta(configKey))
	{
		keySetMeta(toAppend,
			keyName(keyCurrentMeta(configKey)),
			elektraRegexstorePos(str,
				std::string("meta")
					+keyName(keyCurrentMeta(configKey))
					+" of "+newkeyname,
				offsets,
				keyString(keyCurrentMeta(configKey))[1],
				parentKey).c_str());
	}
	regfree (&regex);
	return toAppend;
}



int elektraRegexstoreGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	static std::string d = "system/elektra/modules/regexstore";
	if (keyName(parentKey) == d)
	{
		KeySet *contract = ksNew (30,
		keyNew ("system/elektra/modules/regexstore",
			KEY_VALUE, "dbus plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/regexstore/exports", KEY_END),
		keyNew ("system/elektra/modules/regexstore/exports/open",
			KEY_FUNC, elektraRegexstoreOpen, KEY_END),
		keyNew ("system/elektra/modules/regexstore/exports/close",
			KEY_FUNC, elektraRegexstoreClose, KEY_END),
		keyNew ("system/elektra/modules/regexstore/exports/get",
			KEY_FUNC, elektraRegexstoreGet, KEY_END),
		keyNew ("system/elektra/modules/regexstore/exports/set",
			KEY_FUNC, elektraRegexstoreSet, KEY_END),
#include ELEKTRA_README(regexstore)
		keyNew ("system/elektra/modules/regexstore/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}

	std::ifstream t(keyString(parentKey));
	if (!t.is_open())
	{
		ELEKTRA_SET_ERRORF (95, parentKey, "could not open file %s",
				keyString(parentKey));
		return -1;
	}
	std::string str((std::istreambuf_iterator<char>(t)),
			std::istreambuf_iterator<char>());

	KeySet *conf = elektraPluginGetConfig(handle);

	ksRewind(conf);
	Key *confParent = ksLookupByName(conf, "/regexstore", 0);
	if (!confParent)
	{
		ELEKTRA_SET_ERROR (95, parentKey,
				"Key /regexstore not found in configuration");
		return -1;
	}
	ksNext(conf); // skip root
	do {
		Key *toAppend = elektraRegexstoreProcess(
			ksCurrent(conf),
			str,
			parentKey);
		ksAppendKey(returned, toAppend);
	}
	while(ksNext(conf) && keyIsBelow(confParent, ksCurrent(conf)));

	return 1; /* success */
}

int elektraRegexstoreSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}

int elektraRegexstoreError(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}


extern "C" Plugin *ELEKTRA_PLUGIN_EXPORT(regexstore)
{
	return elektraPluginExport("regexstore",
		ELEKTRA_PLUGIN_OPEN,	&elektraRegexstoreOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraRegexstoreClose,
		ELEKTRA_PLUGIN_GET,	&elektraRegexstoreGet,
		ELEKTRA_PLUGIN_SET,	&elektraRegexstoreSet,
		ELEKTRA_PLUGIN_END);
}

