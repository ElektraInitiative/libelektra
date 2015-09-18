/**
* \file
*
* \brief Source for enum plugin
*
* \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/


#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>
#include <kdberrors.h>
#include "enum.h"

int elektraEnumOpen(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	/* plugin initialization logic */

	return 1; /* success */
}

int elektraEnumClose(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	/* free all plugin resources and shut it down */

	return 1; /* success */
}

int elektraEnumGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	if (!strcmp(keyName(parentKey), "system/elektra/modules/enum"))
	{
		KeySet *contract = ksNew (30,
		keyNew ("system/elektra/modules/enum",
			KEY_VALUE, "enum plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/enum/exports", KEY_END),
		keyNew ("system/elektra/modules/enum/exports/open",
			KEY_FUNC, elektraEnumOpen, KEY_END),
		keyNew ("system/elektra/modules/enum/exports/close",
			KEY_FUNC, elektraEnumClose, KEY_END),
		keyNew ("system/elektra/modules/enum/exports/get",
			KEY_FUNC, elektraEnumGet, KEY_END),
		keyNew ("system/elektra/modules/enum/exports/set",
			KEY_FUNC, elektraEnumSet, KEY_END),
		keyNew ("system/elektra/modules/enum/exports/error",
			KEY_FUNC, elektraEnumError, KEY_END),
#include ELEKTRA_README(enum)
		keyNew ("system/elektra/modules/enum/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}
	/* get all keys */

	return 1; /* success */
}

static int validateKey(Key *key)
{
	const Key *meta = keyGetMeta(key, "check/enum");
	if(!meta)
		return 1;
	const char *validValues = keyString(meta);
	const char *regexString = "([[:alnum:]]+)";
	regex_t regex;
	if(regcomp(&regex, regexString, REG_EXTENDED|REG_NEWLINE))
	{
		ELEKTRA_SET_ERROR(120, key, "regcomp failed"); 
		return -1;
	}
	const char *ptr = validValues;
	regmatch_t match;
	char *value = NULL;
	int nomatch;
	int len;
	while(1)
	{
		nomatch = regexec(&regex, ptr, 1, &match, 0);
		if(nomatch)
			break;
		len = match.rm_eo - match.rm_so;
		value = realloc(value, (match.rm_eo - match.rm_so)+1);
		strncpy(value, (ptr+match.rm_so), len);
		if(strcmp(keyString(key), value) == 0)
		{
			regfree(&regex);
			return 1;
		}
		ptr += match.rm_eo;
	}
	regfree(&regex);
	return 0;
}

int elektraEnumSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	/* set all keys */
	Key *cur;
	while((cur = ksNext(returned)) != NULL)
	{
		if(!validateKey(cur))
		{
			ELEKTRA_SET_ERROR(121, parentKey, "Validation failed");
			return -1;
		}
	}	

	return 1; /* success */
}

int elektraEnumError(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(enum)
{
	return elektraPluginExport("enum",
		ELEKTRA_PLUGIN_OPEN,	&elektraEnumOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraEnumClose,
		ELEKTRA_PLUGIN_GET,	&elektraEnumGet,
		ELEKTRA_PLUGIN_SET,	&elektraEnumSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraEnumError,
		ELEKTRA_PLUGIN_END);
}

