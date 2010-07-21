/***************************************************************************
                     validation.c  -  Skeleton of a plugin
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


#include "validation.h"

int elektraValidationGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	KeySet *n;
	ksAppend (returned, n=ksNew (30,
		keyNew ("system/elektra/modules/validation",
			KEY_VALUE, "validation plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/validation/exports", KEY_END),
		keyNew ("system/elektra/modules/validation/exports/get",
			KEY_FUNC, elektraValidationGet,
			KEY_END),
		keyNew ("system/elektra/modules/validation/exports/set",
			KEY_FUNC, elektraValidationSet,
			KEY_END),
		keyNew ("system/elektra/modules/validation/exports/ksLookupRE",
			KEY_FUNC, ksLookupRE,
			KEY_END),
		keyNew ("system/elektra/modules/validation/infos",
			KEY_VALUE, "All information you want to know", KEY_END),
		keyNew ("system/elektra/modules/validation/infos/author",
			KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
		keyNew ("system/elektra/modules/validation/infos/licence",
			KEY_VALUE, "BSD", KEY_END),
		keyNew ("system/elektra/modules/validation/infos/description",
			KEY_VALUE, "Validates key values using regular expressions", KEY_END),
		keyNew ("system/elektra/modules/validation/infos/provides",
			KEY_VALUE, "validation", KEY_END),
		keyNew ("system/elektra/modules/validation/infos/placements",
			KEY_VALUE, "presetstorage", KEY_END),
		keyNew ("system/elektra/modules/validation/infos/needs",
			KEY_VALUE, "", KEY_END),
		keyNew ("system/elektra/modules/validation/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END));
	ksDel (n);
	return 1;
}

int elektraValidationSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	Key *cur = 0;

	while ((cur = ksNext(returned)) != 0)
	{
		const Key *meta = keyGetMeta (cur, "validation/regex");

		if (!meta) continue;

		regex_t regex;
		regmatch_t offsets;
		int ret = regcomp(&regex, keyString(meta), REG_NOSUB | REG_EXTENDED);

		if (ret != 0)
		{
			char buffer [1000];
			regerror (ret, &regex, buffer, 999);
			ELEKTRA_SET_ERROR (41, parentKey, buffer);
			regfree (&regex);
			return -1;
		}

		ret = regexec(&regex, keyString(cur), 1, &offsets, 0);

		if (ret != 0) /* e.g. REG_NOMATCH */
		{
			const Key *msg = keyGetMeta (cur, "validation/message");
			if (msg)
			{
				ELEKTRA_SET_ERROR (42, parentKey, keyString(msg));
				regfree (&regex);
				return -1;
			} else {
				char buffer [1000];
				regerror (ret, &regex, buffer, 999);
				ELEKTRA_SET_ERROR (42, parentKey, buffer);
				regfree (&regex);
				return -1;
			}
		}
		regfree (&regex);
	}

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(validation)
{
	return elektraPluginExport("validation",
		ELEKTRA_PLUGIN_GET,	&elektraValidationGet,
		ELEKTRA_PLUGIN_SET,	&elektraValidationSet,
		ELEKTRA_PLUGIN_END);
}

