/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include "validation.h"

int elektraValidationGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	KeySet * n;
	ksAppend (returned,
		  n = ksNew (30,
			     keyNew ("system/elektra/modules/validation", KEY_VALUE, "validation plugin waits for your orders", KEY_END),
			     keyNew ("system/elektra/modules/validation/exports", KEY_END),
			     keyNew ("system/elektra/modules/validation/exports/get", KEY_FUNC, elektraValidationGet, KEY_END),
			     keyNew ("system/elektra/modules/validation/exports/set", KEY_FUNC, elektraValidationSet, KEY_END),
			     keyNew ("system/elektra/modules/validation/exports/ksLookupRE", KEY_FUNC, ksLookupRE, KEY_END),
#include "readme_validation.c"
			     keyNew ("system/elektra/modules/validation/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END));
	ksDel (n);
	return 1;
}

int elektraValidationSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	Key * cur = 0;

	while ((cur = ksNext (returned)) != 0)
	{
		const Key * meta = keyGetMeta (cur, "check/validation");

		if (!meta) continue;

		regex_t regex;
		regmatch_t offsets;
		int ret = regcomp (&regex, keyString (meta), REG_NOSUB | REG_EXTENDED);

		if (ret != 0)
		{
			char buffer[1000];
			regerror (ret, &regex, buffer, 999);
			ELEKTRA_SET_ERROR (41, parentKey, buffer);
			regfree (&regex);
			return -1;
		}

		ret = regexec (&regex, keyString (cur), 1, &offsets, 0);

		if (ret != 0) /* e.g. REG_NOMATCH */
		{
			const Key * msg = keyGetMeta (cur, "check/validation/message");
			if (msg)
			{
				ELEKTRA_SET_ERROR (42, parentKey, keyString (msg));
				regfree (&regex);
				return -1;
			}
			else
			{
				char buffer[1000];
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

Plugin * ELEKTRA_PLUGIN_EXPORT (validation)
{
	// clang-format off
	return elektraPluginExport("validation",
		ELEKTRA_PLUGIN_GET,	&elektraValidationGet,
		ELEKTRA_PLUGIN_SET,	&elektraValidationSet,
		ELEKTRA_PLUGIN_END);
}

