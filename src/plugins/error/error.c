/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "error.h"

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include <stdlib.h>

// #include <stdio.h>

int elektraErrorOpen (Plugin * handle ELEKTRA_UNUSED, ElektraKey * parentKey)
{
	ElektraKeyset * conf = elektraPluginGetConfig (handle);

	/*
	FILE *f = fopen("error_plugin_debug.log", "a");
	fprintf (f, "HUHU %s\n", keyName(parentKey));
	ksRewind(conf);
	while (ksNext(conf)) fprintf(f, "%s\n", keyName(ksCurrent(conf)));
	fclose(f);
	*/

	if (ksLookupByName (conf, "/module", 0))
	{
		// suppress warnings + errors if it is just a module
		return 0;
	}

	ElektraKey * warning = ksLookupByName (conf, "/on_open/warnings", 0);
	if (warning)
	{
		elektraTriggerWarnings (keyString (warning), parentKey, "from error plugin in kdbOpen");
	}

	ElektraKey * error = ksLookupByName (conf, "/on_open/error", 0);
	if (error)
	{
		if (parentKey)
		{
			elektraTriggerError (keyString (error), parentKey, "from error plugin in kdbOpen");
		}
		return -1;
	}
	return 0;
}

int elektraErrorGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/error"))
	{
		ElektraKeyset * n;
		ksAppend (returned,
			  n = ksNew (30, keyNew ("system:/elektra/modules/error", ELEKTRA_KEY_VALUE, "error plugin waits for your orders", ELEKTRA_KEY_END),
				     keyNew ("system:/elektra/modules/error/exports", ELEKTRA_KEY_END),
				     keyNew ("system:/elektra/modules/error/exports/open", ELEKTRA_KEY_FUNC, elektraErrorOpen, ELEKTRA_KEY_END),
				     keyNew ("system:/elektra/modules/error/exports/get", ELEKTRA_KEY_FUNC, elektraErrorGet, ELEKTRA_KEY_END),
				     keyNew ("system:/elektra/modules/error/exports/set", ELEKTRA_KEY_FUNC, elektraErrorSet, ELEKTRA_KEY_END),
#include "readme_error.c"
				     keyNew ("system:/elektra/modules/error/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END));
		ksDel (n);

		ksAppend (returned, n = elektraErrorSpecification ());
		ksDel (n);
	}
	return 1;
}

int elektraErrorSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	ElektraKey * cur;
	while ((cur = ksNext (returned)) != 0)
	{
		const ElektraKey * meta = 0;

		meta = keyGetMeta (cur, "trigger/warnings");
		if (meta)
		{
			elektraTriggerWarnings (keyString (meta), parentKey, "from error plugin in kdbSet");
		}

		meta = keyGetMeta (cur, "trigger/error");
		if (meta)
		{
			elektraTriggerError (keyString (meta), parentKey, "from error plugin in kdbSet");
			return -1; /* error */
		}
		meta = keyGetMeta (cur, "trigger/error/nofail");
		if (meta)
		{
			elektraTriggerError (keyString (meta), parentKey, "from error plugin in kdbSet");
		}
	}

	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("error",
		ELEKTRA_PLUGIN_OPEN,	&elektraErrorOpen,
		ELEKTRA_PLUGIN_GET,	&elektraErrorGet,
		ELEKTRA_PLUGIN_SET,	&elektraErrorSet,
		ELEKTRA_PLUGIN_END);
}

