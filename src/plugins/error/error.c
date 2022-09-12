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

	if (elektraKeysetLookupByName (conf, "/module", 0))
	{
		// suppress warnings + errors if it is just a module
		return 0;
	}

	ElektraKey * warning = elektraKeysetLookupByName (conf, "/on_open/warnings", 0);
	if (warning)
	{
		elektraTriggerWarnings (elektraKeyString (warning), parentKey, "from error plugin in kdbOpen");
	}

	ElektraKey * error = elektraKeysetLookupByName (conf, "/on_open/error", 0);
	if (error)
	{
		if (parentKey)
		{
			elektraTriggerError (elektraKeyString (error), parentKey, "from error plugin in kdbOpen");
		}
		return -1;
	}
	return 0;
}

int elektraErrorGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/error"))
	{
		ElektraKeyset * n;
		elektraKeysetAppend (returned,
			  n = elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/error", ELEKTRA_KEY_VALUE, "error plugin waits for your orders", ELEKTRA_KEY_END),
				     elektraKeyNew ("system:/elektra/modules/error/exports", ELEKTRA_KEY_END),
				     elektraKeyNew ("system:/elektra/modules/error/exports/open", ELEKTRA_KEY_FUNC, elektraErrorOpen, ELEKTRA_KEY_END),
				     elektraKeyNew ("system:/elektra/modules/error/exports/get", ELEKTRA_KEY_FUNC, elektraErrorGet, ELEKTRA_KEY_END),
				     elektraKeyNew ("system:/elektra/modules/error/exports/set", ELEKTRA_KEY_FUNC, elektraErrorSet, ELEKTRA_KEY_END),
#include "readme_error.c"
				     elektraKeyNew ("system:/elektra/modules/error/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END));
		elektraKeysetDel (n);

		elektraKeysetAppend (returned, n = elektraErrorSpecification ());
		elektraKeysetDel (n);
	}
	return 1;
}

int elektraErrorSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	ElektraKey * cur;
	while ((cur = elektraKeysetNext (returned)) != 0)
	{
		const ElektraKey * meta = 0;

		meta = elektraKeyGetMeta (cur, "trigger/warnings");
		if (meta)
		{
			elektraTriggerWarnings (elektraKeyString (meta), parentKey, "from error plugin in kdbSet");
		}

		meta = elektraKeyGetMeta (cur, "trigger/error");
		if (meta)
		{
			elektraTriggerError (elektraKeyString (meta), parentKey, "from error plugin in kdbSet");
			return -1; /* error */
		}
		meta = elektraKeyGetMeta (cur, "trigger/error/nofail");
		if (meta)
		{
			elektraTriggerError (elektraKeyString (meta), parentKey, "from error plugin in kdbSet");
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

