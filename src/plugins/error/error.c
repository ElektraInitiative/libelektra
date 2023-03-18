/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "error.h"

#ifndef HAVE_KDBCONFIG
#include <internal/kdb/config.h>
#endif

#include <stdlib.h>

// #include <stdio.h>

int elektraErrorOpen (Plugin * handle ELEKTRA_UNUSED, Key * parentKey)
{
	KeySet * conf = elektraPluginGetConfig (handle);

	if (ksLookupByName (conf, "/module", 0))
	{
		// suppress warnings + errors if it is just a module
		return 0;
	}

	Key * warning = ksLookupByName (conf, "/on_open/warnings", 0);
	if (warning)
	{
		elektraTriggerWarnings (keyString (warning), parentKey, "from error plugin in kdbOpen");
	}

	Key * error = ksLookupByName (conf, "/on_open/error", 0);
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

int elektraErrorGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/error"))
	{
		KeySet * n;
		ksAppend (returned,
			  n = ksNew (30, keyNew ("system:/elektra/modules/error", KEY_VALUE, "error plugin waits for your orders", KEY_END),
				     keyNew ("system:/elektra/modules/error/exports", KEY_END),
				     keyNew ("system:/elektra/modules/error/exports/open", KEY_FUNC, elektraErrorOpen, KEY_END),
				     keyNew ("system:/elektra/modules/error/exports/get", KEY_FUNC, elektraErrorGet, KEY_END),
				     keyNew ("system:/elektra/modules/error/exports/set", KEY_FUNC, elektraErrorSet, KEY_END),
#include "readme_error.c"
				     keyNew ("system:/elektra/modules/error/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END));
		ksDel (n);

		ksAppend (returned, n = elektraErrorSpecification ());
		ksDel (n);
	}
	return 1;
}

int elektraErrorSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		Key * cur = ksAtCursor (returned, it);
		const Key * meta = 0;

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

