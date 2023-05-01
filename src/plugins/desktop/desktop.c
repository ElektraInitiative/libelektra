/**
 * @file
 *
 * @brief Source for desktop plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./desktop.h"

#include <ctype.h>   // for tolower
#include <stdlib.h>  // for getenv
#include <strings.h> // for strcasecmp

#include <elektra/core/errors.h>
#include <internal/macros/attributes.h>
#include <internal/macros/plugin_errors.h>
#include <internal/utility/logger.h>
#include <internal/utility/old_helper.h>


int elektraDesktopGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	ELEKTRA_LOG ("get desktop %s from %s\n", keyName (parentKey), keyString (parentKey));

	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/desktop"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/desktop", KEY_VALUE, "desktop plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/desktop/exports", KEY_END),
			       keyNew ("system:/elektra/modules/desktop/exports/get", KEY_FUNC, elektraDesktopGet, KEY_END),
			       keyNew ("system:/elektra/modules/desktop/exports/set", KEY_FUNC, elektraDesktopSet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/desktop/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}

	const char * desktop;
	// get key
	if (getenv ("GNOME_DESKTOP_SESSION_ID"))
	{
		ksAppendKey (returned, keyNew (keyName (parentKey), KEY_VALUE, "gnome", KEY_END));
	}
	else if (getenv ("KDE_FULL_SESSION"))
	{
		ksAppendKey (returned, keyNew (keyName (parentKey), KEY_VALUE, "kde", KEY_END));
	}
	else if (getenv ("TDE_FULL_SESSION"))
	{
		ksAppendKey (returned, keyNew (keyName (parentKey), KEY_VALUE, "tde", KEY_END));
	}
	else if ((desktop = getenv ("DESKTOP_SESSION")) && !strcasecmp (desktop, "unity"))
	{
		ksAppendKey (returned, keyNew (keyName (parentKey), KEY_VALUE, "unity", KEY_END));
	}
	else if ((desktop = getenv ("XDG_CURRENT_DESKTOP")))
	{
		char * str = elektraStrDup (desktop);
		for (int i = 0; str[i]; i++)
		{
			str[i] = tolower (str[i]);
		}
		ksAppendKey (returned, keyNew (keyName (parentKey), KEY_VALUE, str, KEY_END));
		elektraFree (str);
	}

	return 1; // success
}

int elektraDesktopSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	ELEKTRA_LOG ("set desktop %s from %s\n", keyName (parentKey), keyString (parentKey));

	KeySet * info = ksNew (0, KS_END);
	elektraDesktopGet (handle, info, parentKey);
	ELEKTRA_SET_ERROR_READ_ONLY (info, returned, parentKey);
	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("desktop",
		ELEKTRA_PLUGIN_GET,	&elektraDesktopGet,
		ELEKTRA_PLUGIN_SET,	&elektraDesktopSet,
		ELEKTRA_PLUGIN_END);
}

