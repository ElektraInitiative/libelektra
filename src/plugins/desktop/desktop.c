/**
 * @file
 *
 * @brief Source for desktop plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "desktop.h"

#include <ctype.h>   // for tolower
#include <stdlib.h>  // for getenv
#include <strings.h> // for strcasecmp

#include <kdbhelper.h>


int elektraDesktopGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/desktop"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/desktop", KEY_VALUE, "desktop plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/desktop/exports", KEY_END),
			       keyNew ("system/elektra/modules/desktop/exports/get", KEY_FUNC, elektraDesktopGet, KEY_END),
#include ELEKTRA_README (desktop)
			       keyNew ("system/elektra/modules/desktop/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
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
	else if (!strcasecmp (getenv ("DESKTOP_SESSION"), "unity"))
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

Plugin * ELEKTRA_PLUGIN_EXPORT (desktop)
{
	// clang-format off
	return elektraPluginExport ("desktop",
		ELEKTRA_PLUGIN_GET,	&elektraDesktopGet,
		ELEKTRA_PLUGIN_END);
}

