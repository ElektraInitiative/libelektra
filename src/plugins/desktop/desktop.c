/**
 * @file
 *
 * @brief Source for desktop plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "desktop.h"

#include <ctype.h>   // for tolower
#include <stdlib.h>  // for getenv
#include <strings.h> // for strcasecmp

#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <kdbmacros.h>


int elektraDesktopGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	ELEKTRA_LOG ("get desktop %s from %s\n", keyName (parentKey), keyString (parentKey));

	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/desktop"))
	{
		ElektraKeyset * contract =
			ksNew (30, keyNew ("system:/elektra/modules/desktop", ELEKTRA_KEY_VALUE, "desktop plugin waits for your orders", ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/desktop/exports", ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/desktop/exports/get", ELEKTRA_KEY_FUNC, elektraDesktopGet, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/desktop/exports/set", ELEKTRA_KEY_FUNC, elektraDesktopSet, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/desktop/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}

	const char * desktop;
	// get key
	if (getenv ("GNOME_DESKTOP_SESSION_ID"))
	{
		ksAppendKey (returned, keyNew (keyName (parentKey), ELEKTRA_KEY_VALUE, "gnome", ELEKTRA_KEY_END));
	}
	else if (getenv ("KDE_FULL_SESSION"))
	{
		ksAppendKey (returned, keyNew (keyName (parentKey), ELEKTRA_KEY_VALUE, "kde", ELEKTRA_KEY_END));
	}
	else if (getenv ("TDE_FULL_SESSION"))
	{
		ksAppendKey (returned, keyNew (keyName (parentKey), ELEKTRA_KEY_VALUE, "tde", ELEKTRA_KEY_END));
	}
	else if ((desktop = getenv ("DESKTOP_SESSION")) && !strcasecmp (desktop, "unity"))
	{
		ksAppendKey (returned, keyNew (keyName (parentKey), ELEKTRA_KEY_VALUE, "unity", ELEKTRA_KEY_END));
	}
	else if ((desktop = getenv ("XDG_CURRENT_DESKTOP")))
	{
		char * str = elektraStrDup (desktop);
		for (int i = 0; str[i]; i++)
		{
			str[i] = tolower (str[i]);
		}
		ksAppendKey (returned, keyNew (keyName (parentKey), ELEKTRA_KEY_VALUE, str, ELEKTRA_KEY_END));
		elektraFree (str);
	}

	return 1; // success
}

int elektraDesktopSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	ELEKTRA_LOG ("set desktop %s from %s\n", keyName (parentKey), keyString (parentKey));

	ElektraKeyset * info = ksNew (0, ELEKTRA_KS_END);
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

