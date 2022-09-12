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
	ELEKTRA_LOG ("get desktop %s from %s\n", elektraKeyName (parentKey), elektraKeyString (parentKey));

	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/desktop"))
	{
		ElektraKeyset * contract =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/desktop", ELEKTRA_KEY_VALUE, "desktop plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/desktop/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/desktop/exports/get", ELEKTRA_KEY_FUNC, elektraDesktopGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/desktop/exports/set", ELEKTRA_KEY_FUNC, elektraDesktopSet, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       elektraKeyNew ("system:/elektra/modules/desktop/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return 1; // success
	}

	const char * desktop;
	// get key
	if (getenv ("GNOME_DESKTOP_SESSION_ID"))
	{
		elektraKeysetAppendKey (returned, elektraKeyNew (elektraKeyName (parentKey), ELEKTRA_KEY_VALUE, "gnome", ELEKTRA_KEY_END));
	}
	else if (getenv ("KDE_FULL_SESSION"))
	{
		elektraKeysetAppendKey (returned, elektraKeyNew (elektraKeyName (parentKey), ELEKTRA_KEY_VALUE, "kde", ELEKTRA_KEY_END));
	}
	else if (getenv ("TDE_FULL_SESSION"))
	{
		elektraKeysetAppendKey (returned, elektraKeyNew (elektraKeyName (parentKey), ELEKTRA_KEY_VALUE, "tde", ELEKTRA_KEY_END));
	}
	else if ((desktop = getenv ("DESKTOP_SESSION")) && !strcasecmp (desktop, "unity"))
	{
		elektraKeysetAppendKey (returned, elektraKeyNew (elektraKeyName (parentKey), ELEKTRA_KEY_VALUE, "unity", ELEKTRA_KEY_END));
	}
	else if ((desktop = getenv ("XDG_CURRENT_DESKTOP")))
	{
		char * str = elektraStrDup (desktop);
		for (int i = 0; str[i]; i++)
		{
			str[i] = tolower (str[i]);
		}
		elektraKeysetAppendKey (returned, elektraKeyNew (elektraKeyName (parentKey), ELEKTRA_KEY_VALUE, str, ELEKTRA_KEY_END));
		elektraFree (str);
	}

	return 1; // success
}

int elektraDesktopSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	ELEKTRA_LOG ("set desktop %s from %s\n", elektraKeyName (parentKey), elektraKeyString (parentKey));

	ElektraKeyset * info = elektraKeysetNew (0, ELEKTRA_KS_END);
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

