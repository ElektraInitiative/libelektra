/**
 * @file
 *
 * @brief Source for desktop plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include "desktop.h"

#include <ctype.h>   // for tolower
#include <stdlib.h>  // for getenv
#include <strings.h> // for strcasecmp

#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>


int elektraDesktopGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	ELEKTRA_LOG ("get desktop %s from %s\n", keyName (parentKey), keyString (parentKey));

	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/desktop"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/desktop", KEY_VALUE, "desktop plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/desktop/exports", KEY_END),
			       keyNew ("system/elektra/modules/desktop/exports/get", KEY_FUNC, elektraDesktopGet, KEY_END),
			       keyNew ("system/elektra/modules/desktop/exports/set", KEY_FUNC, elektraDesktopSet, KEY_END),
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

int elektraDesktopSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	ELEKTRA_LOG ("set desktop %s from %s\n", keyName (parentKey), keyString (parentKey));

	KeySet * info = ksNew (0, KS_END);
	elektraDesktopGet (handle, info, parentKey);

	Key * k;
	ksRewind (info);
	ksRewind (returned);
	while ((k = ksNext (returned)))
	{
		Key * c = ksNext (info);
		if (strcmp (keyName (k), keyName (c)) || strcmp (keyString (k), keyString (c)))
		{
			ELEKTRA_SET_ERRORF (84, parentKey, "the key %s (expected %s) was modified to %s (expected %s)", keyName (k),
					    keyName (c), keyString (k), keyString (c));
			return -1;
		}
	}

	ksDel (info);
	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (desktop)
{
	// clang-format off
	return elektraPluginExport ("desktop",
		ELEKTRA_PLUGIN_GET,	&elektraDesktopGet,
		ELEKTRA_PLUGIN_SET,	&elektraDesktopSet,
		ELEKTRA_PLUGIN_END);
}

