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


int elektraDesktopOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional

	return 1; // success
}

int elektraDesktopClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional

	return 1; // success
}

int elektraDesktopGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/desktop"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/desktop", KEY_VALUE, "desktop plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/desktop/exports", KEY_END),
			       keyNew ("system/elektra/modules/desktop/exports/open", KEY_FUNC, elektraDesktopOpen, KEY_END),
			       keyNew ("system/elektra/modules/desktop/exports/close", KEY_FUNC, elektraDesktopClose, KEY_END),
			       keyNew ("system/elektra/modules/desktop/exports/get", KEY_FUNC, elektraDesktopGet, KEY_END),
			       keyNew ("system/elektra/modules/desktop/exports/set", KEY_FUNC, elektraDesktopSet, KEY_END),
			       keyNew ("system/elektra/modules/desktop/exports/error", KEY_FUNC, elektraDesktopError, KEY_END),
			       keyNew ("system/elektra/modules/desktop/exports/checkconf", KEY_FUNC, elektraDesktopCheckConfig, KEY_END),
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
	// get all keys
	// this function is optional

	return 1; // success
}

int elektraDesktopError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional

	return 1; // success
}

int elektraDesktopCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	// the return codes have the following meaning:
	// 0: The configuration was OK and has not been changed
	// 1: The configuration has been changed and now it is OK
	// -1: The configuration was not OK and could not be fixed. An error has to be set to errorKey.
	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (desktop)
{
	// clang-format off
	return elektraPluginExport ("desktop",
		ELEKTRA_PLUGIN_OPEN,	&elektraDesktopOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraDesktopClose,
		ELEKTRA_PLUGIN_GET,	&elektraDesktopGet,
		ELEKTRA_PLUGIN_SET,	&elektraDesktopSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraDesktopError,
		ELEKTRA_PLUGIN_END);
}

