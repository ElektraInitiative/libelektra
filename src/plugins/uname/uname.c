/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./uname.h"

#include <errno.h>
#include <internal/utility/logger.h>
#include <string.h>
#include <sys/utsname.h>

#ifndef HAVE_KDBCONFIG
#include <internal/kdb/config.h>
#endif

#include <elektra/kdb/errors.h>
#include <internal/macros/attributes.h>
#include <internal/macros/plugin_errors.h>
static int elektraAddUname (KeySet * returned, Key * parentKey)
{
	Key * dir;
	Key * key = keyDup (parentKey, KEY_CP_ALL);
	ksAppendKey (returned, key);

	struct utsname buf;

	if (uname (&buf) < 0)
	{
		ELEKTRA_SET_INTERFACE_ERRORF (parentKey, "Cannot retrieve uname info: %s", strerror (errno));
		return -1;
	}

	dir = keyDup (parentKey, KEY_CP_ALL);
	keyAddBaseName (dir, "sysname");
	keySetString (dir, buf.sysname);
	ksAppendKey (returned, dir);

	dir = keyDup (parentKey, KEY_CP_ALL);
	keyAddBaseName (dir, "nodename");
	keySetString (dir, buf.nodename);
	ksAppendKey (returned, dir);

	dir = keyDup (parentKey, KEY_CP_ALL);
	keyAddBaseName (dir, "release");
	keySetString (dir, buf.release);
	ksAppendKey (returned, dir);

	dir = keyDup (parentKey, KEY_CP_ALL);
	keyAddBaseName (dir, "version");
	keySetString (dir, buf.version);
	ksAppendKey (returned, dir);

	dir = keyDup (parentKey, KEY_CP_ALL);
	keyAddBaseName (dir, "machine");
	keySetString (dir, buf.machine);
	ksAppendKey (returned, dir);
	return 0;
}

int elektraUnameGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	ELEKTRA_LOG ("get uname %s from %s\n", keyName (parentKey), keyString (parentKey));

	if (!strcmp (keyName (parentKey), "system:/elektra/modules/uname"))
	{
		KeySet * moduleConfig =
			ksNew (50, keyNew ("system:/elektra/modules/uname", KEY_VALUE, "uname plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/uname/exports", KEY_END),
			       keyNew ("system:/elektra/modules/uname/exports/get", KEY_FUNC, elektraUnameGet, KEY_END),
			       keyNew ("system:/elektra/modules/uname/exports/set", KEY_FUNC, elektraUnameSet, KEY_END),
#include "./readme_uname.c"
			       keyNew ("system:/elektra/modules/uname/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	if (elektraAddUname (returned, parentKey) < 0)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraUnameSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey)
{
	ELEKTRA_LOG ("set uname %s from %s\n", keyName (parentKey), keyString (parentKey));

	KeySet * info = ksNew (0, KS_END);
	if (elektraAddUname (info, parentKey) < 0)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	ELEKTRA_SET_ERROR_READ_ONLY (info, returned, parentKey);
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("uname",
		ELEKTRA_PLUGIN_GET,            &elektraUnameGet,
		ELEKTRA_PLUGIN_SET,            &elektraUnameSet,
		ELEKTRA_PLUGIN_END
	);
}


