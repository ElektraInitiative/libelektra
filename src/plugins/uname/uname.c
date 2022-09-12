/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "uname.h"

#include <errno.h>
#include <kdblogger.h>
#include <string.h>
#include <sys/utsname.h>

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include <kdberrors.h>
#include <kdbmacros.h>

static void elektraAddUname (ElektraKeyset * returned, ElektraKey * parentKey)
{
	ElektraKey * dir;
	ElektraKey * key = keyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	ksAppendKey (returned, key);

	struct utsname buf;

	uname (&buf); // TODO: handle error

	dir = keyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	keyAddBaseName (dir, "sysname");
	keySetString (dir, buf.sysname);
	ksAppendKey (returned, dir);

	dir = keyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	keyAddBaseName (dir, "nodename");
	keySetString (dir, buf.nodename);
	ksAppendKey (returned, dir);

	dir = keyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	keyAddBaseName (dir, "release");
	keySetString (dir, buf.release);
	ksAppendKey (returned, dir);

	dir = keyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	keyAddBaseName (dir, "version");
	keySetString (dir, buf.version);
	ksAppendKey (returned, dir);

	dir = keyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	keyAddBaseName (dir, "machine");
	keySetString (dir, buf.machine);
	ksAppendKey (returned, dir);
}

int elektraUnameGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	int errnosave = errno;
	ELEKTRA_LOG ("get uname %s from %s\n", keyName (parentKey), keyString (parentKey));

	if (!strcmp (keyName (parentKey), "system:/elektra/modules/uname"))
	{
		ElektraKeyset * moduleConfig =
			ksNew (50, keyNew ("system:/elektra/modules/uname", ELEKTRA_KEY_VALUE, "uname plugin waits for your orders", ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/uname/exports", ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/uname/exports/get", ELEKTRA_KEY_FUNC, elektraUnameGet, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/uname/exports/set", ELEKTRA_KEY_FUNC, elektraUnameSet, ELEKTRA_KEY_END),
#include "readme_uname.c"
			       keyNew ("system:/elektra/modules/uname/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	elektraAddUname (returned, parentKey);

	errno = errnosave;
	return 1;
}

int elektraUnameSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey)
{
	ELEKTRA_LOG ("set uname %s from %s\n", keyName (parentKey), keyString (parentKey));

	ElektraKeyset * info = ksNew (0, ELEKTRA_KS_END);
	elektraAddUname (info, parentKey);
	ELEKTRA_SET_ERROR_READ_ONLY (info, returned, parentKey);
	return 0;
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


