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
	ElektraKey * key = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	elektraKeysetAppendKey (returned, key);

	struct utsname buf;

	uname (&buf); // TODO: handle error

	dir = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	elektraKeyAddBaseName (dir, "sysname");
	elektraKeySetString (dir, buf.sysname);
	elektraKeysetAppendKey (returned, dir);

	dir = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	elektraKeyAddBaseName (dir, "nodename");
	elektraKeySetString (dir, buf.nodename);
	elektraKeysetAppendKey (returned, dir);

	dir = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	elektraKeyAddBaseName (dir, "release");
	elektraKeySetString (dir, buf.release);
	elektraKeysetAppendKey (returned, dir);

	dir = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	elektraKeyAddBaseName (dir, "version");
	elektraKeySetString (dir, buf.version);
	elektraKeysetAppendKey (returned, dir);

	dir = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	elektraKeyAddBaseName (dir, "machine");
	elektraKeySetString (dir, buf.machine);
	elektraKeysetAppendKey (returned, dir);
}

int elektraUnameGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	int errnosave = errno;
	ELEKTRA_LOG ("get uname %s from %s\n", elektraKeyName (parentKey), elektraKeyString (parentKey));

	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/uname"))
	{
		ElektraKeyset * moduleConfig =
			elektraKeysetNew (50, elektraKeyNew ("system:/elektra/modules/uname", ELEKTRA_KEY_VALUE, "uname plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/uname/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/uname/exports/get", ELEKTRA_KEY_FUNC, elektraUnameGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/uname/exports/set", ELEKTRA_KEY_FUNC, elektraUnameSet, ELEKTRA_KEY_END),
#include "readme_uname.c"
			       elektraKeyNew ("system:/elektra/modules/uname/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, moduleConfig);
		elektraKeysetDel (moduleConfig);
		return 1;
	}

	elektraAddUname (returned, parentKey);

	errno = errnosave;
	return 1;
}

int elektraUnameSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey)
{
	ELEKTRA_LOG ("set uname %s from %s\n", elektraKeyName (parentKey), elektraKeyString (parentKey));

	ElektraKeyset * info = elektraKeysetNew (0, ELEKTRA_KS_END);
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


