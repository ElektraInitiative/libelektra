/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include "log.h"

int elektraSyslogOpen (Plugin * handle, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	/* plugin initialization logic */

	if (!elektraKeysetLookupByName (elektraPluginGetConfig (handle), "/dontopensyslog", 0))
	{
		openlog ("elektra", LOG_PID, LOG_USER);
	}

	return 0; /* success */
}

int elektraSyslogClose (Plugin * handle, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	/* free all plugin resources and shut it down */

	if (!elektraKeysetLookupByName (elektraPluginGetConfig (handle), "/dontopensyslog", 0))
	{
		closelog ();
	}

	return 0; /* success */
}

int elektraSyslogGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/syslog"))
	{
		ElektraKeyset * n;
		elektraKeysetAppend (returned,
			  n = elektraKeysetNew (30,
				     elektraKeyNew ("system:/elektra/modules/syslog", ELEKTRA_KEY_VALUE, "syslog plugin waits for your orders", ELEKTRA_KEY_END),
				     elektraKeyNew ("system:/elektra/modules/syslog/exports", ELEKTRA_KEY_END),
				     elektraKeyNew ("system:/elektra/modules/syslog/exports/open", ELEKTRA_KEY_FUNC, elektraSyslogOpen, ELEKTRA_KEY_END),
				     elektraKeyNew ("system:/elektra/modules/syslog/exports/close", ELEKTRA_KEY_FUNC, elektraSyslogClose, ELEKTRA_KEY_END),
				     elektraKeyNew ("system:/elektra/modules/syslog/exports/get", ELEKTRA_KEY_FUNC, elektraSyslogGet, ELEKTRA_KEY_END),
				     elektraKeyNew ("system:/elektra/modules/syslog/exports/set", ELEKTRA_KEY_FUNC, elektraSyslogSet, ELEKTRA_KEY_END),
				     elektraKeyNew ("system:/elektra/modules/syslog/exports/error", ELEKTRA_KEY_FUNC, elektraSyslogError, ELEKTRA_KEY_END),
#include "readme_syslog.c"
				     elektraKeyNew ("system:/elektra/modules/syslog/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END));
		elektraKeysetDel (n);

		return 1;
	}

	if (strncmp (elektraKeyString (elektraKeysetLookupByName (elektraPluginGetConfig (handle), "/log/get", 0)), "1", 1) == 0)
	{
		syslog (LOG_NOTICE, "loading configuration %s", elektraKeyName (parentKey));
	}

	return 1;
}

int elektraSyslogSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	size_t changed = 0;
	ElektraKey * k = 0;
	elektraKeysetRewind (returned);
	while ((k = elektraKeysetNext (returned)))
	{
		if (elektraKeyNeedSync (k))
		{
			syslog (LOG_NOTICE, "change %s to %s", elektraKeyName (k), elektraKeyString (k));
			changed++;
		}
	}

	syslog (LOG_NOTICE, "committed configuration %s with %zd keys (%zu changed)", elektraKeyName (parentKey), elektraKeysetGetSize (returned), changed);

	return 1;
}

int elektraSyslogError (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	syslog (LOG_NOTICE, "rollback configuration %s with %zd keys", elektraKeyName (parentKey), elektraKeysetGetSize (returned));

	return 1;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("syslog",
		ELEKTRA_PLUGIN_OPEN,	&elektraSyslogOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraSyslogClose,
		ELEKTRA_PLUGIN_GET,	&elektraSyslogGet,
		ELEKTRA_PLUGIN_SET,	&elektraSyslogSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraSyslogError,
		ELEKTRA_PLUGIN_END);
}
