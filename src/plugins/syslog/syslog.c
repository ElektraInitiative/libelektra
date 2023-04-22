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

#include <kdbchangetracking.h>

#include "log.h"

int elektraSyslogOpen (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	/* plugin initialization logic */

	if (!ksLookupByName (elektraPluginGetConfig (handle), "/dontopensyslog", 0))
	{
		openlog ("elektra", LOG_PID, LOG_USER);
	}

	return 0; /* success */
}

int elektraSyslogClose (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	/* free all plugin resources and shut it down */

	if (!ksLookupByName (elektraPluginGetConfig (handle), "/dontopensyslog", 0))
	{
		closelog ();
	}

	return 0; /* success */
}

int elektraSyslogGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/syslog"))
	{
		KeySet * n;
		ksAppend (returned,
			  n = ksNew (30,
				     keyNew ("system:/elektra/modules/syslog", KEY_VALUE, "syslog plugin waits for your orders", KEY_END),
				     keyNew ("system:/elektra/modules/syslog/exports", KEY_END),
				     keyNew ("system:/elektra/modules/syslog/exports/open", KEY_FUNC, elektraSyslogOpen, KEY_END),
				     keyNew ("system:/elektra/modules/syslog/exports/close", KEY_FUNC, elektraSyslogClose, KEY_END),
				     keyNew ("system:/elektra/modules/syslog/exports/get", KEY_FUNC, elektraSyslogGet, KEY_END),
				     keyNew ("system:/elektra/modules/syslog/exports/commit", KEY_FUNC, elektraSyslogCommit, KEY_END),
				     keyNew ("system:/elektra/modules/syslog/exports/error", KEY_FUNC, elektraSyslogError, KEY_END),
				     keyNew ("system:/elektra/modules/syslog/exports/hook/notification/send/get", KEY_FUNC,
					     elektraSyslogGet, KEY_END),
				     keyNew ("system:/elektra/modules/syslog/exports/hook/notification/send/set", KEY_FUNC,
					     elektraSyslogCommit, KEY_END),
#include "readme_syslog.c"
				     keyNew ("system:/elektra/modules/syslog/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END));
		ksDel (n);

		return 1;
	}

	if (strncmp (keyString (ksLookupByName (elektraPluginGetConfig (handle), "/log/get", 0)), "1", 1) == 0)
	{
		syslog (LOG_NOTICE, "loading configuration %s", keyName (parentKey));
	}

	return 1;
}

int elektraSyslogCommit (Plugin * handle, KeySet * returned, Key * parentKey)
{
	const ChangeTrackingContext * context = elektraChangeTrackingGetContextFromPlugin (handle);
	ElektraDiff * diff = elektraChangeTrackingCalculateDiff (returned, context, parentKey);

	KeySet * added = elektraDiffGetAddedKeys (diff);
	KeySet * modified = elektraDiffGetModifiedKeys (diff);

	KeySet * changed = ksNew (0, KS_END);
	ksAppend (changed, added);
	ksAppend (changed, modified);

	for (elektraCursor it = 0; it < ksGetSize (changed); ++it)
	{
		Key * k = ksAtCursor (changed, it);
		Key * new = ksLookup (returned, k, 0);
		syslog (LOG_NOTICE, "change %s to %s", keyName (new), keyString (new));
	}

	syslog (LOG_NOTICE, "committed configuration %s with %zd keys (%zu changed)", keyName (parentKey), ksGetSize (returned),
		ksGetSize (changed));

	ksDel (modified);
	ksDel (added);
	ksDel (changed);
	elektraDiffDel (diff);

	return 1;
}

int elektraSyslogError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	syslog (LOG_NOTICE, "rollback configuration %s with %zd keys", keyName (parentKey), ksGetSize (returned));

	return 1;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("syslog",
		ELEKTRA_PLUGIN_OPEN,	&elektraSyslogOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraSyslogClose,
		ELEKTRA_PLUGIN_GET,	&elektraSyslogGet,
		ELEKTRA_PLUGIN_COMMIT,	&elektraSyslogCommit,
		ELEKTRA_PLUGIN_ERROR,	&elektraSyslogError,
		ELEKTRA_PLUGIN_END);
}
