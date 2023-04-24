/**
 * @file
 *
 * @brief A plugin which logs write operations and errors via the native journald interface
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <internal/kdb/config.h>
#include <internal/macros/attributes.h>

#include <stdlib.h>
#include <systemd/sd-journal.h>
#include <unistd.h>

#include "./journald.h"

int elektraJournaldGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/journald"))
	{
		KeySet * n;
		ksAppend (
			returned,
			n = ksNew (30,
				   keyNew ("system:/elektra/modules/journald", KEY_VALUE, "journald plugin waits for your orders", KEY_END),
				   keyNew ("system:/elektra/modules/journald/exports", KEY_END),
				   keyNew ("system:/elektra/modules/journald/exports/get", KEY_FUNC, elektraJournaldGet, KEY_END),
				   keyNew ("system:/elektra/modules/journald/exports/commit", KEY_FUNC, elektraJournaldCommit, KEY_END),
				   keyNew ("system:/elektra/modules/journald/exports/error", KEY_FUNC, elektraJournaldError, KEY_END),
#include "./readme_journald.c"
				   keyNew ("system:/elektra/modules/journald/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END));
		ksDel (n);
		return 1;
	}

	if (strncmp (keyString (ksLookupByName (elektraPluginGetConfig (handle), "/log/get", 0)), "1", 1) == 0)
	{
		sd_journal_send ("MESSAGE=loading configuration %s", keyName (parentKey), "MESSAGE_ID=fc65eab25c18463f97e4f9b61ea31eae",
				 "PRIORITY=5", /* notice priority */
				 "HOME=%s", getenv ("HOME"), "USER=%s", getenv ("USER"), "PAGE_SIZE=%li", sysconf (_SC_PAGESIZE),
				 "N_CPUS=%li", sysconf (_SC_NPROCESSORS_ONLN), NULL);
	}

	return 1;
}

int elektraJournaldCommit (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	sd_journal_send ("MESSAGE=committed configuration %s with %zd keys", keyName (parentKey), ksGetSize (returned),
			 "MESSAGE_ID=fc65eab25c18463f97e4f9b61ea31eae", "PRIORITY=5", /* notice priority */
			 "HOME=%s", getenv ("HOME"), "USER=%s", getenv ("USER"), "PAGE_SIZE=%li", sysconf (_SC_PAGESIZE), "N_CPUS=%li",
			 sysconf (_SC_NPROCESSORS_ONLN), NULL);
	return 1;
}

int elektraJournaldError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	sd_journal_send ("MESSAGE=rollback configuration %s with %zd keys", keyName (parentKey), ksGetSize (returned),
			 "MESSAGE_ID=fb3928ea453048649c61d62619847ef6", "PRIORITY=3", /* error priority */
			 "HOME=%s", getenv ("HOME"), "USER=%s", getenv ("USER"), "PAGE_SIZE=%li", sysconf (_SC_PAGESIZE), "N_CPUS=%li",
			 sysconf (_SC_NPROCESSORS_ONLN), NULL);

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("journald",
		ELEKTRA_PLUGIN_GET,	&elektraJournaldGet,
		ELEKTRA_PLUGIN_COMMIT,	&elektraJournaldCommit,
		ELEKTRA_PLUGIN_ERROR,	&elektraJournaldError,
		ELEKTRA_PLUGIN_END);
}
