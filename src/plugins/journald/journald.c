/**
 * @file
 *
 * @brief A plugin which logs write operations and errors via the native journald interface
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include <stdlib.h>
#include <systemd/sd-journal.h>
#include <unistd.h>

#include "journald.h"

int elektraJournaldGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/journald"))
	{
		ElektraKeyset * n;
		elektraKeysetAppend (
			returned,
			n = elektraKeysetNew (30,
				   elektraKeyNew ("system:/elektra/modules/journald", ELEKTRA_KEY_VALUE, "journald plugin waits for your orders", ELEKTRA_KEY_END),
				   elektraKeyNew ("system:/elektra/modules/journald/exports", ELEKTRA_KEY_END),
				   elektraKeyNew ("system:/elektra/modules/journald/exports/get", ELEKTRA_KEY_FUNC, elektraJournaldGet, ELEKTRA_KEY_END),
				   elektraKeyNew ("system:/elektra/modules/journald/exports/set", ELEKTRA_KEY_FUNC, elektraJournaldSet, ELEKTRA_KEY_END),
				   elektraKeyNew ("system:/elektra/modules/journald/exports/error", ELEKTRA_KEY_FUNC, elektraJournaldError, ELEKTRA_KEY_END),
#include "readme_journald.c"
				   elektraKeyNew ("system:/elektra/modules/journald/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END));
		elektraKeysetDel (n);
		return 1;
	}

	if (strncmp (elektraKeyString (elektraKeysetLookupByName (elektraPluginGetConfig (handle), "/log/get", 0)), "1", 1) == 0)
	{
		sd_journal_send ("MESSAGE=loading configuration %s", elektraKeyName (parentKey), "MESSAGE_ID=fc65eab25c18463f97e4f9b61ea31eae",
				 "PRIORITY=5", /* notice priority */
				 "HOME=%s", getenv ("HOME"), "USER=%s", getenv ("USER"), "PAGE_SIZE=%li", sysconf (_SC_PAGESIZE),
				 "N_CPUS=%li", sysconf (_SC_NPROCESSORS_ONLN), NULL);
	}

	return 1;
}

int elektraJournaldSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	sd_journal_send ("MESSAGE=committed configuration %s with %zd keys", elektraKeyName (parentKey), elektraKeysetGetSize (returned),
			 "MESSAGE_ID=fc65eab25c18463f97e4f9b61ea31eae", "PRIORITY=5", /* notice priority */
			 "HOME=%s", getenv ("HOME"), "USER=%s", getenv ("USER"), "PAGE_SIZE=%li", sysconf (_SC_PAGESIZE), "N_CPUS=%li",
			 sysconf (_SC_NPROCESSORS_ONLN), NULL);
	return 1;
}

int elektraJournaldError (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	sd_journal_send ("MESSAGE=rollback configuration %s with %zd keys", elektraKeyName (parentKey), elektraKeysetGetSize (returned),
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
		ELEKTRA_PLUGIN_SET,	&elektraJournaldSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraJournaldError,
		ELEKTRA_PLUGIN_END);
}
