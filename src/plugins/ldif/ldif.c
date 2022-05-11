/**
 * @file
 *
 * @brief Source for ldif plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "ldif.h"

#include <errno.h>
#include <kdbhelper.h>
#include <kdberrors.h>

#include <stdio.h>
#include <stdlib.h>

#include <ldap.h>
#include <ldif.h>


int elektraLdifOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraLdifClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraLdifGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/ldif"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/ldif", KEY_VALUE, "ldif plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/ldif/exports", KEY_END),
			       keyNew ("system:/elektra/modules/ldif/exports/open", KEY_FUNC, elektraLdifOpen, KEY_END),
			       keyNew ("system:/elektra/modules/ldif/exports/close", KEY_FUNC, elektraLdifClose, KEY_END),
			       keyNew ("system:/elektra/modules/ldif/exports/get", KEY_FUNC, elektraLdifGet, KEY_END),
			       keyNew ("system:/elektra/modules/ldif/exports/set", KEY_FUNC, elektraLdifSet, KEY_END),
			       keyNew ("system:/elektra/modules/ldif/exports/commit", KEY_FUNC, elektraLdifCommit, KEY_END),
			       keyNew ("system:/elektra/modules/ldif/exports/error", KEY_FUNC, elektraLdifError, KEY_END),
			       keyNew ("system:/elektra/modules/ldif/exports/checkconf", KEY_FUNC, elektraLdifCheckConf, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/ldif/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys
	int errnosave = errno;
	LDIFFP * lfp = ldif_open(keyString (parentKey), "r");

	if (!lfp)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	unsigned long lineno = 0;
	char *buff = NULL;
	int buflen = 0;

	// TODO replace printf with proper error handling
	while (ldif_read_record(lfp, &lineno, &buff, &buflen)) {
		char *line;
		char *next = buff;

		while ((line = ldif_getline(&next)) != NULL) {
			char *type = NULL;
			char *value = NULL;
			ber_len_t vlen;

			if (ldif_parse_line(line, &type, &value, &vlen)) {
				printf("cannot parse line: %s\n", line);
				continue;
			}

			printf ("type: %s, value: %s\n", type, value);

			free (type);
			free (value);
		}
	}

	free(buff);

	ldif_close (lfp);

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraLdifSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraLdifError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// handle errors (commit failed)
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraLdifCommit (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// commit changes
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraLdifCheckConf (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("ldif",
		ELEKTRA_PLUGIN_OPEN,	&elektraLdifOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraLdifClose,
		ELEKTRA_PLUGIN_GET,	&elektraLdifGet,
		ELEKTRA_PLUGIN_SET,	&elektraLdifSet,
		ELEKTRA_PLUGIN_COMMIT,  &elektraLdifCommit,
		ELEKTRA_PLUGIN_ERROR,	&elektraLdifError,
		ELEKTRA_PLUGIN_END);
}
