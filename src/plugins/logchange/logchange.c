/**
* \file
*
* \brief Source for logchange plugin
*
* \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/


#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <string.h>

#include "logchange.h"

int elektraLogchangeOpen(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	/* plugin initialization logic */

	return 1; /* success */
}

int elektraLogchangeClose(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	/* free all plugin resources and shut it down */

	return 1; /* success */
}

int elektraLogchangeGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	if (!strcmp(keyName(parentKey), "system/elektra/modules/logchange"))
	{
		KeySet *contract = ksNew (30,
		keyNew ("system/elektra/modules/logchange",
			KEY_VALUE, "logchange plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/logchange/exports", KEY_END),
		keyNew ("system/elektra/modules/logchange/exports/open",
			KEY_FUNC, elektraLogchangeOpen, KEY_END),
		keyNew ("system/elektra/modules/logchange/exports/close",
			KEY_FUNC, elektraLogchangeClose, KEY_END),
		keyNew ("system/elektra/modules/logchange/exports/get",
			KEY_FUNC, elektraLogchangeGet, KEY_END),
		keyNew ("system/elektra/modules/logchange/exports/set",
			KEY_FUNC, elektraLogchangeSet, KEY_END),
		keyNew ("system/elektra/modules/logchange/exports/error",
			KEY_FUNC, elektraLogchangeError, KEY_END),
#include ELEKTRA_README(logchange)
		keyNew ("system/elektra/modules/logchange/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}
	/* get all keys */

	return 1; /* success */
}

int elektraLogchangeSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}

int elektraLogchangeError(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(logchange)
{
	return elektraPluginExport("logchange",
		ELEKTRA_PLUGIN_OPEN,	&elektraLogchangeOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraLogchangeClose,
		ELEKTRA_PLUGIN_GET,	&elektraLogchangeGet,
		ELEKTRA_PLUGIN_SET,	&elektraLogchangeSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraLogchangeError,
		ELEKTRA_PLUGIN_END);
}

