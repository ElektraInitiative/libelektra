/**
 * @file
 *
 * @brief Source for mini plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include "mini.h"

#include <kdbhelper.h>

/* -- Macros ---------------------------------------------------------------------------------------------------------------------------- */

#define KEYSET_UNCHANGED 0
#define KEYSET_MODIFIED 1

/* -- Functions ------------------------------------------------------------------------------------------------------------------------- */

// ===========
// = Private =
// ===========

static inline KeySet * elektraMiniContract ()
{
	return ksNew (30, keyNew ("system/elektra/modules/mini", KEY_VALUE, "mini plugin waits for your orders", KEY_END),
		      keyNew ("system/elektra/modules/mini/exports", KEY_END),
		      keyNew ("system/elektra/modules/mini/exports/get", KEY_FUNC, elektraMiniGet, KEY_END),
		      keyNew ("system/elektra/modules/mini/exports/set", KEY_FUNC, elektraMiniSet, KEY_END),
#include ELEKTRA_README (mini)
		      keyNew ("system/elektra/modules/mini/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
}

// ====================
// = Plugin Interface =
// ====================

int elektraMiniGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/mini"))
	{
		KeySet * contract = elektraMiniContract ();
		ksAppend (returned, contract);
		ksDel (contract);

		return KEYSET_MODIFIED;
	}

	return KEYSET_UNCHANGED;
}

int elektraMiniSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return KEYSET_UNCHANGED;
}


Plugin * ELEKTRA_PLUGIN_EXPORT (mini)
{
	// clang-format off
	return elektraPluginExport ("mini",
		ELEKTRA_PLUGIN_GET,	&elektraMiniGet,
		ELEKTRA_PLUGIN_SET,	&elektraMiniSet,
		ELEKTRA_PLUGIN_END);
}
