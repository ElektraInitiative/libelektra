/**
 * @file
 *
 * @brief Source for yanlr plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "yanlr.hpp"
#include "YAMLLexer.h"
#include "YAMLParser.h"
#include "antlr4-runtime.h"

#include <kdbhelper.h>

using namespace ckdb;
using namespace antlr;
using namespace antlr4;

extern "C" {

int elektraYanlrGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/yanlr"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/yanlr", KEY_VALUE, "yanlr plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/yanlr/exports", KEY_END),
			       keyNew ("system/elektra/modules/yanlr/exports/get", KEY_FUNC, elektraYanlrGet, KEY_END),
			       keyNew ("system/elektra/modules/yanlr/exports/set", KEY_FUNC, elektraYanlrSet, KEY_END),
#include ELEKTRA_README (yanlr)
			       keyNew ("system/elektra/modules/yanlr/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraYanlrSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (yanlr)
{
	return elektraPluginExport ("yanlr", ELEKTRA_PLUGIN_GET, &elektraYanlrGet, ELEKTRA_PLUGIN_SET, &elektraYanlrSet,
				    ELEKTRA_PLUGIN_END);
}

} // end extern "C"
