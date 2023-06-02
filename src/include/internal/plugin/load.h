/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_LOAD_H
#define ELEKTRA_PLUGIN_LOAD_H

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/plugin/plugin.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif


Plugin * elektraPluginOpen (const char * backendname, KeySet * modules, KeySet * config, Key * errorKey);
int elektraPluginClose (Plugin * handle, Key * errorKey);
size_t elektraPluginGetFunction (Plugin * plugin, const char * name);

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_PLUGIN_LOAD_H
