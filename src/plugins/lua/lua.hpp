/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_LUA_H

extern "C" {
#include <kdbplugin.h>

int elektraLuaOpen (::Plugin * handle, ::Key * errorKey);
int elektraLuaClose (::Plugin * handle, ::Key * errorKey);
int elektraLuaGet (::Plugin * handle, ::KeySet * ks, ::Key * parentKey);
int elektraLuaSet (::Plugin * handle, ::KeySet * ks, ::Key * parentKey);
int elektraLuaError (::Plugin * handle, ::KeySet * ks, ::Key * parentKey);

::Plugin * ELEKTRA_PLUGIN_EXPORT;
}

#endif
