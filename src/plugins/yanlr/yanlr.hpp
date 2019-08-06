/**
 * @file
 *
 * @brief Header for yanlr plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_YANLR_H
#define ELEKTRA_PLUGIN_YANLR_H

#include <kdbplugin.h>

extern "C" {

using ckdb::Key;
using ckdb::KeySet;
using ckdb::Plugin;

int elektraYanlrGet (Plugin *, KeySet *, Key *);

Plugin * ELEKTRA_PLUGIN_EXPORT;

} // end extern "C"

#endif
