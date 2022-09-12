/**
 * @file
 *
 * @brief Header for dpkg plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_DPKG_H
#define ELEKTRA_PLUGIN_DPKG_H

#include <kdbplugin.h>


int elektraDpkgGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraDpkgSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
