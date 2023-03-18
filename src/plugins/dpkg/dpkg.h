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

#include <elektra/plugin/plugin.h>


int elektraDpkgGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraDpkgSet (Plugin * handle, KeySet * ks, Key * parentKey);
Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
