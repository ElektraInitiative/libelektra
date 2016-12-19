/**
 * @file
 *
 * @brief Header for curlget plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CURLGET_H
#define ELEKTRA_PLUGIN_CURLGET_H

#include <kdbplugin.h>


int elektraCurlgetGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCurlgetSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (curlget);

#endif
