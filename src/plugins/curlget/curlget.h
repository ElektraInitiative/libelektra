/**
 * @file
 *
 * @brief Header for curlget plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CURLGET_H
#define ELEKTRA_PLUGIN_CURLGET_H

#include <elektra/kdbplugin.h>


int elektraCurlgetGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCurlgetSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCurlgetCommit (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCurlgetOpen (Plugin * handle, Key * errorKey);
Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
