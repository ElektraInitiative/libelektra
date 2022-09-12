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

#include <kdbplugin.h>


int elektraCurlgetGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraCurlgetSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraCurlgetCommit (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraCurlgetOpen (Plugin * handle, ElektraKey * errorKey);
Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
