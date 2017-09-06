/**
 * @file
 *
 * @brief Header for haskelltemplate plugin
 *
 * @copyrigh*t BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_HASKELLTEMPLATE_H
#define ELEKTRA_PLUGIN_HASKELLTEMPLATE_H

#include <kdbplugin.h>

int elektraHaskelltemplateOpen (Plugin * handle, Key * errorKey);
int elektraHaskelltemplateClose (Plugin * handle, Key * errorKey);
int elektraHaskelltemplateGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraHaskelltemplateSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraHaskelltemplateError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraHaskelltemplateCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (haskelltemplate);

#endif
