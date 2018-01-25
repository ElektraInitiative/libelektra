/**
 * @file
 *
 * @brief Header for internalnotification plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_INTERNALNOTIFICATION_H
#define ELEKTRA_PLUGIN_INTERNALNOTIFICATION_H

#include <kdbnotification.h>
#include <kdbplugin.h>


int elektraInternalnotificationGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraInternalnotificationSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraInternalnotificationClose (Plugin * handle, Key * errorKey);
int elektraInternalnotificationOpen (Plugin * handle, Key * errorKey);
typedef int (*ElektraInternalnotificationRegisterInt) (Plugin * handle, Key * key, int * variable);
typedef int (*ElektraInternalnotificationRegisterCallback) (Plugin * handle, Key * key, ElektraNotificationChangeCallback callback);

Plugin * ELEKTRA_PLUGIN_EXPORT (internalnotification);

#endif
