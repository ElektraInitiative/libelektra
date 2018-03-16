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

Plugin * ELEKTRA_PLUGIN_EXPORT (internalnotification);

// Not exported by plugin; used for testing
void elektraInternalnotificationUpdateRegisteredKeys (Plugin * plugin, KeySet * keySet);

// Conversion functions
void elektraInternalnotificationConvertInt (Key * key, void * context);
void elektraInternalnotificationConvertLong (Key * key, void * context);
void elektraInternalnotificationConvertUnsignedLong (Key * key, void * context);
void elektraInternalnotificationConvertFloat (Key * key, void * context);
void elektraInternalnotificationConvertDouble (Key * key, void * context);

#endif
