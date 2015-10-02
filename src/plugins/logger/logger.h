/**
* \file
*
* \brief Header for logger plugin
*
* \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/

#ifndef ELEKTRA_PLUGIN_LOGGER_H
#define ELEKTRA_PLUGIN_LOGGER_H

#include <kdbplugin.h>


int elektraLoggerOpen(Plugin *handle, Key *errorKey);
int elektraLoggerClose(Plugin *handle, Key *errorKey);
int elektraLoggerGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraLoggerSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraLoggerError(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(logger);

#endif
