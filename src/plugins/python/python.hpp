/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_PYTHON_H

#define PYTHON_PLUGIN_FUNCTION(func) PYTHON_PLUGIN_FUNCTION2 (PYTHON_PLUGIN_SYMBOL_NAME, func)
#define PYTHON_PLUGIN_FUNCTION2(module, func) ELEKTRA_PLUGIN_FUNCTION (module, func)

#define PYTHON_PLUGIN_EXPORT(module) ELEKTRA_PLUGIN_EXPORT (module)

extern "C" {
#include <kdbplugin.h>

int PYTHON_PLUGIN_FUNCTION (Open) (ckdb::Plugin * handle, ckdb::Key * errorKey);
int PYTHON_PLUGIN_FUNCTION (Close) (ckdb::Plugin * handle, ckdb::Key * errorKey);
int PYTHON_PLUGIN_FUNCTION (Get) (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int PYTHON_PLUGIN_FUNCTION (Set) (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int PYTHON_PLUGIN_FUNCTION (Error) (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);

ckdb::Plugin * PYTHON_PLUGIN_EXPORT (PYTHON_PLUGIN_NAME);
}

#endif
