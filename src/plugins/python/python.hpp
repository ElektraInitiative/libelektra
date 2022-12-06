/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_PYTHON_H

#define PYTHON_PLUGIN_FUNCTION(func) ELEKTRA_PLUGIN_FUNCTION (func)

#define PYTHON_PLUGIN_EXPORT(module) ELEKTRA_PLUGIN_EXPORT

extern "C" {
#include <elektra/kdbplugin.h>

int PYTHON_PLUGIN_FUNCTION (Open) (ckdb::Plugin * handle, ckdb::Key * errorKey);
int PYTHON_PLUGIN_FUNCTION (Close) (ckdb::Plugin * handle, ckdb::Key * errorKey);
int PYTHON_PLUGIN_FUNCTION (Get) (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int PYTHON_PLUGIN_FUNCTION (Set) (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int PYTHON_PLUGIN_FUNCTION (Error) (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);

ckdb::Plugin * PYTHON_PLUGIN_EXPORT (PYTHON_PLUGIN_NAME);
}

#endif
