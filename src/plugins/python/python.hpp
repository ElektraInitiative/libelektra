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
#include <kdbplugin.h>

int PYTHON_PLUGIN_FUNCTION (Open) (::Plugin * handle, ::Key * errorKey);
int PYTHON_PLUGIN_FUNCTION (Close) (::Plugin * handle, ::Key * errorKey);
int PYTHON_PLUGIN_FUNCTION (Get) (::Plugin * handle, ::KeySet * ks, ::Key * parentKey);
int PYTHON_PLUGIN_FUNCTION (Set) (::Plugin * handle, ::KeySet * ks, ::Key * parentKey);
int PYTHON_PLUGIN_FUNCTION (Error) (::Plugin * handle, ::KeySet * ks, ::Key * parentKey);

::Plugin * PYTHON_PLUGIN_EXPORT (PYTHON_PLUGIN_NAME);
}

#endif
