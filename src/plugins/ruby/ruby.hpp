/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_RUBY_H
#define ELEKTRA_PLUGIN_RUBY_H

#define RUBY_PLUGIN_NAME ruby
#define RUBY_PLUGIN_NAME_STR "ruby"

#define RUBY_PLUGIN_FUNCTION(func) ELEKTRA_PLUGIN_FUNCTION (func)

extern "C" {
#include <kdbplugin.h>

int RUBY_PLUGIN_FUNCTION (Open) (::Plugin * handle, ::Key * errorKey);
int RUBY_PLUGIN_FUNCTION (Close) (::Plugin * handle, ::Key * errorKey);
int RUBY_PLUGIN_FUNCTION (Get) (::Plugin * handle, ::KeySet * ks, ::Key * parentKey);
int RUBY_PLUGIN_FUNCTION (Set) (::Plugin * handle, ::KeySet * ks, ::Key * parentKey);
int RUBY_PLUGIN_FUNCTION (Error) (::Plugin * handle, ::KeySet * ks, ::Key * parentKey);
int RUBY_PLUGIN_FUNCTION (CheckConf) (::Key * errorKey, ::KeySet * conf);

::Plugin * ELEKTRA_PLUGIN_EXPORT;
}

#endif
