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
#include <elektra/plugin/plugin.h>

int RUBY_PLUGIN_FUNCTION (Open) (ckdb::Plugin * handle, ckdb::Key * errorKey);
int RUBY_PLUGIN_FUNCTION (Close) (ckdb::Plugin * handle, ckdb::Key * errorKey);
int RUBY_PLUGIN_FUNCTION (Get) (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int RUBY_PLUGIN_FUNCTION (Set) (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int RUBY_PLUGIN_FUNCTION (Error) (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int RUBY_PLUGIN_FUNCTION (CheckConf) (ckdb::Key * errorKey, ckdb::KeySet * conf);

ckdb::Plugin * ELEKTRA_PLUGIN_EXPORT;
}

#endif
