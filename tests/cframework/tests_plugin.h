/**
 * @file
 *
 * @brief Some common functions operating on plugins.
 *
 * If you include this file you have full access to elektra's internals
 * and your test might not be ABI compatible with the next release.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <internal/plugin/load.h>
#include <tests_internal.h>

#define PLUGIN_OPEN(NAME)                                                                                                                  \
	KeySet * modules = ksNew (0, KS_END);                                                                                              \
	elektraModulesInit (modules, 0);                                                                                                   \
	Key * errorKey = keyNew ("/", KEY_END);                                                                                            \
	Plugin * plugin = elektraPluginOpen (NAME, modules, conf, errorKey);                                                               \
	succeed_if (output_warnings (errorKey), "warnings in kdbOpen for plugin " NAME);                                                   \
	succeed_if (output_error (errorKey), "error in kdbOpen for plugin " NAME);                                                         \
	keyDel (errorKey);                                                                                                                 \
	exit_if_fail (plugin != 0, "could not open " NAME " plugin")

#define PLUGIN_CLOSE()                                                                                                                     \
	elektraPluginClose (plugin, 0);                                                                                                    \
	elektraModulesClose (modules, 0);                                                                                                  \
	ksDel (modules)
