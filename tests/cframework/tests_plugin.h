/** @file
  *
  * Some common functions operating on plugins.
  *
  * @copyright This program is free software; you can redistribute it and/or modify
  *            it under the terms of the BSD License (revised).
  *
  * If you include this file you have full access to elektra's internals
  * and your test might not be ABI compatible with the next release.
  *
  * @see tests_internal.h
  */

#include <tests_internal.h>

#define PLUGIN_OPEN(NAME) \
		KeySet *modules = ksNew(0, KS_END); \
		elektraModulesInit(modules, 0); \
		Plugin *plugin = elektraPluginOpen(NAME, modules, conf, 0); \
		succeed_if (plugin != 0, "could not open " NAME " plugin"); \

#define PLUGIN_CLOSE() \
		elektraPluginClose (plugin, 0); \
		elektraModulesClose(modules, 0); \
		ksDel (modules);
