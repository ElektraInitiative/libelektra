/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef KDBLIBLOADER_H
#define KDBLIBLOADER_H

#include <kdb.h>
#include <kdbplugin.h>

#ifdef __cplusplus
namespace ckdb {
extern "C" {
#endif

/* The pointer to a function which will create a plugin */
typedef Plugin *(*elektraPluginFactory) (void);

int elektraModulesInit (KeySet *modules, Key *error);
elektraPluginFactory elektraModulesLoad (KeySet *modules, const char *name, Key *error);
int elektraModulesClose (KeySet *modules, Key *error);

#ifdef __cplusplus
}
}
#endif

#endif /* KDBLIBLOADER_H */
