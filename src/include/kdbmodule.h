/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KDBMODULE_H
#define KDBMODULE_H

#include <kdb.h>
#include <kdbplugin.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

typedef Plugin * (*elektraPluginFactory) (void);

int elektraModulesInit (ElektraKeyset * modules, ElektraKey * error);
elektraPluginFactory elektraModulesLoad (ElektraKeyset * modules, const char * name, ElektraKey * error);
int elektraModulesClose (ElektraKeyset * modules, ElektraKey * error);


#ifdef __cplusplus
}
}
#endif

#endif
