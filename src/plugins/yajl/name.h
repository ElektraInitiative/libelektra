/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAJL_NAME_H
#define ELEKTRA_PLUGIN_YAJL_NAME_H

#include "kdb.h"

ssize_t elektraKeyCountLevel (const ElektraKey * cur);
ssize_t elektraKeyCountEqualLevel (const ElektraKey * cmp1, const ElektraKey * cmp2);

#endif
