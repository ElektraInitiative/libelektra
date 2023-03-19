/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAJL_NAME_H
#define ELEKTRA_PLUGIN_YAJL_NAME_H

#include <elektra/old_kdb.h>

ssize_t elektraKeyCountLevel (const Key * cur);
ssize_t elektraKeyCountEqualLevel (const Key * cmp1, const Key * cmp2);

#endif
