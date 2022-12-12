/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_augeas_H
#define ELEKTRA_PLUGIN_augeas_H

#include <elektra/kdbextension.h>
#include <elektra/kdbplugin.h>
#include <kdberrors.h>

#include <augeas.h>
#include <errno.h>
#include <libgen.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>


#define AUGEAS_OUTPUT_ROOT "/raw/output"
#define AUGEAS_CONTENT_ROOT "/raw/content"
#define AUGEAS_TREE_ROOT "/raw/tree"

int elektraAugeasGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraAugeasSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraAugeasError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
