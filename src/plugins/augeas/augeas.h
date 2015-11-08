/*
 * \copydoc augeas.c
 */

#ifndef ELEKTRA_PLUGIN_augeas_H
#define ELEKTRA_PLUGIN_augeas_H

#include <kdbplugin.h>
#include <kdberrors.h>
#include <kdbproposal.h>
#include <kdbextension.h>

#include <augeas.h>
#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stddef.h>
#include <errno.h>


#define AUGEAS_OUTPUT_ROOT "/raw/output"
#define AUGEAS_CONTENT_ROOT "/raw/content"
#define AUGEAS_TREE_ROOT "/raw/tree"

int elektraAugeasGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraAugeasSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraAugeasError(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(augeas);

#endif
