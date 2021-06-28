/**
 * @file meta.h
 *
 * @brief Functions for reading and writing metakeys from and into comment strings
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_TOML_META_H
#define ELEKTRA_PLUGIN_TOML_META_H

#include <kdb.h>
#include <stdbool.h>
#include <stdio.h>

bool shouldWriteMetakey (const Key * meta);
bool isMetakeyComment (const char * comment);
int writeMetakeyAsComment (const Key * meta, FILE * f);
int assignMetakeyFromComment (Key * key, const char * comment);

#endif // ELEKTRA_PLUGIN_TOML_META_H
