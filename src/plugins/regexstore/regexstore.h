/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_REGEXSTORE_H
#define ELEKTRA_PLUGIN_REGEXSTORE_H

#include <kdbplugin.h>


int elektraRegexstoreOpen (ckdb::Plugin * handle, ckdb::Key * errorKey);
int elektraRegexstoreClose (ckdb::Plugin * handle, ckdb::Key * errorKey);
int elektraRegexstoreGet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraRegexstoreSet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);

#endif
