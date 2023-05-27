/**
 * @file
 *
 * @brief Header for things needed for external programs
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_EXTERNAL_H
#define ELEKTRA_KDB_EXTERNAL_H

#include <kdb.h>

const char * getExternalBin (KeySet * binaries, const char * key);

int runExternal (const char * bin, char ** argv, Key * errorKey);
int loadExternalSpec (KeySet * spec, KeySet * binaries, Key * errorKey);
int tryLoadExternal (char * commandName, KeySet * binaries);

#endif // ELEKTRA_KDB_EXTERNAL_H
