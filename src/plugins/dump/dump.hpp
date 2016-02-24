/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <kdbplugin.h>

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include <cstring>

#define DUMP_PATH "/tmp/dump.edf"

// edf = elektra dump format

namespace dump
{
int serialise (std::ostream & os, ckdb::Key *, ckdb::KeySet * ks);
int unserialise (std::istream & is, ckdb::Key * errorKey, ckdb::KeySet * ks);
}

extern "C" {

namespace ckdb
{
ssize_t keySetRaw (ckdb::Key * key, const void * newBinary, size_t dataSize);
}

int elektraDumpGet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraDumpSet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
ckdb::Plugin * ELEKTRA_PLUGIN_EXPORT (dump);

} // extern C
