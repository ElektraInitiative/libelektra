/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/core.h>
#include <elektra/plugin/plugin.h>

extern "C" {

namespace ckdb
{
ssize_t keySetRaw (ckdb::Key * key, const void * newBinary, size_t dataSize);
}

int elektraDumpGet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraDumpSet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
ckdb::Plugin * ELEKTRA_PLUGIN_EXPORT;

} // extern C
