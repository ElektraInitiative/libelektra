/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbplugin.h>

extern "C" {

ssize_t keySetRaw (::Key * key, const void * newBinary, size_t dataSize);

int elektraDumpGet (::Plugin * handle, ::KeySet * ks, ::Key * parentKey);
int elektraDumpSet (::Plugin * handle, ::KeySet * ks, ::Key * parentKey);
::Plugin * ELEKTRA_PLUGIN_EXPORT;

} // extern C
