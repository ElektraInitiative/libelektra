/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_TCL_H
#define ELEKTRA_PLUGIN_TCL_H

#include <kdbplugin.h>

#include <keyset.hpp>

#include <ostream>

namespace elektra
{

void serialise (std::ostream & os, kdb::KeySet & output, kdb::Key & parent);
void unserialise (std::istream & os, kdb::KeySet & output, kdb::Key & parent);
} // namespace elektra

extern "C" {

int elektraTclGet (::Plugin * handle, ::KeySet * ks, ::Key * parentKey);
int elektraTclSet (::Plugin * handle, ::KeySet * ks, ::Key * parentKey);
int elektraTclError (::Plugin * handle, ::KeySet * ks, ::Key * parentKey);

::Plugin * ELEKTRA_PLUGIN_EXPORT;

} // end extern "C"

#endif
