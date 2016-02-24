/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_TCL_H
#define ELEKTRA_PLUGIN_TCL_H

#include <kdbplugin.h>

#include <keyset.hpp>

#include <ostream>

namespace elektra
{

void serialise (std::ostream & os, kdb::KeySet & output);
void unserialise (std::istream & os, kdb::KeySet & output);
}

extern "C" {

int elektraTclGet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraTclSet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraTclError (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);

ckdb::Plugin * ELEKTRA_PLUGIN_EXPORT (tcl);

} // end extern "C"

#endif
