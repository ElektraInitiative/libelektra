/**
 * @file
 *
 * @brief cryptographic interface using the gcrypt library
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <botan/init.h>
#include <kdbplugin.h>

using namespace ckdb;
using namespace Botan;

extern "C" {

#include "botan_operations.h"
#include "crypto.h"

int elektraCryptoBotanInit (ckdb::Key * errorKey)
{
	return 1; // success
}

int elektraCryptoBotanEncrypt (ckdb::KeySet * pluginConfig, ckdb::Key * k, ckdb::Key * errorKey)
{
	return -1;
}

int elektraCryptoBotanDecrypt (ckdb::KeySet * pluginConfig, ckdb::Key * k, ckdb::Key * errorKey)
{
	return -1;
}

} // extern "C"
