/**
 * @file
 * @ingroup kdbio
 *
 * @brief Elektra-I/O functions and declarations for the I/O binding test suite
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef KDB_IO_PLUGIN_H_
#define KDB_IO_PLUGIN_H_

#include <elektra/kdbio.h>
#include <elektra/kdbplugin.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

/**
 * Set I/O binding for asynchronous I/O operations for plugin.
 *
 * Implemented by plugins to set the I/O binding.
 *
 * @param  plugin     plugin handle
 * @param  parameters contains the binary key "/ioBinding" with a pointer to ElektraIoInterface
 */
typedef void (*ElektraIoPluginSetBinding) (Plugin * plugin, KeySet * parameters);

#ifdef __cplusplus
}
}
#endif

#endif
