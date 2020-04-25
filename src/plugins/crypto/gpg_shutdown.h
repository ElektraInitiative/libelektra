/**
 * @file
 *
 * @brief module for shutting down the gpg-agent
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CRYPTO_GPGSHUTDOWN_H
#define ELEKTRA_PLUGIN_CRYPTO_GPGSHUTDOWN_H

#include <kdb.h>
#include <kdbplugin.h>

int ELEKTRA_PLUGIN_FUNCTION (gpgQuitAgent) (void);

#endif
