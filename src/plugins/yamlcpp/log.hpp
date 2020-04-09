/**
 * @file
 *
 * @brief Log data using Elektra’s logging facility
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAMLCPP_LOG_HPP
#define ELEKTRA_PLUGIN_YAMLCPP_LOG_HPP

#include <kdb.hpp>
#include <kdblogger.h>

#ifdef HAVE_LOGGER

namespace yamlcpp
{

/**
 * @brief This function uses Elektra’s logging facility to print the contents of a key set.
 *
 * @param keys This parameter stores the key set this function prints.
 */
void logKeySet (kdb::KeySet const & keys);

}

#endif // HAVE_LOGGER

#endif // ELEKTRA_PLUGIN_YAMLCPP_LOG_HPP
