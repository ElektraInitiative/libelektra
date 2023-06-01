/**
 * @file
 *
 * @brief Declarations for the glib I/O binding.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef KDB_IOWRAPPER_GLIB_H_
#define KDB_IOWRAPPER_GLIB_H_

#include <elektra/io/api.h>
#include <glib.h>

/**
 * Create and initialize a new I/O binding.
 * @param  context Context to use for I/O operations
 * @return         Populated I/O interface
 */
ElektraIoInterface * elektraIoGlibNew (GMainContext * context);

#endif
