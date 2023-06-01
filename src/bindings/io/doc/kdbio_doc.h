/**
 * @file
 *
 * @brief Declarations for the doc I/O binding.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef KDB_IOWRAPPER_UV_H_
#define KDB_IOWRAPPER_UV_H_

#include <elektra/io/api.h>

/**
 * Create and initialize a new doc I/O binding
 * @param  foo Some data from I/O management library (e.g. a handle)
 * @return     Populated I/O interface
 */
ElektraIoInterface * elektraIoDocNew (char * foo);

#endif
