/**
 * @file
 *
 * @brief Declarations for the ev I/O binding.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef KDB_IOWRAPPER_EV_H_
#define KDB_IOWRAPPER_EV_H_

#include <ev.h>
#include <kdbio.h>

/**
 * Create and initialize a new I/O binding.
 * @param  loop Loop to use for I/O operations
 * @return      Populated I/O interface
 */
ElektraIoInterface * elektraIoEvNew (struct ev_loop * loop);

#endif
