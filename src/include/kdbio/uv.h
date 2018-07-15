/**
 * @file
 *
 * @brief Declarations for the uv I/O binding.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef KDB_IOWRAPPER_UV_H_
#define KDB_IOWRAPPER_UV_H_

#include <kdbio.h>
#include <uv.h>

/**
 * Create and initialize a new I/O binding.
 * @param  loop Loop to use for I/O operations
 * @return      Populated I/O interface
 */
ElektraIoInterface * elektraIoUvNew (uv_loop_t * loop);

#endif
