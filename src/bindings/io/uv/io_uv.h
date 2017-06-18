/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef KDB_IOWRAPPER_UV_H_
#define KDB_IOWRAPPER_UV_H_

#include <kdbio.h>
#include <uv.h>

ElektraIoInterface * elektraIoUvNew (uv_loop_t * loop);

#endif
