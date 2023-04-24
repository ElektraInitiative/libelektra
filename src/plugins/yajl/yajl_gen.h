/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef YAJL_GEN_H
#define YAJL_GEN_H

#include "yajl.h"

#include <stdio.h>
#include <string.h>

#include <elektra/kdb/errors.h>
#include <internal/macros/old_utils.h>

#include <yajl/yajl_gen.h>


#include "iterator.h"
#include "name.h"

typedef enum
{
	/**
	 * We are at end of string, so no lookahead.
	 */
	LOOKAHEAD_END = 0,
	/**
	 * We are iterating in the middle of an array (or at least
	 * certainly not starting it)
	 */
	LOOKAHEAD_ARRAY = 1,
	/**
	 * We found a special marker for empty arrays.
	 * @todo not implemented yet
	 */
	LOOKAHEAD_EMPTY_ARRAY = 3,
	/**
	 * We found a special marker for empty maps.
	 * @todo not implemented yet
	 */
	LOOKAHEAD_EMPTY_MAP = 4,
	/**
	 * We are iterating a map or starting it.
	 */
	LOOKAHEAD_MAP = 5
} lookahead_t;

lookahead_t elektraLookahead (const char * pnext, size_t size);

void elektraGenOpenInitial (yajl_gen g, Key * parentKey, const Key * first);
void elektraGenOpen (yajl_gen g, const Key * cur, const Key * next);

void elektraGenClose (yajl_gen g, const Key * cur, const Key * next);
void elektraGenCloseFinally (yajl_gen g, const Key * cur, const Key * next);

#endif
