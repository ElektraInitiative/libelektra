/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAJL_ITERATOR_H
#define ELEKTRA_PLUGIN_YAJL_ITERATOR_H

#include <elektra/kdb.h>

typedef struct _keyNameReverseIterator
{
	const char * rbegin;  ///< begin of name (constant during iteration)
	const char * rend;    ///< end of name (constant during iteration)
	const char * current; ///< current position
	size_t size;	      ///< size of current substring (beginning from position)
} keyNameReverseIterator;

keyNameReverseIterator elektraKeyNameGetReverseIterator (const Key * k);
int elektraKeyNameReverseNext (keyNameReverseIterator * it);

Key * elektraNextNotBelow (KeySet * ks, elektraCursor pos);

#endif
