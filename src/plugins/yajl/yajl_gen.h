#ifndef YAJL_GEN_H
#define YAJL_GEN_H

#include "yajl.h"

#include <stdio.h>
#include <string.h>

#include <kdbconfig.h>
#include <kdberrors.h>

#include <yajl/yajl_gen.h>


#include "iterator.h"
#include "name.h"

// uncomment to enable logging for yajl_gen functionality
// #define ELEKTRA_YAJL_VERBOSE

// TODO defined privately in keyhelpers.c, API break possible..
char *keyNameGetOneLevel(const char *name, size_t *size);

typedef enum
{
	/**
	 * We are at end of string, so no lookahead.
	 */
	LOOKAHEAD_END,
	/**
	 * We are iterating in the middle of an array (or at least
	 * certainly not starting it)
	 */
	LOOKAHEAD_ARRAY,
	/**
	 * We are starting a new array.
	 */
	LOOKAHEAD_START_ARRAY,
	/**
	 * We found a special marker for empty arrays.
	 * @todo not implemented yet
	 */
	LOOKAHEAD_EMPTY_ARRAY,
	/**
	 * We found a special marker for empty maps.
	 * @todo not implemented yet
	 */
	LOOKAHEAD_EMPTY_MAP,
	/**
	 * We are iterating a map or starting it.
	 */
	LOOKAHEAD_MAP
} lookahead_t;

lookahead_t elektraLookahead(const char* pnext, size_t size);

void elektraGenOpenInitial(yajl_gen g, Key *parentKey, const Key *first);
void elektraGenOpen(yajl_gen g, const Key *cur, const Key *next);

void elektraGenClose(yajl_gen g, const Key *cur, const Key *next);
void elektraGenCloseFinally(yajl_gen g, const Key *cur, const Key *next);

#endif
