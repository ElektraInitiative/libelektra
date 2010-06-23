/***************************************************************************
            split.c  -  Functions for splitting keysets for kdbSet
                             -------------------
    begin                : Fri 21 Mar 2008
    copyright            : (C) 2008 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if DEBUG && HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#include <kdbinternal.h>

/**
 * @defgroup split Split :: Represents splitted keysets
 * @brief used internally for kdbSet()
 *
 * Splits up a keyset into multiple keysets where each
 * of them will passed to the correct kdbSet().
**/


/**
 * Allocates a new split object.
 *
 * @return a fresh allocated split object
 * @ingroup split
**/
Split * elektraSplitNew(void)
{
	Split *ret = elektraCalloc (sizeof (Split));

	ret->size = 0;
	ret->alloc = APPROXIMATE_NR_OF_BACKENDS;

	ret->keysets=elektraCalloc(sizeof(KeySet*) * ret->alloc);
	ret->handles=elektraCalloc(sizeof(KDB *) * ret->alloc);
	ret->parents=elektraCalloc(sizeof(Key *) * ret->alloc);
	ret->syncbits=elektraCalloc(sizeof(int) * ret->alloc);
	ret->belowparents=elektraCalloc(sizeof(int) * ret->alloc);

	return ret;
}


/**
 * Delete a split object.
 *
 * Will free all allocated resources of a splitted keyset.
 *
 * @param keysets the split object to work with
 * @ingroup split
 */
void elektraSplitDel(Split *keysets)
{
	for (size_t i=0; i<keysets->size; ++i)
	{
		ksDel(keysets->keysets[i]);
	}
	elektraFree (keysets->keysets);
	elektraFree (keysets->handles);
	elektraFree (keysets->parents);
	elektraFree (keysets->syncbits);
	elektraFree (keysets->belowparents);
	elektraFree (keysets);
}

/**
 * Doubles the size
 */
void elektraSplitResize(Split *ret)
{
	ret->alloc *= 2;

	elektraRealloc((void**) &ret->keysets, ret->alloc * sizeof(KeySet *));
	elektraRealloc((void**) &ret->handles, ret->alloc * sizeof(KDB *));
	elektraRealloc((void**) &ret->parents, ret->alloc * sizeof(Key *));
	elektraRealloc((void**) &ret->syncbits, ret->alloc * sizeof(int));
	elektraRealloc((void**) &ret->belowparents, ret->alloc * sizeof(int));
}

void elektraSplitAppend(Split *ret)
{
	++ ret->size;
	if (ret->size > ret->alloc) elektraSplitResize(ret);

	ret->keysets[ret->size-1]=NULL;
	ret->handles[ret->size-1]=NULL;
	ret->parents[ret->size-1]=NULL;
	ret->syncbits[ret->size-1]=0;
	ret->belowparents[ret->size-1]=0;
}


/* Split keysets.
 * Make sure that parentKey has a name or is a null pointer*/
Split *elektraSplitKeySet(KDB *handle, KeySet *ks,
	Key *parentKey, unsigned long options)
{
	Split *ret;

	Key *curKey;
	Backend *curHandle;
	int curFound;

	ret = elektraSplitNew ();

	ksRewind (ks);
	while ((curKey = ksNext (ks)) != 0)
	{
		curHandle = kdbGetBackend(handle, curKey);
		curFound = 0;

		if (options & KDB_O_SYNC) curKey->flags |= KEY_FLAG_SYNC;

		for (size_t i=0; i<ret->size; i++)
		{
			if (curHandle == ret->handles[i] && 
				(!parentKey || keyIsBelowOrSame(ret->parents[i], curKey)))
			{
				curFound = 1;
				ksAppendKey(ret->keysets[i],curKey);
				if (keyNeedSync (curKey) == 1) ret->syncbits[i]=1;
			}
		}

		if (!curFound)
		{
			elektraSplitAppend (ret);

			ret->keysets[ret->size-1] = ksNew (ksGetSize (ks) / APPROXIMATE_NR_OF_BACKENDS + 2, KS_END);
			ksAppendKey(ret->keysets[ret->size-1],curKey);
			ret->handles[ret->size-1] = curHandle;
			ret->parents[ret->size-1] = curKey;
			if (parentKey)
			{
				ret->belowparents[ret->size-1] = keyIsBelowOrSame (parentKey, curKey);
			} else ret->belowparents[ret->size-1] = 1;
			if (keyNeedSync (curKey) == 1) ret->syncbits[ret->size-1]=1;
		}
	}

	return ret;
}

