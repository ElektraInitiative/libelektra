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


void elektraSplitClose(Split *keysets)
{
	for (size_t i=0; i<keysets->no; ++i)
	{
		ksDel(keysets->keysets[i]);
	}
	free (keysets->keysets);
	free (keysets->handles);
	free (keysets->parents);
	free (keysets->syncbits);
	free (keysets->belowparents);
	free (keysets);
}

void elektraSplitInit(Split *ret)
{
	ret->no=0;
	ret->keysets=malloc(sizeof(KeySet*));
	ret->keysets[0]=NULL;
	ret->handles=malloc(sizeof(KDB *));
	ret->handles[0]=NULL;
	ret->parents=malloc(sizeof(Key *));
	ret->parents[0]=NULL;
	ret->syncbits=malloc(sizeof(int));
	ret->syncbits[0] = 0;
	ret->belowparents=malloc(sizeof(int));
	ret->belowparents[0] = 0;
}

void resize_splitted_keysets(Split *ret)
{
	ret->no++;
	ret->keysets=realloc(ret->keysets,(ret->no+1)*sizeof(KeySet *));
	ret->keysets[ret->no]=NULL;
	ret->handles=realloc(ret->handles,(ret->no+1)*sizeof(KDB *));
	ret->handles[ret->no]=NULL;
	ret->parents=realloc(ret->parents,(ret->no+1)*sizeof(Key *));
	ret->parents[ret->no]=NULL;
	ret->syncbits=realloc(ret->syncbits,(ret->no+1)*sizeof(int));
	ret->syncbits[ret->no]=0;
	ret->belowparents=realloc(ret->belowparents,(ret->no+1)*sizeof(int));
	ret->belowparents[ret->no]=0;
}

/* Split keysets.
 * Make sure that parentKey has a name or is a null pointer*/
Split *split_keyset(KDB *handle, KeySet *ks,
	Key *parentKey, unsigned long options)
{
	Split *ret;

	Key *curKey;
	Backend *curHandle;
	int curFound;

	ret = malloc (sizeof (Split));
	elektraSplitInit (ret);

	ksRewind (ks);
	while ((curKey = ksNext (ks)) != 0)
	{
		curHandle = kdbGetBackend(handle, curKey);
		curFound = 0;

		if (options & KDB_O_SYNC) curKey->flags |= KEY_FLAG_SYNC;

		for (size_t i=0; i<ret->no; i++)
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
			resize_splitted_keysets (ret);

			ret->keysets[ret->no-1] = ksNew (ksGetSize (ks) / APPROXIMATE_NR_OF_BACKENDS + 2, KS_END);
			ksAppendKey(ret->keysets[ret->no-1],curKey);
			ret->handles[ret->no-1] = curHandle;
			ret->parents[ret->no-1] = curKey;
			if (parentKey)
			{
				ret->belowparents[ret->no-1] = keyIsBelowOrSame (parentKey, curKey);
			} else ret->belowparents[ret->no-1] = 1;
			if (keyNeedSync (curKey) == 1) ret->syncbits[ret->no-1]=1;
		}
	}

	return ret;
}

