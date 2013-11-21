/***************************************************************************
                     yajl.c  -  Skeleton of a plugin
                             -------------------
    begin                : Fri May 21 2010
    copyright            : (C) 2010 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This is the skeleton of the methods you'll have to implement in order *
 *   to provide a valid plugin.                                            *
 *   Simple fill the empty functions with your code and you are            *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#ifndef ELEKTRA_PLUGIN_YAJL_H
#define ELEKTRA_PLUGIN_YAJL_H

#include <kdbplugin.h>

typedef struct _keyNameReverseIterator
{
	const char *rbegin;  // begin of name (constant during iteration)
	const char *rend;    // end of name (constant during iteration)
	const char *current; // current position
	size_t size;         // size of current substring (beginning from position)
} keyNameReverseIterator;

keyNameReverseIterator elektraKeyNameGetReverseIterator(const Key *k);
int elektraKeyNameReverseNext(keyNameReverseIterator *it);
int elektraKeyIsSibling(Key *cur, Key *prev);
int elektraArrayIncName(Key *key);
Key * elektraNextNotBelow(KeySet *ks);
ssize_t elektraKeyCountLevel(const Key *cur);
ssize_t elektraKeyCountEqualLevel(const Key *cmp1, const Key *cmp2);

int elektraYajlOpen(Plugin *handle, Key *errorKey);
int elektraYajlClose(Plugin *handle, Key *errorKey);
int elektraYajlGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraYajlSet(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(yajl);

#endif
