/***************************************************************************
            trie.c  -  low level functions for backend mounting.
                             -------------------
    begin                : Sat Nov 3
    copyright            : (C) 2007 by Patrick Sabin
    email                : patricksabin@gmx.at
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

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#include "kdbinternal.h"

static Trie* elektraTrieInsert(Trie *trie, const char *name, const void *value);
static void* prefix_lookup(Trie *trie, const char *name);
static char* starts_with(const char *str, char *substr);
static void* prefix_lookup(Trie *trie, const char *name);


Backend* trieLookup(Trie *trie, const Key *key)
{
	char *where=0;
	Backend *ret=0;
	size_t len=0;

	if (!trie) return 0;

	len = keyGetNameSize(key) + 1;
	where = kdbiMalloc(len);
	strncpy(where, keyName(key), len);
	where[len-2] = '/';

	ret = prefix_lookup(trie,where);
	kdbiFree(where);

	return ret;
}

/** Creates a trie from a given configuration.
 *
 * The config will be deleted within this function.
 *
 * @param config the configuration which should be used to build up the trie.
 * @return created trie on success, 0 on failure
 */
Trie *trieOpen(KeySet *config)
{
	Trie *trie = 0;
	Key *root;
	Key *cur;

	ksRewind(config);
	root=ksLookupByName(config, KDB_KEY_MOUNTPOINTS, 0);

	if (!root)
	{
		kdbPrintDebug ("Could not find any mountpoint configuration");
		goto error;
	}

	while ((cur = ksNext(config)) != 0)
	{
		if (keyRel (root, cur) == 1)
		{
			KeySet *cut = ksCut(config, cur);
			Backend *backend = elektraBackendOpen(cut);
			char *mountpoint;

			if (!backend)
			{
				kdbPrintDebug("Ignored invalid backend");
			}

			if (!backend->mountpoint)
			{
				kdbPrintDebug("backend has no mountpoint");

				ksDel(cut);
				elektraBackendClose(backend);
				continue;
			}
			
			if (!strcmp(keyName(backend->mountpoint), ""))
			{
				/* Mount as root backend */
				mountpoint = kdbiStrDup ("");
			} else {
				/* Prepare the name for the mountpoint*/
				mountpoint = kdbiMalloc (keyGetNameSize(backend->mountpoint)+1);
				sprintf(mountpoint,"%s/",keyName(backend->mountpoint));
			}
			trie = elektraTrieInsert(trie, mountpoint, (void*)backend);
			kdbiFree(mountpoint);
		}
	}

	ksDel (config);
	return trie;

error:
	trieClose (trie);
	ksDel (config);
	return 0;
}

int trieClose (Trie *trie)
{
	int i;
	if (trie==NULL) return 0;
	for (i=0;i<MAX_UCHAR;i++) {
		if (trie->text[i]!=NULL) {
			trieClose(trie->children[i]);
			if (trie->value[i])
				elektraBackendClose(trie->value[i]);
			free(trie->text[i]);
		}
	}
	if (trie->empty_value)
		elektraBackendClose(trie->empty_value);
	free(trie);
	return 0;
}


/******************
 * Private static declarations
 ******************/

static Trie* elektraTrieInsert(Trie *trie, const char *name, const void *value)
{
	char* p;
	int i;
	unsigned int idx;

	if (name==0) name="";
	idx=(unsigned int) name[0];

	if (trie==NULL) {
		trie=malloc(sizeof(Trie));
		trie->empty_value=0;
		for (i=0;i<MAX_UCHAR;i++) {
			trie->children[i]=0;
			trie->text[i]=0;
			trie->textlen[i]=0;
			trie->value[i]=0;
		}

		if (!strcmp("",name)) {
			trie->empty_value=(void*)value;
			return trie;
		}

		trie->textlen[idx]=strlen(name);

		trie->text[idx]=kdbiStrDup(name);

		trie->value[idx]=(void*)value;
		return trie;
	}

	if (!strcmp("",name)) {
		trie->empty_value=(void*)value;
		return trie;
	}

	if (trie->text[idx]) {
		/* there exists an entry with the same first character */
		if ((p=starts_with(name, trie->text[idx]))==0) {
			/* the name in the trie is part of the searched name --> continue search */
			trie->children[idx]=elektraTrieInsert(trie->children[idx],name+trie->textlen[idx],value);
		} else {
			/* name in trie doesn't match name --> split trie */
			char *newname;
			Trie *child;
			unsigned int idx2;

			newname=kdbiStrDup(p);
			*p=0; /* shorten the old name in the trie */
			trie->textlen[idx]=strlen(trie->text[idx]);

			child=trie->children[idx];

			/* insert the name given as a parameter into the new trie entry */
			trie->children[idx]=elektraTrieInsert(NULL, name+(p-trie->text[idx]), value);

			/* insert the splitted try into the new trie entry */

			idx2=(unsigned int) newname[0];
			trie->children[idx]->text[idx2]=newname;
			trie->children[idx]->textlen[idx2]=strlen(newname);
			trie->children[idx]->value[idx2]=trie->value[idx];
			trie->children[idx]->children[idx2]=child;

			trie->value[idx]=0;

		}
	} else {
		/* there doesn't exist an entry with the same first character */
		trie->text[idx]=kdbiStrDup(name);
		trie->value[idx]=(void*)value;
		trie->textlen[idx]=strlen(name);
	}

	return trie;
}

#if 0

static Trie *delete_trie(Trie *trie, char *name, CloseMapper closemapper)
{
	Trie *tr;
	unsigned int idx;
	if (trie==NULL) {
		return NULL;
	}

	idx=(unsigned int) name[0];

	if (trie->text[idx]==NULL) {
		return NULL;
	}

	if (starts_with(name,trie->text[idx])==0) {

		tr=delete_trie(trie->children[idx],name+trie->textlen[idx],closemapper);

		if (tr==NULL) {
			/* child trie has been deleted */
			trie->children[idx]=NULL;
			free(trie->text[idx]);
			closemapper(trie->value[idx]);
			trie->text[idx]=NULL;
		}

		return trie;
	}
	return NULL;
}

#endif

/* return NULL if string starts with substring, except for the terminating '\0',
 * otherwise return a pointer to the first mismatch in substr.
 */
static char* starts_with(const char *str, char *substr)
{
	int i = 0;
	int sublen = strlen(substr);

	for (i=0;i<sublen;i++) {
		if (substr[i]!=str[i])
			return substr+i;
	}
	return 0;
}

static void* prefix_lookup(Trie *trie, const char *name)
{
	unsigned int idx;
	void * ret=NULL;
	if (trie==NULL) return NULL;

	idx=(unsigned int) name[0];

	if (trie->text[idx]==NULL) {
		return trie->empty_value;
	}

	if (starts_with((char*)name, (char*)trie->text[idx])==0) {
		ret=prefix_lookup(trie->children[idx],name+trie->textlen[idx]);
	} else {
		return trie->empty_value;
	}

	if (ret==NULL && trie->value[idx]==NULL) {
		return trie->empty_value;
	}
	if (ret==NULL) return trie->value[idx];
	return ret;
}
