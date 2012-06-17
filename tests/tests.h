/**Some common functions in use for testing framework*/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#ifndef TESTS_H
#define TESTS_H

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_TIME_H
#include <time.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#include <kdbinternal.h>

#define BUFFER_LENGTH 4096

extern int nbError;
extern int nbTest;

extern uid_t nbUid;
extern gid_t nbGid;

int init(int argc, char** argv);

#define warn_if_fail(x,y) {nbTest++; if (!(x)) { printf("%s:%d: warn in %s: %s\n", __FILE__, __LINE__, __FUNCTION__, y); }}
#define succeed_if(x,y) {nbTest++; if (!(x)) { nbError++; printf("%s:%d: error in %s: %s\n", __FILE__, __LINE__, __FUNCTION__, y); }}
#define exit_if_fail(x,y) {nbTest++; if (!(x)) { printf("%s:%d: fatal in %s: %s\n", __FILE__, __LINE__, __FUNCTION__, y); exit(1); }}

int compare_key (Key *k1, Key *k2);
int compare_keyset (KeySet *ks, KeySet *ks2);
int compare_files (const char * filename);

char *srcdir_file(const char * fileName);

Key * create_root_key (const char *backendName);
KeySet *create_conf (const char *filename);

void clear_sync (KeySet *ks);
void output_keyset (KeySet *ks);
void output_key (Key *ks);

void output_plugin(Plugin *plugin);
void output_backend(Backend *backend);

void output_trie(Trie *trie);

void generate_split (Split *split);
void output_split(Split *split);

void output_warnings(Key *errorKey);
void output_errors(Key *errorKey);


#endif

