#ifndef TESTS_H
#define TESTS_H

#ifdef HAVE_CONFIG_H
#include "config.h"
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

#define DYN_LINK
#include <kdbtools.h>
#include <kdbloader.h>
#include <kdbbackend.h>

extern int nbStreaming;

KSFromXMLfile ksFromXMLfile;
KSFromXML ksFromXML;

output ksToStream;
output ksOutput;
output ksGenerate;

extern int nbError;
extern int nbTest;

extern uid_t nbUid;
extern gid_t nbGid;

int init();

#define warn_if_fail(x,y) {nbTest++; if (!(x)) { printf("%s:%d: warn in %s: %s\n", __FILE__, __LINE__, __FUNCTION__, y); }}
#define succeed_if(x,y) {nbTest++; if (!(x)) { nbError++; printf("%s:%d: error in %s: %s\n", __FILE__, __LINE__, __FUNCTION__, y); }}
#define exit_if_fail(x,y) {nbTest++; if (!(x)) { printf("%s:%d: fatal in %s: %s\n", __FILE__, __LINE__, __FUNCTION__, y); exit(1); }}

#endif
