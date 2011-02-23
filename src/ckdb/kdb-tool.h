/***************************************************************************
                          kdb.h  -  Tool for the kdb administration
                             -------------------
    begin                : Fri Feb 22 2008
    copyright            : (C) 2008 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#ifndef HASGETOPT
#include "BSDgetopt.h"
#endif

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <time.h>
#include <locale.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_GRP_H
#include <grp.h>
#endif
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#include <ctype.h>
#include <ltdl.h>
#include <assert.h>
/* We need fcntl.h for open and related constants used in our definition of mkstemp */
#ifdef HAVE_WIN32
#include <fcntl.h>
#endif

#define DYN_LINK
#include <kdbtools.h>
#include <kdb.h>
#include <kdbloader.h>
#include <kdbextension.h>

#define CMD_GET       1
#define CMD_SET       2
#define CMD_REMOVE    3
#define CMD_LIST      4
#define CMD_LINK      5
#define CMD_EDIT      6
#define CMD_LOAD      7
#define CMD_SAVE      8
#define CMD_MONITOR   9
#define CMD_MOVE      10
#define CMD_INFO      25
#define CMD_HELP      30

#define ARGSIZE      30


#ifdef HAVE_WIN32
#define mkstemp(m) open(mktemp(m), O_RDWR)
#endif

/* Exported functions from help.c */
int commandHelp();
int helpCommand(int command);


