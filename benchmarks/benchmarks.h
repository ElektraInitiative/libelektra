/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef BENCHMARKS_H
#define BENCHMARKS_H

#include <kdb.h>
#include <kdbinternal.h>

#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#include <time.h>

#define KEY_ROOT "user/benchmark"

#define KEY_NAME_LENGTH 1000
#define NUM_DIR 200
#define NUM_KEY 200

#define NR 50

#define BUF_SIZ 50

void timeInit (void);
void timePrint (char * msg);

void benchmarkCreate();
void benchmarkFillup();

extern int num_dir; // default = NUM_DIR;
extern int num_key; // default = NUM_KEY;
extern KeySet *large;

#endif


