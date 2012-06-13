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
#define NUM_DIR 100
#define NUM_KEY 100

#define NUM_THREAD 2
#define NR 50

#define BUF_SIZ 50

void timeInit (void);
void timePrint (char * msg);

#endif


