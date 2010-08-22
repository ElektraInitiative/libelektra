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


#define TEST_OK 0
#define TEST_FAIL 1

#define BUF_SIZ 50


extern int nbTest;
extern int nbError;


#define succeed_if(x,y) nbTest++; if (!(x)) { nbError++; printf("%s:%d: error in %s: %s\n", __FILE__, __LINE__, __FUNCTION__, y);}
#define exit_if_fail(x,y) nbTest++; if (!(x)) { printf("%s:%d: fatal in %s: %s\n", __FILE__, __LINE__, __FUNCTION__, y); exit(1); }


void printKey (Key * k);
void printKeys (KeySet * set);

void init_time (void);
void print_time (char * msg);

void statistics (void);
void compare_keyset (KeySet *ks1, KeySet *ks2);
KeySet * create_keyset ();

int output_keyset (KeySet *ks, int filter);
void output_key (Key * k);

void * reader(void * pV_data);
void * writer (void * pV_data);
void * remover (void * pV_data);

#endif


