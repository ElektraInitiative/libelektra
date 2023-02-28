/******************************************************************************
 * Nickel - a library for hierarchical maps and .ini files
 * One of the Bohr Game Libraries (see chaoslizard.org/devel/bohr)
 * Copyright (C) 2008 Charles Lindsay.  Some rights reserved; see COPYING.
 * $Id: internal.h 331 2008-01-13 18:28:41Z chaz $
 ******************************************************************************/


#ifndef __internal_h__
#define __internal_h__

#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#define Ds_VECTOR_BEHAVIOR 2
#define Ds_VECTOR_TYPE unsigned char
#define Ds_VECTOR_SUFFIX _uc
#include <bohr/ds_str.h>
#include <bohr/ds_vector.h>

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>


// Controls export behavior.
#ifdef _WIN32
#define elektraNi_PUBLIC __declspec (dllexport)
#define elektraNi_PRIVATE
#else
#define elektraNi_PUBLIC __attribute__ ((visibility ("default")))
#define elektraNi_PRIVATE __attribute__ ((visibility ("hidden")))
#endif

// Nix non-critical C99 keywords in compilers that don't support them.
#if ((!defined(__STDC_VERSION__) || __STDC_VERSION__ < 199901L) && !defined(restrict))
#define restrict
#endif


// Internally, it's a pointer to our node struct.
typedef struct elektraNi_node_struct * elektraNi_node;
#define _elektraNi_NODE_DEFINED


// A wrapper around FILE.
typedef struct file_buf
{
	FILE * stream;	     // the FILE
	Ds_vector_uc buffer; // the buffer
	size_t pos;	     // current position for reading

} file_buf;
#define FILE_BUF_INIT                                                                                                                      \
	{                                                                                                                                  \
		NULL, Ds_VECTOR_INIT, 0                                                                                                    \
	} // initializer


// The following functions are in buf.c:
int InitFileBuf (file_buf * restrict b, FILE * restrict f);
void FreeFileBuf (file_buf * restrict b);
int BufGetC (file_buf * restrict b);
void BufSeekBack (file_buf * restrict b, size_t n_back);
void BufFlush (file_buf * restrict b);

// The following functions are in io.c:
int GetNextIdentifier (file_buf * restrict fb, char * restrict idfr_out, int * restrict len_out, int * restrict level_out);
int GetValue (file_buf * restrict fb, Ds_str * restrict value_out);
int PutSection (FILE * restrict f, const char * restrict name, int name_len, int level);
int PutEntry (FILE * restrict f, const char * restrict key, int key_len, const char * restrict value, int value_len, int level);

// In hash.c:
uint32_t Hash (const void * restrict key, size_t length, uint32_t initval);


#endif
