/***************************************************************************
            ini.h  -  Backend for ini-style like files
                             -------------------
    begin                : 01.03.2005
    updated              : 06.10.2005
    copyright            : (C) 2005 by Markus Raab
    email                : debian@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/



/***************************************************************************
 *
 *   This is a ini-style backend.
 *   Key/Value Pairs are stored in files in following scheme:
 *   
 *   key1=value1;comment
 *
 * TODO:
 *   allow subkeys setting/getting
 *   setting existing keys again
 *   setting errno properly (update doc, how it should be)
 *   KDB_ERR_NOMEM bei malloc errors
 *
 ***************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <kdbbackend.h>

#define BACKENDNAME "ini"
#define BACKENDVERSION "0.1.2"

struct _backendData
{
	int fd;
	FILE *fc;
};

typedef struct _backendData backendData;

#define FILEDES ((((backendData*)kdbhGetBackendData (handle))->fd))
#define FILEPTR ((((backendData*)kdbhGetBackendData (handle))->fc))

int IniReadDir(KDB *handle, Key * key, KeySet * returned, unsigned long options);
int IniChooseFile(KDB *handle, Key * key, KeySet * returned, unsigned long options);
int IniReadFile (KDB *handle, Key * key, KeySet * returned, unsigned long options);
int IniSetKeys (KDB *handle, KeySet * origKeys);

/**Helper functions*/
int open_file (KDB *handle, char * filename, char mode);
int close_file (KDB *handle);
int enlarge_file (KDB *handle, long where, long space);
int shrink_file (KDB *handle, long where, long space);
int read_key (KDB *handle, Key * key, char * root);
int write_key (KDB *handle, Key * key, long oldpos);
int remove_key (KDB *handle, Key * key, long oldpos);

int elektraRealloc (void ** buffer, size_t size);

int stat_file (Key * forKey, char * filename);
size_t base_name (const Key * forKey, char * basename);
size_t file_name (const Key * forKey, char * basename);

void * open_dir (char * pathname);
int create_dir (char * filename);
int read_dir (void * dir, char * filename);
int close_dir (void * dir);

/**Parsing functions*/
int parse_buffer (char * c);
int convert_engine (char * c);
int convert_strlen (char * p, int size);
int convert_stream (char * buffer, int size, FILE * stream);
int make_key (Key * key, char * root, char * buffer_key, char * buffer_value, char * buffer_comment);

/**Some systems have even longer pathnames */
#ifdef PATH_MAX
#define MAX_PATH_LENGTH PATH_MAX
/**This value is garanteed on any Posix system */
#elif _POSIX_PATH_MAX
#define MAX_PATH_LENGTH _POSIX_PATH_MAX
#else 
/**Fallback: This value should be useful*/
#define MAX_PATH_LENGTH 4096
#endif


/**This buffer size is fastest for reading and writing
 * in files*/
#define BUFFER_RDWR_SIZE 8024

/**Buffer for holding strings*/
#ifndef BUFFER_SIZE
#define BUFFER_SIZE 4048
#endif

/**Some more key types needed for ini
 * FILE ... is a real file on the system
 * DIR ... is a real directory
 * SUBDIR ... is a subdirectoy within a file*/
#define KEY_TYPE_FILE 4
#define KEY_TYPE_DIR 8
#define KEY_TYPE_SUBDIR 16

#define O_RDONLY 'r'
#define O_RDWR 'w'

