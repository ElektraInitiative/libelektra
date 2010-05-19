/***************************************************************************
                kdbbackend.h  -  Methods for backend programing
                             -------------------
    begin                : Mon Dec 25 2004
    copyright            : (C) 2004 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

/*You have to include this file in order to write backends or for internal
 *source files for elektra. You do not need this functions to use elektra!*/


#ifndef KDBBACKEND_H
#define KDBBACKEND_H


#include <kdb.h>
#include <kdbcap.h>
#include <kdbextension.h>

#ifdef ELEKTRA_STATIC
        #define KDBEXPORT(module) libelektra_##module##_LTX_kdbBackendFactory(void)	
#else
        #define KDBEXPORT(module) kdbBackendFactory(void)
#endif



/**
 * Switches to denote the backend methods. Used in calls to kdbBackendExport().
 *
 * @ingroup backend
 */
typedef enum {
	KDB_BE_OPEN=1,		/*!< Next arg is backend for kdbOpen() */
	KDB_BE_CLOSE=1<<1,	/*!< Next arg is backend for kdbClose() */
	KDB_BE_GET=1<<2,	/*!< Next arg is backend for kdbGet() */
	KDB_BE_SET=1<<3,	/*!< Next arg is backend for kdbSet() */
	KDB_BE_VERSION=1<<4,	/*!< Next arg is char * for Version */
	KDB_BE_DESCRIPTION=1<<5,/*!< Next arg is char * for Description */
	KDB_BE_AUTHOR=1<<6,	/*!< Next arg is char * for Author*/
	KDB_BE_LICENCE=1<<7,	/*!< Next arg is char * for Licence*/
	KDB_BE_END=0		/*!< End of arguments */
} backend_t;

#ifdef __cplusplus
namespace ckdb {
extern "C" {
#endif

KDB *kdbBackendExport(const char *backendName, ...);

/* Mounting API */
int kdbMount(KDB *handle, const Key *mountpoint, const KeySet *config);
int kdbUnmount(KDB *handle, const Key *mountpoint);
Key *kdbGetMountpoint(KDB *handle, const Key *where);

/* Idea for new api...
unsigned long kdbGetCapability(KDB *handle, const Key *where, unsigned long mask);
const char *kdbGetString(KDB *handle, const Key *where, unsigned long which);
*/

int keyClearSync (Key *key);

/* Some internal methods */
int kdbiRealloc (void ** buffer, size_t size);
void* kdbiMalloc (size_t size);
void* kdbiCalloc (size_t size);
void kdbiFree (void *ptr);
char *kdbiStrDup (const char *s);
size_t kdbiStrLen(const char *s);

ssize_t kdbbEncode(void *kdbbDecoded, size_t size, char *returned);
ssize_t kdbbDecode(char *kdbbEncoded,void *returned);

int kdbbNeedsUTF8Conversion(void);
int kdbbkdbbUTF8Engine(int direction, char **string, size_t *inputOutputByteSize);

int kdbbEncodeChar(char c, char *buffer, size_t bufSize);
int kdbbDecodeChar(const char *from, char *into);

int kdbbFilenameToKeyName(const char *string, char *buffer, int bufSize);
int kdbbKeyNameToRelativeFilename(const char *string, char *buffer, size_t bufSize);
ssize_t kdbbKeyCalcRelativeFilename(const Key *key,char *relativeFilename,size_t maxSize);

ssize_t kdbbGetFullKeyName (KDB *handle, const char *forFilename, const Key *parentKey, Key *returned);
ssize_t kdbbGetFullFilename(KDB *handle, const Key *forKey,char *returned,size_t maxSize);


/* Some handle manipulation methods */
void *kdbhGetBackendData(const KDB *handle);
void *kdbhSetBackendData(KDB *handle, void *data);

const Key *kdbhGetMountpoint(KDB *handle);
void kdbhSetMountpoint(KDB *handle, const Key* mountpoint);

KeySet *kdbhGetConfig(KDB *handle);


#ifdef __cplusplus
}
}
#endif


#endif /* KDBBACKEND_H */

