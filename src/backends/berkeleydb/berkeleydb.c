/***************************************************************************
            berkeleydb.c  -  A Berkeley DB backend for Elektra
                             -------------------
    begin                : Mon Jan 24 2005
    copyright            : (C) 2005 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/




/* Subversion stuff

$Id$
$LastChangedBy$

*/



#include <kdb.h>
#include <kdbbackend.h>
#include <db.h>


#define BACKENDNAME "berkeleydb"



/**Some systems have even longer pathnames */
#ifdef PATH_MAX
#define MAX_PATH_LENGTH PATH_MAX
/**This value is garanteed on any Posix system */
#elif __USE_POSIX
#define MAX_PATH_LENGTH _POSIX_PATH_MAX
#else 
#define MAX_PATH_LENGTH 4096
#endif






int KeyToBDB(Key *key, DBT *dbkey, DBT *dbdata) {
	void *serialized;
	size_t metaInfoSize;

	memset(dbkey, 0, sizeof(DBT));
	memset(dbdata, 0, sizeof(DBT));

	metaInfoSize=
		  sizeof(key->type)
		+ sizeof(key->uid)
		+ sizeof(key->gid)
		+ sizeof(key->access)
		+ sizeof(key->atime)
		+ sizeof(key->mtime)
		+ sizeof(key->ctime)
		+ sizeof(key->commentSize)
		+ sizeof(key->dataSize);

	
	dbdata->size = metaInfoSize + key->dataSize + key->commentSize;
	serialized = malloc(dbdata->size);

	/* First part: the metainfo */
	memcpy(serialized,key,metaInfoSize);

	/* Second part: the comment */
	memcpy(serialized+metaInfoSize,key->comment,key->commentSize);

	/* Third part: the value */
	memcpy(serialized+metaInfoSize+key->commentSize,key->data,key->dataSize);
	
	dbdata->data=serialized;

	dbkey->size=strblen(key->key);
	dbkey->data=malloc(dbkey->size);
	strcpy(dbkey->data,key->key);
	
	return 0;
}






int BDBToKey(Key *key, DBT *dbkey, DBT *dbdata) {
	size_t metaInfoSize;
	
	metaInfoSize=
		  sizeof(key->type)
		+ sizeof(key->uid)
		+ sizeof(key->gid)
		+ sizeof(key->access)
		+ sizeof(key->atime)
		+ sizeof(key->mtime)
		+ sizeof(key->ctime)
		+ sizeof(key->commentSize)
		+ sizeof(key->dataSize);
	
	keyClose(key);
	memset(key,0,sizeof(key));
	
	/* Set all metainfo */
	memcpy(key,          /* destination */
		dbdata->data,    /* source */
		metaInfoSize);   /* size */

	/* Set key name */
	keySetName(key,dbkey->data);
	
	/* Set comment */
	if (key->commentSize)
		keySetComment(key,dbdata->data+metaInfoSize);
	
	/* Set value */
	keySetRaw(key,dbdata->data+metaInfoSize+key->commentSize,key->dataSize);
	
	/* userDomain must be set outside this function,
	 * someplace more aware of the context */
	
	return 0;
}






int kdbOpen_bdb() {
	/* backend initialization logic */
	return 0;
}




int kdbClose_bdb() {
	/* free all backend resources and shut it down */
	return 0; /* success */
}




int kdbStatKey_bdb(Key *key) {
	/* get the most possible key metainfo */
	return 0; /* success */
}




int kdbGetKey_backend(Key *key) {
	/* fully gets a key */
	return 0; /* success */
}



/**
 * Implementation for kdbSetKey() method.
 *
 * @see kdbSetKey() for expected behavior.
 * @ingroup backend
 */
int kdbSetKey_backend(Key *key) {
	/* fully sets a key */
	return 0; /* success */
}



/**
 * Implementation for kdbRename() method.
 *
 * @see kdbRename() for expected behavior.
 * @ingroup backend
 */
int kdbRename_backend(Key *key, const char *newName) {
	/* rename a key to another name */
	return 0; /* success */
}




/**
 * Implementation for kdbRemoveKey() method.
 *
 * @see kdbRemove() for expected behavior.
 * @ingroup backend
 */
int kdbRemoveKey_backend(const Key *key) {
	/* remove a key from the database */
	return 0;  /* success */
}




/**
 * Implementation for kdbGetKeyChildKeys() method.
 *
 * @see kdbGetKeyChildKeys() for expected behavior.
 * @ingroup backend
 */
int kdbGetKeyChildKeys_backend(const Key *parentKey, KeySet *returned, unsigned long options) {
	/* retrieve multiple hierarchical keys */
	return 0; /* success */
}


/**
 * Implementation for kdbSetKeys() method.
 * 
 * The implementation of this method is optional, and a builtin, probablly 
 * inefficient implementation can be explicitly used when exporting the
 * backend with kdbBackendExport(), using kdbSetKeys_default().
 * 
 * @see kdbSetKeys() for expected behavior.
 * @ingroup backend
 */
int kdbSetKeys_backend(KeySet *ks) {
	/* set many keys */
	return 0;
}


/**
 * The implementation of this method is optional.
 * The builtin inefficient implementation will use kdbGetKey() for each
 * key inside @p interests.
 *
 * @see kdbMonitorKeys() for expected behavior.
 * @ingroup backend
 */
u_int32_t kdbMonitorKeys_backend(KeySet *interests, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep) {
}



/**
 *
 * The implementation of this method is optional.
 * The builtin inefficient implementation will use kdbGetKey() for
 * @p interest.
 *
 * @see kdbMonitorKey() for expected behavior.
 * @ingroup backend
 */
u_int32_t kdbMonitorKey_backend(Key *interest, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep) {
}


/**
 * All KeyDB methods implemented by the backend can have random names, except
 * kdbBackendFactory(). This is the single symbol that will be looked up
 * when loading the backend, and the first method of the backend
 * implementation that will be called.
 * 
 * Its purpose is to "publish" the exported methods for libkdb.so. The
 * implementation inside the provided skeleton is usually enough: simply
 * call kdbBackendExport() with all methods that must be exported.
 * 
 * @return whatever kdbBackendExport() returns
 * @see kdbBackendExport() for an example
 * @see kdbOpenBackend()
 * @ingroup backend
 */
KDBBackend *kdbBackendFactory(void) {
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,           &kdbOpen_bdb,
		KDB_BE_CLOSE,          &kdbClose_bdb,
		KDB_BE_GETKEY,         &kdbGetKey_backend,
		KDB_BE_SETKEY,         &kdbSetKey_backend,
		KDB_BE_STATKEY,        &kdbStatKey_bdb,
		KDB_BE_RENAME,         &kdbRename_backend,
		KDB_BE_REMOVEKEY,      &kdbRemoveKey_backend,
		KDB_BE_GETCHILD,       &kdbGetKeyChildKeys_backend,
		KDB_BE_MONITORKEY,     &kdbMonitorKey_backend,
		KDB_BE_MONITORKEYS,    &kdbMonitorKeys_backend,
		/* set to default implementation: */
		KDB_BE_SETKEYS,        &kdbSetKeys_default,
		KDB_BE_END);
}
