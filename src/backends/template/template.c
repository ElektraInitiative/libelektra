/***************************************************************************
            temaple.c  -  Skeleton of backends to access the Key Database
                             -------------------
    begin                : Mon Dec 26 2004
    copyright            : (C) 2004 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/



/***************************************************************************
 *                                                                         *
 *   This is the skeleton of the methods you'll have to implement in order *
 *   to provide libkdb.so a valid backend.                                 *
 *   Simple fill the empty _backend functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


/* Subversion stuff

$Id:$
$LastChangedBy: aviram $

*/



#include <kdb.h>
#include <kdbbackend.h>



/**Some systems have even longer pathnames*/
#ifdef PATH_MAX
#define MAX_PATH_LENGTH PATH_MAX
/**This value is garanteed on any Posixsystem*/
#elif __USE_POSIX
#define MAX_PATH_LENGTH _POSIX_PATH_MAX
#else 
#define MAX_PATH_LENGTH 4096
#endif




/**
 * @defgroup backend Elektra framework for pluggable backends
 * @brief The tactics to create pluggable backends to libkdb.so
 *
 * Since version 0.4.9, Elektra can dynamically load different key storage
 * backends.
 * 
 * The methods of class KeyDB that are backend dependent are kdbOpen(),
 * kdbClose(), kdbGetKey(), kdbSetKey(), kdbStatKey(),
 * kdbGetKeyChildKeys(), kdbRemove(), kdbRename(). So a backend must
 * reimplement these methods.
 * 
 * And methods that have a builtin default high-level inefficient
 * implementation are kdbSetKeys(), kdbMonitorKey(), kdbMonitorKeys(). So
 * it is suggested to reimplement them too, to make them more efficient.
 *
 * The other KeyDB methods are higher level. They use the above methods to
 * do their job, and generally don't have to be reimplemented for a
 * different backend.
 * 
 * The backend must implement a method with name kdbBackendFactory() and no
 * parameters, that is responsible of exporting the implementations of 
 * libkdb.so backend dependent methods.
 * 
 * Elektra source code or development package provides a skeleton and Makefile
 * to implement a backend, and we'll document this skeleton here.
 * 
 * A backend is defined by a single name, for example @c BACKENDNAME, that
 * causes libkdb.so look for its library as @c libkdb-BACKENDNAME.so.
 * 
 * So a KeyDB backend implementation will look like:
 * @code
#include <kdb.h>
#include <kdbbackend.h>

int kdbOpen_backend() {...}
int kdbClose_backend() {...}
int kdbGetKey_backend() {...}
int kdbSetKey_backend() {...}

... etc implementations of other methods ...


KDBBackend *kdbBackendFactory(void) {
	return kdbBackendExport(
		&kdbOpen_backend,
		&kdbClose_backend,
		&kdbGetKey_backend,
		&kdbSetKey_backend,
		&kdbStatKey_backend,
		&kdbRename_backend,
		&kdbRemove_backend,
		&kdbGetKeyChildKeys_backend,
		
		&kdbSetKeys_default,
		&kdbMonitorKey_backend,
		&kdbMonitorKeys_backend
	);
}

 * @endcode
 *
 * In the example, the *_backend() methods can have other random names,
 * since you'll correctly pass them later to kdbBackendExport().
 *
 */





/**
 * Initialize the backend.
 * This is the first method kdbOpenBackend() calls after dynamically loading
 * the backend library.
 *
 * @return 0 on success, anything else otherwise.
 * @see kdbOpenBackend()
 * @see kdbOpen()
 * @ingroup backend
 */
int kdbOpen_backend() {
	/* backend initialization logic */
	return 0;
}




/**
 * All finalization logic of the backend should go here.
 *
 * @return 0 on success, anything else otherwise.
 * @see kdbClose()
 * @ingroup backend
 */
int kdbClose_backend() {
	/* free all backend resources and shutdown */
	return 0; /* success */
}



/**
 * 
 * @see kdbStatKey() for expected behavior.
 * @ingroup backend
 */
int kdbStatKey_backend(Key *key) {
	/* get the most possible key metainfo */
	return 0; /* success */
}


/**
 *
 * @see kdbGetKey() for expected behavior.
 * @ingroup backend
 */
int kdbGetKey_backend(Key *key) {
	/* fully gets a key */
	return 0; /* success */
}



/**
 *
 * @see kdbSetKey() for expected behavior.
 * @ingroup backend
 */
int kdbSetKey_backend(Key *key) {
	/* fully sets a key */
	return 0; /* success */
}



/**
 *
 * @see kdbRename() for expected behavior.
 * @ingroup backend
 */
int kdbRename_backend(Key *key, const char *newName) {
	/* rename a key to another name */
	return 0; /* success */
}




/**
 *
 * @see kdbRemove() for expected behavior.
 * @ingroup backend
 */
int kdbRemove_backend(const char *keyName) {
	/* remove a key from the database */
	return 0;  /* success */
}




/**
 *
 * @see kdbGetKeyChildKeys() for expected behavior.
 * @ingroup backend
 */
int kdbGetKeyChildKeys_backend(const Key *parentKey, KeySet *returned, unsigned long options) {
	/* retrieve multiple hierarchical keys */
	return 0; /* success */
}


/**
 * The implementation of this method is optional.
 * The builtin inefficient implementation will use kdbSetKey() for each
 * key inside @p ks.
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
 * All KeyDB methods implemented by the backend can have random names, but
 * kdbBackendFactory() because this is the single symbol that will be looked
 * for when loading the backend, and the first method of the backend
 * implementation that will be called.
 * 
 * Its purpose is to "publish" the exported methods for libkdb.so. The
 * implementation inside the provided skeleton is usually enough, calling
 * kdbBackendExport() with all methods that must be exported.
 * 
 * @return whatever kdbBackendExport() returns
 * @see kdbOpenBackend()
 * @ingroup backend
 */
KDBBackend *kdbBackendFactory(void) {
	return kdbBackendExport(
		&kdbOpen_backend,
		&kdbClose_backend,
		&kdbGetKey_backend,
		&kdbSetKey_backend,
		&kdbStatKey_backend,
		&kdbRename_backend,
		&kdbRemove_backend,
		&kdbGetKeyChildKeys_backend,
		
		/* Next parameters can be NULL, then a default high-level
		 * inneficient implementation will be used */
		&kdbSetKeys_default,
		&kdbMonitorKey_backend,
		&kdbMonitorKeys_backend
	);
}
