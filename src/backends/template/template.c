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
 *   it under the terms of the BSD License (revised).                      *
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

$Id$
$LastChangedBy$

*/



#include <kdb.h>
#include <kdbbackend.h>


#define BACKENDNAME "my_backend"



/**Some systems have even longer pathnames */
#ifdef PATH_MAX
#define MAX_PATH_LENGTH PATH_MAX
/**This value is garanteed on any Posix system */
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
 * backends. Fast jump to kdbBackendExport() to see an example of a backend
 * implementation.
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
 * The backend implementation must:
 * @code
#include <kdb.h>
#include <kdbbackend.h>
 * @endcode
 * 
 * <b>Better than that, a skeleton of a backend implementation is provided inside
 * Elektra development package or source code tree, and should be used as a
 * base for the implementation.</b>
 * 
 * An elektrified program will use the backend defined by environment variable
 * @e $KDB_BACKEND, The backend library is dynamically loaded when the program
 * calls kdbOpen(), unless if the program is security/authentication/setuid
 * related, in which it probably uses the more secure kdbOpenDefault() which
 * completely ignores the @e $KDB_BACKEND environment and will use the
 * @c "default" named backend defined by the sysadmin. Look at
 * @c /lib/libkdb-default.so link to see the default backend for your
 * system.
 * 
 * Elektra source code or development package provides a skeleton and Makefile
 * to implement a backend, and we'll document this skeleton here.
 * 
 * A backend is defined by a single name, for example @c BACKENDNAME, that
 * causes libkdb.so look for its library as @c libkdb-BACKENDNAME.so.
 * 
 * Elektra source code tree includes several backend implementations
 * (http://germane-software.com/repositories/elektra/trunk/src/backends)
 * that can also be used as a reference.
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
 * Called prior to unloading the backend dynamic module. Should ensure that no
 * functions or static/global variables from the module will ever be accessed again.
 * Should free any memory that the backend no longer needs.
 * After this call, libkdb.so will unload the backend library, so this is
 * the point to shutdown any affairs with the storage.
 *
 * @return 0 on success, anything else otherwise.
 * @see kdbClose()
 * @ingroup backend
 */
int kdbClose_backend() {
	/* free all backend resources and shut it down */
	return 0; /* success */
}



/**
 * Implementation for kdbStatKey() method.
 * 
 * @see kdbStatKey() for expected behavior.
 * @ingroup backend
 */
int kdbStatKey_backend(Key *key) {
	/* get the most possible key metainfo */
	return 0; /* success */
}


/**
 * Implementation for kdbGetKey() method.
 *
 * @see kdbGetKey() for expected behavior.
 * @ingroup backend
 */
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
		KDB_BE_OPEN,           &kdbOpen_backend,
		KDB_BE_CLOSE,          &kdbClose_backend,
		KDB_BE_GETKEY,         &kdbGetKey_backend,
		KDB_BE_SETKEY,         &kdbSetKey_backend,
		KDB_BE_STATKEY,        &kdbStatKey_backend,
		KDB_BE_RENAME,         &kdbRename_backend,
		KDB_BE_REMOVEKEY,      &kdbRemove_backend,
		KDB_BE_GETCHILD,       &kdbGetKeyChildKeys_backend,
		KDB_BE_MONITORKEY,     &kdbMonitorKey_backend,
		KDB_BE_MONITORKEYS,    &kdbMonitorKeys_backend,
		/* set to default implementation: */
		KDB_BE_SETKEYS,        &kdbSetKeys_default,
		KDB_BE_END);
}
