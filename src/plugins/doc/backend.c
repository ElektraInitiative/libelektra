/***************************************************************************
            backend.c  -  Skeleton of backends to access the Key Database
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


#include <kdbbackend.h>


#define BACKENDNAME "backend"
#define BACKENDVERSION "1.0.0"


/**
 * @defgroup backend KDB Backends :: Elektra framework for pluggable backends
 * @brief The tactics to create pluggable backends to libelektra.so
 *
 * @section intro Introduction
 *
 * @since Since version 0.4.9, Elektra can dynamically load different key storage
 * backends.
 *
 * @since Since version 0.7.0 Elektra can have multiple storage backends, called just
 * backends henceforth, at once for different purposes.
 *
 * @par Definition: You refers to the implementation of the function in this specification.
 * If you read the documentation about kdbGet_backend(), then the caller is kdbGet()
 * which is the only function which can and will call (invoke) you. The Preconditions
 * will always be met by the caller, you can count on them. But you (as said before we
 * speak about the function) need to take care that all Postconditions are met.
 *
 * @subsection overview Overview
 *
 * The methods of class KDB that are backend dependent are only kdbOpen_backend(),
 * kdbClose_backend(), kdbGet_backend(), kdbSet_backend() and KDBEXPORT() to export
 * these methods. A backend must implement each of them. A detailed
 * specification of these methods and methods needed in that context
 * follows in this Documentation Module.
 *
 * The other KDB methods are higher level. They use the above methods to
 * do their job, and generally don't have to be reimplemented for a
 * different backend, but there might be a solution to do so for higher
 * performance in future. kdbh* methods are for access to the internals
 * of KDB, which will be passed to all functions.
 *
 * @subsection incl Include Files
 *
 * The backend implementation must include:
 * @code
#include <kdbbackend.h>
 * @endcode
 * to have direct access to the structs, which is currently needed to
 * access the capability structure.
 *
 * Don't include kdb.h, it will be automatically included and some macros will
 * avoid redefining structs where you have more insight from a backend than
 * you would normally have. Additionally you get the declaration of all functions
 * described here, except the one you have to implement.
 *
 * @subsection dyn Dynamic Mounting
 *
 * An elektrified program will use elektra/libelektra-default.so as its default backend.
 * This backend provides the system/ hierarchy and some base configuration
 * in system/elektra for elektra itself. Everything below system/ and the other
 * hierarchies can be stored in any different backend. This is allowed through the
 * technique mounting. A backend can be mounted to any path except system/
 * and system/elektra.
 *
 * A backends is guaranteed to be loaded whenever calling kdbGet()
 * or kdbSet() requires the backend, but may already be loaded at kdbOpen(). It
 * might be loaded explizit by kdbMount() at any time after kdbOpen().
 * Backends get a chance to initialize by calling kdbOpen_backend() whenever
 * they are loaded.
 *
 * Using kdbUnmount() a backend may closed during runtime.
 * All backends will be closed when kdbClose() is called.
 * Backends might be unloaded after some time of inactivity or other reasons.
 * After loading backends get a chance to cleanup by calling
 * kdbClose_backend().
 *
 * That means it is not guaranteed that the backend live the whole time nor
 * it will be loaded only one time. A tactic to handle this well is to build
 * stateless backends referring to kdbGet_backend() and kdbSet_backend().
 * That means that there is no more information present than in the storage itself.
 * Be aware that you must not have any global variables in your backend. Read more
 * about that in kdbOpen_backend().
 * But to be stateless you also have to consider not to store any other than caching
 * information into kdbhGetBackendData(). I repeat: it must be possible to restore
 * everything dynamically stored without exception.
 *
 * @subsection lib Library Names
 *
 * Elektra source code or development package provides a skeleton and Makefile
 * to implement a backend. Copy src/backends/template to have a good
 * starting point. See the CODING document
 * to know how to integrate the backend in the build system or how to compile
 * it external.
 *
 * A backend is defined by a single name, for example @c BACKENDNAME, that
 * causes libelektra.so look for its library as @c libelektra-BACKENDNAME.so.
 *
 * @par Example of a complete backend:
 * @code
//
// This is my implementation for an Elektra backend storage.
//
// To compile it:
// $ cc -fpic `pkg-config --cflags elektra` -o myback.o -c myback.c
// $ cc -shared -fpic `pkg-config --libs elektra` -o libelektra-myback.so myback.o
//
// To use it:
// $ preload mount myback system/myback myback /tmp/nofile
// $ kdb ls system/myback
// $ kdb set system/myback/key "value"
// $ kdb get system/myback/key
//

#include <kdbbackend.h>

#define BACKENDNAME "backend"


int kdbOpen_backend(KDB *handle) {...}
int kdbClose_backend(KDB *handle) {...}
int kdbGet_backend(KDB handle, KeySet *returned, Key *key) {...}
int kdbSet_backend(KDB handle, KeySet *returned, Key *key) {...}

KDBEXPORT(backend) {
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,  &kdbOpen_backend,
		KDB_BE_CLOSE, &kdbClose_backend,
		KDB_BE_GET,   &kdbGet_backend,
		KDB_BE_SET,   &kdbSet_backend,
		KDB_BE_END);
}
 * @endcode
 *
 * In the example, the *_backend() methods can have other random names,
 * since you'll correctly pass them later to kdbBackendExport(). It is
 * recommended to use names according to your backendname to avoid
 * name clashes. Be aware that every symbol name in the linked application
 * must be unique.
 *
 * Don't copy above example out, use src/backends/template, it does
 * compile as-is and does some initialization and cleanup already.
 *
 * Elektra source code tree includes several backend implementations
 * https://svn.libelektra.org/svn/elektra/trunk/src/backends/
 * that can also be used as a reference.
 *
 * @section backenddetail Details
 *
 * @subsection intro Introduction
 *
 * Capabilities may make your live much easier. If it is impossible,
 * very hard or would impact performance badly you may leave out some
 * parts described here, but need to declare that you have done so with
 * capabilites.
 *
 * It is allowed to provide additional information, even if you
 * declared you don't have it. If you declare that you are capable
 * of doing something, you must provide it without exceptions.
 *
 * @subsection owner Owner
 *
 * You need to set the owner of keys by keySetOwner(). Owner is the
 * name to whom a specific key of the user/ hierarchy belongs.
 * If you declare kdbcGetnoOwner() you need not to set the owner of
 * the keys. It also means that even if you want to get keys from another
 * user hierarchy you get yours.
 *
 * @subsection value Values
 *
 * Values are the central information of keys next to the name
 * describing what informations it holds. Parse them out of your backend
 * and put them into the key with keySetString(). The information will
 * be duplicated, so you might need to free() your string. Don't try
 * to directly access key->data, things may change there and your
 * backend might be compiled with a different libc than elektra.
 * If you support types, you might want to use keySetRaw() to not
 * change the key type.
 * If you don't support values for all keys declare kdbcGetnoValue().
 *
 * @subsection id IDs
 *
 * You need to set uid respective gid for any key not having the uid
 * and gid of the current process. This will be set by default in
 * every key. You can do it with keySetUID() and keySetGID().
 * Declaring kdbcGetnoUID() and kdbcGetnoGID() you need not set uid
 * and gid.
 *
 * @subsection mode Mode
 *
 * Mode shows what can be done with the key having or not having
 * the above uid and gid. Use keySetMode() to set the correct
 * mode description, read the description in keySetMode()
 * for the semantics of the 3 octal representation.
 * Declaring kdbcGetnoMode() means mode will remain default.
 *
 * The very related method keySetDir() sets the executable bits
 * of mode. Even if your backend does not support mode, it
 * might support directories, meaning that keys have the mode
 * 0664 or 0775 for directories. Declaring kdbcGetnoDir() means
 * that the backend is flat, no key will be true for keyIsDir()
 * and so can't have any subkeys.
 *
 * @subsection timing Timing
 *
 * Keys should have exact timing information of their
 * modification and access times. Use keySetATime(), keySetMTime()
 * and keySetCTime() to store appropriate information.
 * ATime need to be stored in database, if you stat a key
 * the backend need to return the time kdbGet() was last used
 * for the keys.
 * If you don't support this, declare
 * kdbcGetnoATime() and simple store time(0) in the atime.
 * This must be the same for every key for a single
 * kdbGet_backend().
 * If you only stat keys with kdbGet(), see below, then the access time
 * should not be updated.
 * MTime is the last modification time of value or comment.
 * If you don't support this, declare
 * kdbcGetnoMTime() and simple store time(0) in the mtime.
 * This must be the same for every key for a single
 * kdbGet_backend().
 * CTime is the last change time of any metadata or
 * add/remove of subkeys.
 * If you don't support this, declare
 * kdbcGetnoCTime() and simple store time(0) in the ctime.
 * This must be the same for every key for a single
 * kdbGet_backend().
 *
 * @subsection type Types
 *
 * Keys having value and comment can be one of two fundamental
 * types, string or binary, both called value. While string is
 * a null terminated utf8 character sequence, binary is any data
 * of a specific length. Be sure to use keySetString() for string
 * and keySetBinary() if you want to store binary data.
 * If you do not support one of these, be sure to declare
 * kdbcGetnoBinary() or kdbcGetnoString(), if you don't support
 * both make sure to also declare kdbcGetnoValue().
 *
 * Using keySetRaw() does not set the type, be sure to set the
 * meta data binary using keySetMeta(key, "binary", "")
 * afterwards. Declare kdbcGetnoTypes() when your
 * backend does not support arbitrary types.
 *
 *
 */





/**
 * Initialize the backend.
 * This is the first method kdbOpenPlugin() calls after dynamically loading
 * the backend library.
 *
 * This method is responsible of:
 * - backend's specific configuration gathering
 * - all backend's internal structs initialization
 * - initial setup of all I/O details such as opening a file, connecting to a
 *   database, etc
 *
 * If your backend does not support all aspects described in kdbGet_backend()
 * and kdbSet_backend() you need capabilities to export this information.
 * Per default you declare to be fully compliant to the specification given
 * here, to change it get a pointer to KDBCap structure by using
 * kdbhGetCapability().
 *
 * You may also read the configuration you can get with kdbhGetConfig() and transform it
 * into other structures used by your backend.
 *
 * But be aware that you don't have any global variables. If you do your backend will
 * not be threadsafe. You can use kdbhSetBackendData() and kdbhGetBackendData() to store
 * and get any information related to your backend.
 *
 * The correct substitute for global variables will be:
 * @code
struct _GlobalData{ int global; };
typedef struct _GlobalData GlobalData;
int kdbOpen_backend(KDB *handle) {
	PasswdData *data;
	data=malloc(sizeof(PasswdData));
	data.global = 20;
	kdbhSetBackendData(handle,data);
}
 * @endcode
 *
 * Make sure to free everything in kdbClose_backend().
 *
 * @return 0 on success
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @see kdbOpen()
 * @ingroup backend
 */
int kdbOpen_backend(KDB *handle) {
	return 0;
}




/**
 * Finalize the backend.
 * Called prior to unloading the backend dynamic module. Should ensure that no
 * functions or static/global variables from the module will ever be accessed again.
 *
 * Make sure to free all memory that your backend requested at runtime.
 *
 * Specifically make sure to capDel() all capabilites and free your backendData in
 * kdbhGetBackendData().
 *
 * After this call, libelektra.so will unload the backend library, so this is
 * the point to shutdown any affairs with the storage.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @return 0 on success, anything else otherwise.
 * @see kdbClose()
 * @ingroup backend
 */
int kdbClose_backend(KDB *handle) {
	return 0; /* success */
}



/**
 * Retrieve information from a permanent storage to construct
 * a keyset.
 *
 * @section intro Introduction
 *
 * This function does everything related to get keys out from a
 * backend. There is only one function for that purpose to make
 * implementation and locking much easier.
 *
 * The keyset @p returned needs to be filled with information
 * so that the application using elektra can access it.
 * See the live cycle of a comment to understand:
 * @code
kdbGet_backend(KDB *handle, KeySet *returned, Key *parentKey)
{
	// the task of kdbGet_backend is to retrieve the comment out of the permanent storage
	Key *key = keyDup (parentKey); // generate a new key to hold the information
	char *comment;
	loadfromdisc (comment);
	keySetComment (key, comment, size); // set the information
	ksAppendKey(returned, key);
}

// Now return to kdbGet
int kdbGet(KDB *handle, KeySet *keyset, Key *parentKey, options)
{
	kdbGet_backend (handle, keyset, 0);
	// postprocess the keyset and return it
}

// Now return to usercode, waiting for the comment
void usercode (Key *key)
{
	kdbGet (handle, keyset, parentKey, 0);
	key = ksCurrent (keyset, key); // lookup the key from the keyset
	keyGetComment (key); // now the usercode retrieves the comment
}

 * @endcode
 * Of course not only the comment, but all information of every key in the keyset
 * @p returned need to be fetched from permanent storage and stored in the key.
 * So this specification needs to give
 * an exhaustive list of information present in a key.
 *
 * @section conditions Conditions
 *
 * @pre The caller kdbGet() will make sure before you are called
 * that the parentKey:
 * - is a valid key (means that it is a system or user key).
 * - is below (see keyIsBelow()) your mountpoint and that your backend is responsible for it.
 * and that the returned:
 * - is a valid keyset.
 * - has @p all keys with the flag KEY_FLAG_SYNC set.
 * - contains only valid keys direct below (see keyIsDirectBelow()) your parentKey.
 *   That also means, that the parentKey will not be in that keyset.
 * - is in a sorted order, see ksSort().
 * and that the handle:
 *  - is a valid KDB for your backend.
 *  - that kdbhGetBackendHandle() contains the same handle for lifetime kdbOpen_backend()
 *    until kdbClose_backend() was called.
 *
 * @pre The caller kdbGet() will make sure that afterwards you were called,
 * whenever the user requested it with the options, that:
 * - hidden keys they will be thrown away.
 * - dirs or only dirs kdbGet() will remove
 *   the other.
 * - you will be called again recursively with all subdirectories.
 * - the keyset will be sorted when needed.
 * - the keys in returned having KEY_FLAG_SYNC will be sorted out.
 *
 * @invariant There are no global variables and kdbhGetBackendData()
 *  only stores information which can be regenerated any time.
 *  The handle is the same when it is the same backend.
 *
 * @post The keyset @p returned has the @p parentKey and all keys direct
 * below (keyIsDirectBelow()) with all information from the storage.
 * Make sure to return all keys, all directories and also all
 * hidden keys. If some of them are not wished, the caller kdbGet() will
 * drop these keys, see above.
 *
 * @section detail Details
 *
 * Now lets look at an example how the typical kdbGet_backend() might be
 * implemented. To explain we introduce some pseudo functions which do all
 * the work with the storage (which is of course 90% of the work for a real
 * backend):
 * - find_key() gets an key out from the storage and memorize the position.
 * - next_key() will find the next key and return it (with the name).
 * - fetch_key() gets out all information of a key from storage
 *    (details see below example).
 * - stat_key() gets all meta information (everything but value and comment).
 *   It removes the key keyNeedSync() flag afterwards.
 * returns the next key out from the storage.
 * The typical loop now will be like:
 * @code
ssize_t kdbGet_backend(KDB *handle, KeySet *update, const Key *parentKey) {
	Key * current;
	KeySet *returned = ksNew(ksGetSize(update)*2, KS_END);

	find_key (parentKey);
	current = keyDup (parentKey);
	current = fetch_key(current);

	keyClearSync (current);
	ksAppendKey(returned, current);

	while ((current = next_key()) != 0)
	{
		// search if key was passed in update by caller
		Key * tmp = ksLookup (update, current, KDB_O_WITHOWNER|KDB_O_POP);
		if (tmp) current = tmp; // key was passed, so use it
		current = fetch_key(current);
		keyClearSync (current);
		ksAppendKey(returned, current);
		// TODO: delete lookup key
	}

	if (error_happened())
	{
		errno = restore_errno();
		return -1;
	}

	ksClear (update); // the rest of update keys is not in storage anymore
	ksAppend(update, returned); // append the keys
	ksDel (returned);

	return nr_keys();
}
 * @endcode
 *
 * @note - returned and update are separated, for details why see ksLookup()
 * - the bit KEY_FLAG_SYNC is always cleared, see postconditions
 *
 * So your mission is simple: Search the @c parentKey and add it and then search
 * all keys below and add them too, of course with all the values.
 *
 * @section updating Updating
 *
 * To get all keys out of the storage over and over again can be very inefficient.
 * You might know a more efficient method to know if the key needs update or not,
 * e.g. by stating it or by an external time stamp info. In that case you can make
 * use of @p returned KeySet. There are following possibilities:
 * - The key is in returned and up to date.
 *   You just need to remove the KEY_FLAG_SYNC flag.
 * - The key is not in returned.
 *   You need to fully retrieve the key out of storage, clear
 *   KEY_FLAG_SYNC using keyClearSync() and ksAppendKey() it to the @p returned keyset.
 *
 * @note You must clear the flag KEY_FLAG_SYNC at the very last point where no more
 * modification on the key will take place, because any modification on the key will
 * set the KEY_FLAG_SYNC flag again. With that keyNeedSync() will return true and
 * the caller will sort this key out.
 *
 * @section fullget only Full Get
 *
 * In some backends it is not useful to get only a part of the configuration, because
 * getting all keys would take as long as getting some. For this situation,
 * you can declare onlyFullGet, see kdbcGetonlyFullGet().
 *
 * The only valid call for your backend is then that @p parentKey equals the @p mountpoint.
 * For all other @p parentKey you must, add nothing and just return 0.
 *
 * @code
if (strcmp (keyName(kdbhGetMountpoint(handle)), keyName(parentKey))) return 0;
 * @endcode
 *
 * If the @p parentKey is your mountpoint you will of course fetch all keys, and not only
 * the keys direct below the @c parentKey.
 * So @p returned is valid iff:
 * - every key is below ( keyIsBelow()) the parentKey
 * - every key has a direct parent (keyIsDirectBelow()) in the keyset
 *
 * @note This statement is only valid for backends with kdbcGetonlyFullGet() set.
 *
 * @note If any calls you use change errno, make sure to restore the old errno.
 *
 * @see kdbGet() for caller.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param returned contains a keyset where the function need to
 * append the keys got from the storage. There might be also some
 * keys inside it, see conditions. You may use them to support
 * efficient updating of keys, see @ref updating.
 * @param parentKey contains the information below which key
 * the keys should be gotten.
 *
 * @return Return how many keys you added.
 *
 * @return -1 on failure, the current key in returned shows the position.
 * In normal execution cases a positive value will be returned.
 * But in some cases you are not able to get keys and have to
 * return -1. If you declare kdbcGetnoError() you are done, but
 * otherwise you have to set the cause of the error.
 * (Will be added in 0.7.1)
 *
 * @ingroup backend
 */
ssize_t kdbGet_backend(KDB *handle, KeySet *returned, const Key *parentKey) {
	return 0;
}


/**
 * Store a keyset permanently.
 *
 * This function does everything related to set and remove keys in a
 * backend. There is only one function for that purpose to make
 * implementation and locking much easier.
 *
 * The keyset @p returned was filled in with information from the application
 * using elektra and the task of this function is to store it in a permanent
 * way so that a subsequent call of kdbGet_backend() can rebuild the keyset
 * as it was before. See the live cycle of a comment to understand:
 * @code
void usercode (Key *key)
{
	keySetComment (key, "mycomment"); // the usercode stores a comment for the key
	ksAppendKey(keyset, key); // append the key to the keyset
	kdbSet (handle, keyset, 0, 0);
}

// so now kdbSet is called
int kdbSet(KDB *handle, KeySet *keyset, Key *parentKey, options)
{
	// find appropriate backend
	kdbSet_backend (handle, keyset, 0); // the keyset with the key will be passed to this function
}

// so now kdbSet_backend(), which is the function described here, is called
kdbSet_backend(KDB *handle, KeySet *keyset, Key *parentKey)
{
	// the task of kdbSet_backend is now to store the comment
	Key *key = ksCurrent (keyset); // get out the key where the user set the comment before
	char *comment = allocate(size);
	keyGetComment (key, comment, size);
	savetodisc (comment);
}
 * @endcode
 * Of course not only the comment, but all information of every key in the keyset
 * @p returned need to be stored permanetly. So this specification needs to give
 * an exhaustive list of information present in a key.
 *
 * @pre The keyset @p returned holds all keys which must be saved
 * permanently for this keyset. The keyset is sorted and rewinded.
 * All keys having children must be true for keyIsDir().
 *
 * @pre The @p parentKey is the key which is the ancestor for all other keys in the
 * keyset. The first key of the keyset @p returned has the same keyname.
 * The parentKey is below the mountpoint, see kdbhGetMountpoint().
 *
 * @pre The caller kdbSet will fulfill following parts:
 * - If the user does not want hidden keys they will be thrown away.
 *   All keys in @p returned need to be stored permanently.
 * - If the user does not want dirs or only dirs kdbGet() will remove
 *   the other.
 * - Sorting of the keyset. It is not important in which order the keys
 *   are appended.
 * So make sure to set all keys, all directories and also all
 * hidden keys. If some of them are not wished, the caller kdbSet() will
 * sort them out.
 *
 * @invariant There are no global variables and kdbhGetBackendData()
 *  only stores information which can be regenerated any time.
 *  The handle is the same when it is the same backend.
 *
 * @post The information of the keyset @p returned is stored permanently.
 *
 * Lock your permanent storage in an exclusive way, no access of a
 * concurrent kdbSet_backend() or kdbGet_backend() is possible
 * and these methods block until the function has finished.
 * Otherwise declare kdbcGetnoLock().
 *
 * @see kdbSet() for caller.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param returned contains a keyset with relevant keys
 * @param parentKey contains the information where to set the keys
 *
 * @return When everything works gracefully return the number of keys you set.
 * The cursor position and the keys remaining in the keyset are not important.
 *
 * @return Return 0 on success with no changed key in database
 *
 * @return Return -1 on failure.
 *
 * @note If any calls you use change errno, make sure to restore the old errno.
 *
 * @err In normal execution cases a positive value will be returned.
 * But in some cases you are not able to set keys and have to
 * return -1. If you declare kdbcGetnoError() you are done, but
 * otherwise you have to set the cause of the error.
 * (Will be added with 0.7.1)
 *
 * You also have to make sure that ksGetCursor()
 * shows to the position where the error appeared.
 *
 * @ingroup backend
 */
ssize_t kdbSet_backend(KDB *handle, KeySet *returned, const Key *parentKey) {
	return 0;
}

/**
 * All KDB methods implemented by the backend can have random names, except
 * kdbBackendFactory(). This is the single symbol that will be looked up
 * when loading the backend, and the first method of the backend
 * implementation that will be called.
 *
 * Its purpose is to publish the exported methods for libelektra.so. The
 * implementation inside the provided skeleton is usually enough: simply
 * call kdbBackendExport() with all methods that must be exported.
 *
 * The first paramter is the name of the backend.
 * Then every backend must have:
 * @c KDB_BE_OPEN,
 * @c KDB_BE_CLOSE,
 * @c KDB_BE_GET and
 * @c KDB_BE_SET
 *
 * You might also give following information by char *:
 * @c KDB_BE_VERSION,
 * @c KDB_BE_AUTHOR,
 * @c KDB_BE_LICENCE and
 * @c KDB_BE_DESCRIPTION
 *
 * You must use static "char arrays" in a read only segment.
 * Don't allocate storage, it won't be freed.
 *
 * With capability you can get that information on
 * runtime from any backend with kdbGetCapability().
 *
 * The last parameter must be @c KDB_BE_END.
 *
 * @return kdbBackendExport() with the above described parameters.
 * @see kdbBackendExport() for an example
 * @see kdbOpenPlugin()
 * @ingroup backend
 */
KDBEXPORT(backend)
{
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,	&kdbOpen_backend,
		KDB_BE_CLOSE,	&kdbClose_backend,
		KDB_BE_GET,	&kdbGet_backend,
		KDB_BE_SET,	&kdbSet_backend,
		KDB_BE_END);
}

