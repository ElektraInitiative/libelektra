/***************************************************************************
  kdbhandle.c  -  Functions which operate on the private kdbhandle struct
                             -------------------
    begin                : Mon Dec 29 2003
    copyright            : (C) 2003 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

/**
 * @defgroup backendhandle KDB Backends :: KDB access functions
 * @brief Methods to access the backend handle.
 *
 * To use them:
 * @code
 * #include <kdb.h>
 * @endcode
 *
 * These functions provide access to the information stored in
 * Backend Handles.
 *
 */


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if DEBUG && HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif


#include "kdbinternal.h"

/**
 * Set some backend-specific @p data in the @p handle.
 *
 * This is useful when your backend have a backend-global context or
 * environment.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param data a pointer to general data specific to a backend implementation.
 * @see kdbhGetBackendData()
 * @ingroup backendhandle
 */
void *kdbhSetBackendData(KDB *handle, void *data) {
	return handle->backendData = data;
}


/**
 * Get the previously set backend-specific @p data from the @p handle.
 *
 * This is useful when your backend have a backend-global context or
 * environment.
 * 
 * This method will probably be called everytime one of your kdb*()
 * implementations is called. And if you change something inside the data, you
 * don't have to kdbhSetBackendData() again, bacause you are manipulating your
 * data, and not a copy of it.
 *
 * @par Example:
 * @code
struct MyBackendData {
 int context1;
 int context2;
};

int kdbOpen_mybackend(KDB *handle) {
	struct MyBackendData *context;

	context=malloc(sizeof(struct MyBackendData));
 
	// a random initialization...
	context->context1=1;
	context->context2=2;

	kdbhSetBackendData(*handle,context);

	return 0;
}

int kdbGetKey_maybackend(KDB handle) {
	struct MyBackendData *context;

	context=kdbhGetBackendData(handle);

	// No do something with the context
	. . .

	return 0;
}
 * @endcode
 *
 * On the kdbClose() implementation of your backend, you must remember to
 * free all resources associated to your data. 
 *
 * @par Example of kdbClose() implementation that correctly cleans the context:
 * @code
int kdbClose_mybackend(KDB &handle) {
	struct MyBackendData *context;

	context=kdbhGetBackendData(handle);
	free(context);

	return 0;
}
 * @endcode
 * @return a pointer to the data previously set be kdbhSetBackendData()
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @ingroup backendhandle
 */
void *kdbhGetBackendData(const KDB *handle) {
	return handle->backendData;
}


/**
 * Sets capabilty for handle.
 *
 * @param cap a pointer to capability structure
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @return The backend name set in @p handle.
 * @ingroup backendhandle
 */
KDBCap* kdbhSetCapability(KDB *handle, KDBCap *cap) {
	return handle->capability = cap;
}

/**
 * Gets capability for handle.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @return The backend name set in @p handle.
 * @ingroup backendhandle
 */
KDBCap* kdbhGetCapability(const KDB *handle) {
	return handle->capability;
}

/**
 * Gets trie for handle.
 *
 * The trie is a datastructure containing the mounted backends.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @see kdbhSetTrie()
 * @return The backend name set in @p handle.
 * @ingroup backendhandle
 */
Trie *kdbhGetTrie(const KDB *handle) {
	return handle->trie;
}

/**
 * Sets trie for handle.
 *
 * The trie is a datastructure containing the mounted backends. This must not done inside
 * backends, it was set correctly already for you.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param trie the datastructure referencing to the other handles of backends
 * @see kdbhGetTrie()
 * @return nothing
 * @ingroup backendhandle
 */
void kdbhSetTrie(KDB *handle, Trie *trie) {
	handle->trie = trie;
}

/**
 * Gets mountpoint for handle.
 *
 * Every mounted backend has a specific mountpoint where it is mounted. You may need to know
 * where you were mounted inside a backend to calculate relative pathes.
 *
 * The keyName() is where the backend is mounted, keyString() gives the name of which
 * backend is mounted.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @see kdbhSetMountpoint()
 * @return The Key containing the mountpoint.
 * @ingroup backendhandle
 */
const Key *kdbhGetMountpoint(KDB *handle) {
	return handle->mountpoint;
}

/**
 * Sets mountpoint for handle.
 *
 * You must not change the mountpoint inside your backend, it was set correctly already
 * for you.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param mountpoint the key containing as name where backend is mounted and as value the backendname
 * @see kdbhGetMountpoint()
 * @return nothing
 * @ingroup backendhandle
 */
void kdbhSetMountpoint(KDB *handle, const Key* mountpoint) {
	if (handle->mountpoint)
		keyDel(handle->mountpoint);
	handle->mountpoint = keyDup(mountpoint);
}

/**
 * Returns configuration for handle.
 *
 * Every backend may have its own configuration using a Keyset.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @return the keyset containing configuration for a backend
 * @ingroup backendhandle
 * */
KeySet *kdbhGetConfig(KDB *handle)
{
	return handle->config;
}

