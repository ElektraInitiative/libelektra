/***************************************************************************
            kdb.c  -  Low level functions for access the Key Database
                             -------------------
 *  begin                : Wed 19 May, 2010
 *  copyright            : (C) 2010 by Markus Raab
 *  email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/



/**
 * @defgroup kdb KDB :: Low Level Methods
 * @brief General methods to access the Key database.
 *
 * To use them:
 * @code
 * #include <kdb.h>
 * @endcode
 *
 * The kdb*() class of methods are used to access the storage, to get and set
 * @link key Keys @endlink or @link keyset KeySets @endlink.
 *
 * The most important functions are:
 *  - kdbOpen()
 *  - kdbClose()
 *  - kdbGet()
 *  - kdbSet()
 *
 * The two essential functions for dynamic information about backends are:
 *  - kdbGetMountpoint()
 *  - kdbGetCapability()
 *
 * They use some backend implementation to know the details about how to access
 * the storage. Currently we have this backends:
 * - @c berkeleydb: the keys are stored in a Berkeley DB database, providing
 *   very small footprint, speed, and other advantages.
 * - @c filesys: the key hierarchy and data are saved as plain text files in 
 *   the filesystem.
 * - @c ini: the key hierarchy are saved into configuration files.
 *   @see http://www.libelektra.org/Ini
 * - @c fstab: a reference backend used to interpret the @c /etc/fstab file as
 *   a set of keys under @c system/filesystems .
 * - @c gconf: makes Elektra use the GConf daemon to access keys. Only the
 *   @c user/ tree is available since GConf is not system wide.
 *
 * Backends are physically a library named @c /lib/libelektra-{NAME}.so.
 *
 * See @link backend writing a new backend @endlink for information
 * about how to write a backend.
 *
 * Language binding writers should follow the same rules:
 * - You must relay completely on the backend-dependent methods.
 * - You may use or reimplement the second set of methods.
 * - You should completely reimplement in your language the higher
 *   lever methods.
 * - Many methods are just for comfort in C. These methods are marked
 *   and need not to be implemented if the binding language has e.g. string
 *   operators which can do the operation easily.
 */


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if DEBUG && HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#include <kdbinternal.h>


KDB* kdbOpenBackend(const char *backendname, const char *mountpoint, KeySet *config)
{
	KDB * handle;
	char* backend_name;

	kdbLibHandle dlhandle=0;
	typedef KDB *(*KDBBackendFactory) (void);
	KDBBackendFactory kdbBackendFactory=0;

	backend_name = malloc(sizeof("libelektra-")+strlen(backendname));

	strncpy(backend_name,"libelektra-",sizeof("libelektra-"));
	strncat(backend_name,backendname,strlen(backendname));

	dlhandle=kdbLibLoad(backend_name);
	if (dlhandle == 0) {
		/*errno=KDB_ERR_EBACKEND;*/
#if DEBUG && VERBOSE
		printf("kdbLibLoad(%s) failed\n", backend_name);
#endif
		goto err_clup; /* error */
	}

	/* load the "kdbBackendFactory" symbol from backend */
	kdbBackendFactory=(KDBBackendFactory)kdbLibSym(dlhandle, "kdbBackendFactory");
	if (kdbBackendFactory == 0) {
		/*errno=KDB_ERR_NOSYS;*/
#if DEBUG && VERBOSE
		printf("Could not kdbLibSym kdbBackendFactory for %s\n", backend_name);
#endif
		goto err_clup; /* error */
	}
	
	handle=kdbBackendFactory();
	if (handle == 0)
	{
		/*errno=KDB_ERR_NOSYS;*/
#if DEBUG && VERBOSE
		printf("Could not call kdbBackendFactory for %s\n", backend_name);
#endif
		goto err_clup; /* error */
	}

	/* save the libloader handle for future use */
	handle->dlHandle=dlhandle;
	handle->trie	= 0;

	handle->mountpoint=keyNew(mountpoint,KEY_VALUE,backendname,0);

	/* let the backend initialize itself */
	if (handle->kdbOpen)
	{
		handle->config = config;
		if ((handle->kdbOpen(handle)) == -1)
		{
#if DEBUG && VERBOSE
			printf("kdbOpen() failed for %s\n", backend_name);
#endif
		}
	}
	else {
		/*errno=KDB_ERR_NOSYS;*/
#if DEBUG && VERBOSE
			printf("No kdbOpen supplied in %s\n", backend_name);
#endif
		goto err_clup;
	}

#if DEBUG && VERBOSE
	printf("Finished loading Backend %s\n", backend_name);
#endif
	free(backend_name);
	return handle;

err_clup:
#if DEBUG
	fprintf(stderr,"Failed to load backend %s\n", backend_name);
#endif
	free(backend_name);
	return 0;
}

int kdbCloseBackend(KDB *handle)
{
	int rc=0;

	if (handle->kdbClose)
		rc=handle->kdbClose(handle);
	
	if (rc == 0) {
		kdbLibClose(handle->dlHandle);
		capDel (handle->capability);
		keyDel(handle->mountpoint);
		if (handle->config) ksDel(handle->config);
		free(handle);
	}
	
	return rc;
}


/**
 * Dynamically mount a single backend.
 *
 * Maps the mountpoint, defined through its name and value, into the global elektra
 * hierachy. If successfull, under the mountpoint another backend will reside.
 *
 * This only works for a single KDB, that means a single thread in a single process.
 * You may want statically mounting by editing system/elektra/mountpoints.
 *
 * If you allocated mountpoint and config first, make sure that you free it!
 * It is ok to free it immediately afterwards.
 *
 * @param handle handle to the kdb data structure 
 * @param mountpoint the keyName() of this key is the mountpoint, keyValue() the backend
 * @param config the configuration passed for that backend
 * @return 0 on success, -1 if an error occurred
 * @ingroup kdb
*/
int kdbMount(KDB *handle, const Key *mountpoint, const KeySet *config)
{
	char *mountpoint_slash;
	const char *key_name;
	const char *backend;
	size_t size;
	KDB *h;
	Trie *trie;
	KeySet *c;

	key_name=keyName(mountpoint);
	backend=keyValue(mountpoint);

	size = kdbiStrLen(key_name);
	mountpoint_slash = malloc (size + 1);
	strcpy (mountpoint_slash, key_name);
	mountpoint_slash [size-1] = '/';
	mountpoint_slash [size] = 0;

	h=kdbOpenBackend(backend,mountpoint_slash,c=ksDup(config));
	if (!h) {
		free(mountpoint_slash);
		ksDel(c);
		return -1;
	}
	trie=insert_trie(kdbhGetTrie(handle), mountpoint_slash, h);
	kdbhSetTrie(handle,trie);

	free(mountpoint_slash);
	return 0;
}


/**
 * Dynamically unmount a single backend.
 *
 * Unmount a backend that was mounted with kdbMount() before.
 *
 * @param handle handle to the kdb data structure 
 * @param mountpoint directory where backend is mounted to, that should be unmounted
 * @return 0 on success, -1 if an error ocurred.
 * @ingroup kdb
 */

int kdbUnmount(KDB *handle, const Key *mountpoint)
{
	size_t size;
	const char *key_name;
	char *mountpoint_slash;

	if (mountpoint==NULL) {
		return -1;
	}
	if (kdbGetBackend(handle,mountpoint)==NULL) {
		return -1;
	}
	key_name=keyName(mountpoint);

	size = kdbiStrLen(key_name);
	mountpoint_slash = malloc (size + 1);
	strcpy (mountpoint_slash, key_name);
	mountpoint_slash [size-1] = '/';
	mountpoint_slash [size] = 0;

	delete_trie(kdbhGetTrie(handle), mountpoint_slash,kdbCloseBackend);
	free(mountpoint_slash);
	return 0;
}



/**
 * Lookup a mountpoint in a handle for a specific key.
 *
 * Will return a key representing the mountpoint or null
 * if there is no appropriate mountpoint e.g. its the
 * root mountpoint.
 *
 * Together with kdbGetCapability() the two essential
 * informations about mounted backends.
 *
 * @par Example:
 * @code
Key * key = keyNew ("system/template");
KDB * handle = kdbOpen();
Key *mountpoint=0;
mountpoint=kdbGetMountpoint(handle, key);

printf("The library I am using is %s mounted in %s\n",
	keyValue(mountpoint),
	keyName(mountpoint));
kdbClose (handle);
keyDel (key);
 * @endcode
 *
 *
 * @param handle is the data structure, where the mounted directories are saved.
 * @param where the key, that should be looked up.
 * @return the mountpoint associated with the key
 * @ingroup kdb
 */
Key* kdbGetMountpoint (KDB *handle, const Key *where)
{
	KDB *backend_handle;

	backend_handle=kdbGetBackend(handle,where);
	if (!backend_handle)
	{
		/*errno = KDB_ERR_EBACKEND;*/
		return 0;
	}

	return backend_handle->mountpoint;
}


/*
 * Returns a structure of information about the internals
 * of the library and the backend used.
 *
 * This is mainly used to check the capabilities of a specific
 * backend.
 *
 * @par Example:
 * @code
Key * key = keyNew ("system/template");
KDB * handle = kdbOpen();
KDBCap *capability=0;
capability=kdbGetCapability(handle, key);

printf("The library I am using is %s in version %s\n",
	kdbcGetName(capability),
	kdbcGetVersion(capability));
if (kdbcGetnoError(capability)) printf ("Ohh! Error states are not supported\n");
else printf ("Puhh! Error states are supported\n");
kdbClose (handle);
keyDel (key);
 * @endcode
 *
 * Together with kdbGetMountpoint() the two essential
 * informations about mounted backends.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param where lets you choose the position of where you want the information from
 * @return info on sucess, 0 if @p info is NULL
 * 	errno will be set to KDB_ERR_EBACKEND if no backend is found
 * @see commandInfo()
 * @ingroup kdb
 */
KDBCap *kdbGetCapability(KDB *handle, const Key *where)
{
	KDB *backend_handle;

	backend_handle=kdbGetBackend(handle,where);
	if (!backend_handle)
	{
		/*errno = KDB_ERR_EBACKEND;*/
		return 0;
	}

	return backend_handle->capability;
}



/**
 * Opens the session with the Key database.
 *
 * The first step is to open the default backend. With it
 * system/elektra/mountpoints will be loaded and all needed
 * libraries and mountpoints will be determined.
 * These libraries for backends will be loaded and with it the
 * @p KDB datastructure will be initialized.
 *
 * You must always call this method before retrieving or commiting any
 * keys to the database. In the end of the program,
 * after using the key database, you must not forget to kdbClose().
 * You can use the atexit () handler for it.
 *
 * The pointer to the @p KDB structure returned will be initialized
 * like described above, and it must be passed along on any kdb*()
 * method your application calls.
 *
 * Get a @p KDB handle for every thread using elektra. Don't share the
 * handle across threads, and also not the pointer accessing it:
 * @code
thread1 {
	KDB * h;
	h = kdbOpen();
	// fetch keys and work with them
	kdbClose(h);
}
thread2 {
	KDB * h;
	h = kdbOpen();
	// fetch keys and work with them
	kdbClose(h);
}
 * @endcode
 *
 * You don't need to use the kdbOpen() if you only want to
 * manipulate plain in-memory Key or KeySet objects without any affairs with
 * the backend key database,
 *
 * @see kdbClose() to end all affairs to the key database.
 * @return a KDB pointer on success
 * @return NULL on failure
 * @ingroup kdb
 */
KDB * kdbOpen()
{
	KDB * handle;
	KeySet *keys;
	Trie *trie;
#if DEBUG && VERBOSE
	Key *key;

	fprintf (stderr, "open elektra " KDB_VERSION "\n");
#endif

	if (kdbLibInit()) {
		/*errno=KDB_ERR_NOSYS;*/
		return 0;
	}

	/* Open default backend */
	handle=kdbOpenBackend("default",0,0);
	if (!handle)
	{
#if DEBUG
		printf ("failed to open default backend");
#endif
		return 0;
	}

	/* get mount config from root backend */
	keys=ksNew(0);

	/* TODO added KDB_O_NORECURSIVE because kdbGet() code is broken at the moment */
	kdbGet(handle,keys,keyNew(KDB_KEY_MOUNTPOINTS,KEY_END),KDB_O_DEL|KDB_O_NORECURSIVE);

#if DEBUG && VERBOSE
	ksRewind(keys);
	for (key=ksNext(keys);key;key=ksNext(keys)) {
		printf("config for createTrie name: %s value: %s\n",keyName(key),(char*) keyValue(key));
	}
#endif
	trie=createTrie(keys,kdbOpenBackend);
	kdbhSetTrie(handle, trie);
	ksDel(keys);

	return handle;
}

/**
 * Closes the session with the Key database.
 *
 * You should call this method when you finished your affairs with the key
 * database. You can manipulate Key and KeySet objects also after kdbClose().
 * You must not use any kdb* call afterwards. You can implement kdbClose()
 * in the atexit() handler.
 *
 * This is the counterpart of kdbOpen().
 *
 * The @p handle parameter will be finalized and all resources associated to it
 * will be freed. After a kdbClose(), this @p handle can't be used anymore,
 * unless it gets initialized again with another call to kdbOpen().
 *
 * @see kdbOpen()
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @return 0 on success
 * @return -1 on NULL pointer
 * @ingroup kdb
 */
int kdbClose(KDB *handle)
{
	if (!handle)
	{
		/*errno=KDB_ERR_NOSYS;*/
		return -1;
	}
	if (handle->trie)
		kdbDelTrie (handle->trie,kdbCloseBackend);

	kdbCloseBackend (handle);

	return 0;
}



/**
 * Retrieve keys in an atomic and universal way, all other kdbGet Functions
 * rely on that one.
 *
 * The @p returned KeySet must be initialized or may already contain some
 * keys. The new retrieved keys will be appended using ksAppendKey().
 *
 * In default behaviour (@p options = 0) it will fully retrieve all keys
 * under the @p parentKey folder, with all subfolders and their children
 * but not inactive keys or folders.
 *
 * The keyset will not be sorted at first place, but will be marked dirty and
 * sorted afterwards when needed. That could be a subsequent ksLookup(),
 * ksLookupByName() or kdbSet(). See ksSort() on that issue.
 *
 * The behaviour can be fine-tuned with options in various ways to make kdbGet() more
 * comfortable.
 *
 * @section kdbgetoption Options
 *
 * The @p option is an array of the following ORed flags:
 *
 * - @p option_t::KDB_O_DEL \n
 *   Its often useful to keyDel() the parentKey in the line after kdbGet().
 *   Using this flag, you can just pass a key allocated with keyNew(),
 *   kdbGet() will free it for you in the end.
 * - @p option_t::KDB_O_POP \n
 *   The @p parentKey itself will always be added to @p returned.
 *   If you only want the children
 *   of the parentKey in @p returned, but not the parentKey itself, use this flag.
 *   This is only valid for the first parentKey, the one you passed.
 *   The other recursive parentKeys will stay in the keyset.
 *   To get only the leaves of the tree, without any parentKey,
 *   see @ref option_t::KDB_O_NODIR below.
 * - @p option_t::KDB_O_NODIR \n
 *   Don't include folders in the @p returned KeySet, so only keys without
 *   subkeys. You can picture it best that you only get the leaves of the
 *   tree of keys.
 * - @p option_t::KDB_O_DIRONLY \n
 *   Put in @p returned only the folder keys. The resulting 
 *   KeySet will be only the skeleton of the tree. This option must not be
 *   ORed together with KDB_O_DIR.
 * - @p option_t::KDB_O_INACTIVE \n
 *   Will make it not ignore inactive keys, so @p returned will contain also
 *   inactive keys. Inactive keys are those that have names
 *   begining with '.' (dot).
 *   Please be sure that you know what you are doing, inactive keys must not
 *   have any semantics to the application. This flag should only be set in
 *   key browsers after explicit user request.
 *   You might also get inactive keys when you plan to remove a whole
 *   hierarchy.
 * - @p option_t::KDB_O_SORT \n
 *   Force @p returned to be ksSort()ed. Normally you don't want that the
 *   @p returned is sorted immediately because you might add other keys or
 *   go for another kdbGet(). Sorting will
 *   take place automatically when needed by ksLookup() or kdbSet(),
 *   also without this option set.
 *   But you need to sort the keyset for yourself, when you just iterate
 *   over it. If you want to do that, pass this flag at the last kdbGet().
 * - @p option_t::KDB_O_NORECURSIVE \n
 *   Dont get the keys recursive. Only receive keys from one folder.
 *   This might not work if the backend does not support it. Be prepared
 *   for more keys and use ksLookup() and avoid static assumptions
 *   on how many keys you get.
 *
 * @par Example:
 * @code
KDB *handle;
KeySet *myConfig;
Key *key;

myConfig=ksNew(0);

handle = kdbOpen();

key=keyNew("system/sw/MyApp",KEY_END);
rc=kdbGet(handle,key, myConfig, 0);
keyDel(key);

key=keyNew("user/sw/MyApp",KEY_END);
rc=kdbGet(handle,key, myConfig, 0);
keyDel(key);

// will sort keyset here
key=ksLookupByName(myConfig,"/sw/MyApp/key", 0);
// check if key is not 0 and work with it...

ksDel (myConfig); // delete the in-memory configuration


// maybe you want kdbSet() myConfig here

kdbClose(handle); // no more affairs with the key database.
 * @endcode
 *
 * @section kdbgetdetail Details
 *
 * When no backend could be found (e.g. no backend mounted)
 * the default backend will be used.
 *
 * If you pass a NULL pointer as handle and/or returned  kdbGet() will
 * return -1 and do nothing but keyDel() the parentKey when requested
 * and not a NULL pointer.
 *
 * If you pass NULL as parentKey the root keys of all namespaces
 * will be appended to returned.
 *
 * For every directory key (keyIsDir()) the appropriate backend
 * will be chosen and keys in it will be requested.
 *
 * If any backend reports an failure the recursive getting of
 * keys will be stopped. Backends only report failure when they
 * are not able to get keys for any problems.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param parentKey parent key or NULL to get the root keys
 * @param returned the (pre-initialized) KeySet returned with all keys found
 * @param options ORed options to control approaches
 * @see #option_t
 * @see @link kdbhighlevel kdb higher level Methods @endlink that rely on kdbGet()
 * @see ksLookupByName(), ksLookupByString() for powerful
 * 	lookups after the KeySet was retrieved
 * @see commandList() code in kdb command for usage example
 * @see commandEdit() code in kdb command for usage example
 * @see commandExport() code in kdb command for usage example
 * @return number of keys contained by @p returned
 * @return -1 on failure
 * @ingroup kdb
 *
 */
ssize_t kdbGet (KDB *handle, KeySet *returned,
	Key * parentKey, option_t options)
{
	ssize_t size = 0; /* nr of keys get */
	ssize_t ret = 0;
	KeySet *keys;
	KeySet *tmp;
	Key *current;
	KDB *backend_handle;
	KDB *try_handle;

	if (!handle || !returned)
	{
		if (parentKey && (options & KDB_O_DEL)) keyDel (parentKey);
		/* errno=KDB_ERR_NOSYS; */
		return -1;
	}

	if (!parentKey)
	{
		ksAppendKey(returned, keyNew("user", KEY_DIR, KEY_END)); ret ++;
		ksAppendKey(returned, keyNew("system", KEY_DIR, KEY_END)); ret ++;
		return ret;
	}

#if DEBUG && VERBOSE
	fprintf (stderr, "now in new kdbGet (%s)\n", keyName(parentKey));
#endif

	backend_handle=kdbGetBackend(handle,parentKey);
	if (backend_handle==NULL)
		backend_handle=handle;

	keys = ksNew (0);
	tmp = ksNew (0);
	ksRewind (returned);
	while ((current = ksPop(returned)) != 0)
	{
		if (keyIsDirectBelow(parentKey, current))
		{
			set_bit (current->flags, KEY_FLAG_SYNC);
			ksAppendKey(keys, current);
		} else {
			ksAppendKey(tmp, current);
		}
	}

	ksAppend (returned, tmp);
	ksDel (tmp);

	ret = backend_handle->kdbGet(backend_handle,keys,parentKey);
	if (ret == -1)
	{
		if (options & KDB_O_DEL) keyDel (parentKey);
		ksDel (keys);
#if DEBUG && VERBOSE
		fprintf (stderr, "call of handle->kdbGet failed\n");
#endif
		return -1;
	}

	ksRewind(keys);
	while ((current = ksPop(keys)) != 0)
	{
		const char *parentName = keyName(parentKey);
		const char *currentName = keyName(current);
		if (keyNeedSync(current))
		{	/* Key was not updated, throw it away */
			keyDel (current);
			continue;
		}
		if (strcmp (parentName, currentName) == 0)
		{
			if (! (options & KDB_O_POP))
			{
				if (keyIsDir(current))
				{
					if (options & KDB_O_NODIR)
					{
						keyDel (current);
						continue;
					}
				} else if (options & KDB_O_DIRONLY) {
					keyDel (current);
					continue;
				}
				size ++;
				ksAppendKey(returned, current);
			} else {
				keyDel (current);
			}
			continue;
		}
		if ((!(options & KDB_O_INACTIVE)) && keyIsInactive (current))
		{
			keyDel (current);
			continue;
		}
		if (keyIsDir (current))
		{
			if (options & KDB_O_NODIR)
			{
				keyDel (current);
				continue;
			}
		}
		else if (options & KDB_O_DIRONLY)
		{
			keyDel (current);
			continue;
		}
		try_handle=kdbGetBackend(handle,current);
		if (try_handle != backend_handle)
		{
			/* Ohh, another backend is responsible, so we will delete the key */
			if (! (options & KDB_O_NORECURSIVE))
			{
				/* This key resides somewhere else, go recurse */
				ret = kdbGet(handle, returned, current,
					options & ~KDB_O_DEL & ~KDB_O_SORT & ~KDB_O_POP);
				if (ret == -1)
				{
#if DEBUG && VERBOSE
					fprintf (stderr, "recursive call failed\n");
#endif
					size = -1;
					keyDel (current);
					break;
				}
				size += ret;
			}
			keyDel (current);
			continue;
		}
		if (size > -1)
		{
			size ++;
			ksAppendKey(returned, current);
		}
	}
	ksDel(keys);

	if (options & KDB_O_SORT) ksSort (returned);
	if (options & KDB_O_DEL) keyDel (parentKey);
	return size;
}



/**
 * Set keys in an atomic and universal way, all other kdbSet Functions
 * rely on that one.
 *
 * The given handle and keyset are the objects to work with.
 *
 * With parentKey you can only store a part of the given keyset.
 * Otherwise pass a null pointer or a parentKey without a name.
 *
 * @code
KeySet *ks = ksNew(0);
kdbGet (h, ks, keyNew("system/myapp",0), KDB_O_DEL);
kdbGet (h, ks, keyNew("user/myapp",0), KDB_O_DEL);

//now only set everything below user, because you can't write to system
kdbSet (h, ks, keyNew("user/myapp",0), KDB_O_DEL);

ksDel (ks);
 * @endcode
 *
 *
 * Each key is checked with keyNeedSync() before being actually committed. So
 * only changed keys are updated. If no key of a backend needs to be synced
 * the kdbSet_backend() will be omitted.
 *
 * If some error occurs, kdbSet() will stop. In this situation the KeySet
 * internal cursor will be set on the key that generated the error.
 * This specific key and all behind it were not set.
 * To be failsafe jump over it and try to set the rest, but report the error
 * to the user.
 *
 * @par Example of how this method can be used:
 * @code
int i;
KeySet *ks;  // the KeySet I want to set
// fill ks with some keys
for (i=0; i< 10; i++) // limit to 10 tries
{
	ret=kdbSet(handle,ks, 0, 0);
	if (ret == -1)
	{
		// We got an error. Warn user.
		Key *problem;
		problem=ksCurrent(ks);
		if (problem)
		{
			char keyname[300]="";
			keyGetFullName(problem,keyname,sizeof(keyname));
			fprintf(stderr,"kdb import: while importing %s", keyname);
		} else break;
		// And try to set keys again starting from the next key,
		// unless we reached the end of KeySet
		if (ksNext(ks) == 0) break;
	}
}
 * @endcode
 *
 * @section kdbsetoption Options
 *
 * There are some options changing the behaviour of kdbSet():
 *
 * - @p option_t::KDB_O_DEL \n
 *   Its often useful to keyDel() the parentKey in the line after kdbGet().
 *   Using this flag, you can just pass a key allocated with keyNew(),
 *   kdbGet() will free it for you in the end.
 * - @p option_t::KDB_O_SYNC \n
 *   Will force to save all keys, independent of their sync state.
 * - @p option_t::KDB_O_NOREMOVE \n
 *   Don't remove any key from disk, even with an empty keyset.
 *   With that flag removing keys can't happen unintentional.
 * - @p option_t::KDB_O_REMOVEONLY \n
 *   Remove all keys instead of setting them.
 *   because the sync state will be changed when they are marked remove.
 *   You might need @ref option_t::KDB_O_INACTIVE set for the previous call
 *   of kdbGet() if there are any. Otherwise the recursive remove will fail,
 *   because removing directories is only possible when all subkeys are
 *   removed.
 *
 * @section kdbsetdetail Details
 *
 * When you dont have a parentKey or its name empty, then all keys will
 * be set.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param ks a KeySet which should contain changed keys, otherwise nothing is done
 * @param parentKey holds the information below which key keys should be set
 * @param options see in kdbSet() documentation
 * @return 0 on success
 * @return -1 on failure
 * @see keyNeedSync(), ksNext(), ksCurrent()
 * @see commandEdit(), commandImport() code in kdb command for usage and error
 *       handling example
 * @ingroup kdb
 */
ssize_t kdbSet (KDB *handle, KeySet *ks,
	Key * parentKey, option_t options)
{
	size_t i;
	int t=0;
	KDB *h;
	Key *errorKey;

	int size = 0;
	int errors_occurred=0;
	Split *keysets;

	if (parentKey && !parentKey->key)
	{
		if (options & KDB_O_DEL) keyDel (parentKey);
		parentKey = 0;
	}

#if DEBUG && VERBOSE
	fprintf (stderr, "now in new kdbSet (%s)\n", keyName(parentKey));
#endif

	if (!handle || !ks)
	{
		if (parentKey && (options & KDB_O_DEL)) keyDel (parentKey);
		/*errno=KDB_ERR_NOSYS;*/
		return -1;
	}

	if (!kdbhGetTrie(handle)) 
	{ /* Fallback code without mounting */
		ksRewind (ks);
		return handle->kdbSet(handle,ks,parentKey);
	}

	keysets=split_keyset(handle, ks, parentKey, options);

	for (i=0; i<keysets->no;i++) {
		t=0;

		h=keysets->handles[i];
		/* if there is no backend in the trie use the default */
		if (h==NULL) {
			h=handle;
		}
		if (keysets->syncbits[i] && keysets->belowparents[i])
		{
			ksRewind (keysets->keysets[i]);
			t=h->kdbSet(h,keysets->keysets[i],keysets->parents[i]);
		}
		if (t==-1) {
			errors_occurred=1;
			errorKey = ksCurrent (keysets->keysets[i]);
			if (errorKey) ksLookup(ks, errorKey, KDB_O_WITHOWNER);
			break;
		}
		else {
			size+=t;
		}
	}

	free_splitted_keysets(keysets);
	if (options & KDB_O_DEL) keyDel (parentKey);
	if (errors_occurred)
		return -1;
	return size;
}



/**
 * This function must be called by a backends's kdbBackendFactory() to
 * define the backend's methods that will be exported.
 *
 * See KDBEXPORT() how to use it for backends.
 *
 * The order and number of arguments are flexible (as in keyNew() and ksNew()) to let
 * libelektra.so evolve without breaking its ABI compatibility with backends.
 * So for each method a backend must export, there is a flag defined by
 * #backend_t. Each flag tells kdbBackendExport() which method comes
 * next. A backend can have no implementation for a few methods that have
 * default inefficient high-level implementations and to use these defaults, simply
 * don't pass anything to kdbBackendExport() about them.
 *
 * @param backendName a simple name for this backend
 * @return an object that contains all backend informations needed by
 * 	libelektra.so
 * @ingroup backend
 */
KDB *kdbBackendExport(const char *backendName, ...) {
	va_list va;
	KDB *returned;
	backend_t method=0;

	if (backendName == 0) return 0;

	returned=malloc(sizeof(KDB));
	memset(returned,0,sizeof(KDB));

	returned->capability = capNew();
	returned->capability->name = backendName;
	returned->capability->version = "";
	returned->capability->description = "";
	returned->capability->author = "";
	returned->capability->licence = "";

	/* Start processing parameters */
	
	va_start(va,backendName);

	while ((method=va_arg(va,backend_t))) {
		switch (method) {
			case KDB_BE_OPEN:
				returned->kdbOpen=va_arg(va,kdbOpenPtr);
				break;
			case KDB_BE_CLOSE:
				returned->kdbClose=va_arg(va,kdbClosePtr);
				break;
			case KDB_BE_GET:
				returned->kdbGet=va_arg(va,kdbGetPtr);
				break;
			case KDB_BE_SET:
				returned->kdbSet=va_arg(va,kdbSetPtr);
				break;
			case KDB_BE_VERSION:
				returned->capability->version=va_arg(va, char *);
				break;
			case KDB_BE_DESCRIPTION:
				returned->capability->description=va_arg(va, char *);
				break;
			case KDB_BE_AUTHOR:
				returned->capability->author=va_arg(va, char *);
				break;
			case KDB_BE_LICENCE:
				returned->capability->licence=va_arg(va, char *);
				break;
			default:
#if DEBUG
				printf ("backend passed something unexpected");
#endif
			case KDB_BE_END:
				va_end(va);
				return returned;
		}
	}
	return returned;
}

