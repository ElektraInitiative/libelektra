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
 * @defgroup kdb KDB
 * @brief General methods to access the Key database.
 *
 * To use them:
 * @code
 * #include <kdb.h>
 * @endcode
 *
 * The kdb*() methods are used to access the storage, to get and set
 * @link keyset KeySets @endlink.
 *
 * They use some backend implementation to know the details about how
 * to access the storage.
 *
 * One backend consists of multiple plugins.
 * See @link plugin writing a new plugin @endlink for information
 * about how to write a plugin.
 *
 * @{
 */


#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
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


/**
 * @brief Opens the session with the Key database.
 *
 * The method will bootstrap itself the following way.
 * The first step is to open the default backend. With it
 * system/elektra/mountpoints will be loaded and all needed
 * libraries and mountpoints will be determined.
 * These libraries for backends will be loaded and with it the
 * @p KDB datastructure will be initialized.
 *
 * You must always call this method before retrieving or committing any
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
thread1
{
	KDB * h;
	h = kdbOpen(0);
	// fetch keys and work with them
	kdbClose(h, 0);
}
thread2
{
	KDB * h;
	h = kdbOpen(0);
	// fetch keys and work with them
	kdbClose(h, 0);
}
 * @endcode
 *
 * You don't need to use the kdbOpen() if you only want to
 * manipulate plain in-memory Key or KeySet objects.
 *
 * @pre errorKey must be a valid key, e.g. created with keyNew()
 *
 * @param errorKey the key which holds errors and warnings which were issued
 * @see kdbGet(), kdbClose() to end all affairs to the key database.
 * @return a KDB pointer on success
 * @return NULL on failure
 * @ingroup kdb
 */
KDB * kdbOpen(Key *errorKey)
{
	KDB *handle;
	KeySet *keys;

	handle = elektraCalloc(sizeof(struct _KDB));

	handle->modules = ksNew(0, KS_END);
	if(elektraModulesInit(handle->modules, errorKey) == -1)
	{
		ksDel(handle->modules);
		elektraFree(handle);
		ELEKTRA_SET_ERROR(94, errorKey,
				"elektraModulesInit returned with -1");
		return 0;
	}

	handle->defaultBackend=elektraBackendOpenDefault(handle->modules,
			errorKey);
	if(!handle->defaultBackend)
	{
		ksDel(handle->modules);
		elektraFree(handle);
		ELEKTRA_SET_ERROR(40, errorKey,
				"could not open default backend");
		return 0;
	}

	handle->split = elektraSplitNew();
	elektraSplitAppend (handle->split, handle->defaultBackend,
			keyNew (KDB_KEY_MOUNTPOINTS, KEY_END), 2);

	keys=ksNew(0, KS_END);

	Key *initialParent = keyDup (errorKey);
	keySetName(errorKey, KDB_KEY_MOUNTPOINTS);

	if (kdbGet(handle, keys, errorKey) == -1)
	{
		ELEKTRA_ADD_WARNING(17, errorKey,
				"kdbGet() of " KDB_KEY_MOUNTPOINTS
				" failed");
		elektraBackendClose(handle->defaultBackend, errorKey);
		elektraSplitDel(handle->split);
		handle->defaultBackend = 0;
		handle->trie = 0;

		keySetName(errorKey, keyName(initialParent));
		keyDel(initialParent);
		return handle;
	}
	keySetName(errorKey, keyName(initialParent));

	elektraBackendClose(handle->defaultBackend, errorKey);
	elektraSplitDel(handle->split);
	handle->defaultBackend = 0;
	handle->trie = 0;

#if DEBUG && VERBOSE
	Key *key;

	ksRewind(keys);
	for (key=ksNext(keys);key;key=ksNext(keys))
	{
		printf("config for createTrie name: %s value: %s\n",
				keyName(key), keyString(key));
	}
#endif

	handle->split = elektraSplitNew();

	// Open the trie, keys will be deleted within elektraMountOpen
	if (elektraMountOpen(handle, keys, handle->modules, errorKey) == -1)
	{
		ELEKTRA_ADD_WARNING(93, errorKey,
				"Initial loading of trie did not work");
	}

	if (elektraMountDefault(handle, handle->modules, errorKey) == -1)
	{
		ELEKTRA_SET_ERROR(40, errorKey,
				"could not reopen and mount default backend");
		kdbClose(handle, errorKey);
		keyDel (initialParent);
		return 0;
	}

	if (elektraMountModules(handle, handle->modules, errorKey) == -1)
	{
		ELEKTRA_ADD_WARNING(92, errorKey,
				"Mounting modules did not work");
	}

	elektraMountVersion (handle, errorKey);

	keyDel (initialParent);
	return handle;
}


/**
 * Closes the session with the Key database.
 *
 * This is the counterpart of kdbOpen().
 *
 * You must call this method when you finished your affairs with the key
 * database. You can manipulate Key and KeySet objects also after
 * kdbClose(), but you must not use any kdb*() call afterwards. You
 * can run kdbClose() in the atexit() handler.
 *
 * The @p handle parameter will be finalized and all resources associated to it
 * will be freed. After a kdbClose(), the @p handle can't be used anymore.
 *
 * @pre The handle must be a valid handle as returned from kdbOpen()
 *
 * @pre errorKey must be a valid key, e.g. created with keyNew()
 *
 * @param handle contains internal information of
 *               @link kdbOpen() opened @endlink key database
 * @param errorKey the key which holds error information
 * @return 0 on success
 * @return -1 on NULL pointer
 * @ingroup kdb
 */
int kdbClose(KDB *handle, Key *errorKey)
{
	if (!handle)
	{
		return -1;
	}

	elektraSplitDel (handle->split);

	elektraTrieClose(handle->trie, errorKey);

	elektraBackendClose (handle->defaultBackend, errorKey);
	handle->defaultBackend = 0;

	if (handle->modules)
	{
		elektraModulesClose (handle->modules, errorKey);
		ksDel (handle->modules);
	}
	else
	{
		ELEKTRA_ADD_WARNING(47, errorKey,
				"modules were not open");
	}

	elektraFree(handle);

	return 0;
}

/**
 * @internal
 *
 * @brief Check if an update is needed at all
 *
 * @retval -1 an error occurred
 * @retval 0 no update needed
 * @retval number of plugins which need update
 */
static int elektraGetCheckUpdateNeeded(Split *split, Key *parentKey)
{
	int updateNeededOccurred = 0;
	for (size_t i=0; i<split->size;i++)
	{
		int ret = 0;
		Backend *backend = split->handles[i];

		if (backend->getplugins[0])
		{
			ksRewind(split->keysets[i]);
			keySetName(parentKey,
					keyName(split->parents[i]));
			ret = backend->getplugins[0]->kdbGet (
					backend->getplugins[0],
					split->keysets[i],
					parentKey);
			// store resolved filename
			keySetString(split->parents[i],
					keyString(parentKey));
		}
		if (ret == -1)
		{
			// Ohh, an error occurred, lets stop the
			// process.
			return -1;
		}
		if (ret == 1)
		{
			/* Seems like we need to sync that */
			split->syncbits[i] |= 1;
			++ updateNeededOccurred;
		}
	}
	return updateNeededOccurred;
}

/**
 * @internal
 * @brief Do the real update.
 *
 * @retval -1 on error
 * @retval 0 on success
 */
static int elektraGetDoUpdate(Split *split, Key *parentKey)
{
	for (size_t i=0; i<split->size-1;i++)
	{
		Backend *backend = split->handles[i];
		ksRewind (split->keysets[i]);
		keySetName (parentKey, keyName(split->parents[i]));
		keySetString(parentKey,
				keyString(split->parents[i]));

		for (size_t p=1; p<NR_OF_PLUGINS; ++p)
		{
			int ret = 0;
			if (backend->getplugins[p])
			{
				ret = backend->getplugins[p]->kdbGet(
						backend->getplugins[p],
						split->keysets[i],
						parentKey);
			}
			if (ret == -1)
			{
				// Ohh, an error occurred,
				// lets stop the process.
				return -1;
			}
		}
	}
	return 0;
}


/**
 * @brief Retrieve keys in an atomic and universal way.
 *
 * @pre kdbOpen() must be called before using this method.
 *
 * @pre The @p returned KeySet must be a valid KeySet, e.g. constructed
 *     with ksNew().
 *
 * The @p returned KeySet may already contain some keys from previous
 * kdbGet() calls. The new retrieved keys will be appended using
 * ksAppendKey().
 *
 * It will fully retrieve, at least, all keys under the @p parentKey
 * folder, with all subfolders and their children.
 *
 * @note kdbGet() might retrieve more keys then requested (that are not
 *     below parentKey). These keys must be passed to calls of kdbSet(),
 *     otherwise they will be lost. This stems from the fact that the
 *     user has the only copy of the whole configuration and backends
 *     only write configuration that was passed to them.
 *     For example, if you kdbGet() "system/mountpoint/interest"
 *     you will not only get all keys below system/mountpoint/interest,
 *     but also all keys below system/mountpoint (if system/mountpoint
 *     is a mountpoint as the name suggests, but
 *     system/mountpoint/interest is not a mountpoint).
 *     Make sure to not touch or remove keys outside the keys of interest,
 *     because others may need them!
 *
 * @see ksCut() for further remarks on that topic.
 *
 * @par Example:
 *
 * This example demonstrates the typical usecase within an application
 * (without error handling).
 *
 * @code
KeySet *myConfig = ksNew(0,KS_END);
Key *pkey = keyNew("system/sw/MyApp",KEY_END);
KDB *handle = kdbOpen(pkey);

kdbGet(handle, myConfig, pkey); // get the config initially

Key *key = ksLookupByName(myConfig,"/sw/MyApp/key", 0);
// check if key is not 0 and work with it...

kdbGet(handle, myConfig, pkey); // get the config again

ksDel (myConfig); // delete the in-memory configuration

kdbClose(handle, pkey); // no more affairs with the key database.
keyDel(pkey);
 * @endcode
 *
 * @par Details:
 *
 * If you pass NULL on any parameter kdbGet() will fail
 * immediately without doing anything.
 *
 * When a backend fails kdbGet() will return -1 with all
 * error and warning information in the @p parentKey.
 * The parameter @p returned will not be changed.
 *
 * @par Updates:
 *
 * In the first run of kdbGet all keys are retrieved. On subsequent
 * calls only the keys are retrieved where something was changed
 * inside the key database. The other keys stay unchanged in the
 * keyset, even when they were manipulated.
 *
 * It is your responsibility to save the original keyset if you
 * need it afterwards.
 *
 * If you must get the same keyset again, e.g. in another
 * thread you need to open a second handle to the key database
 * using kdbOpen().
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param parentKey parent key holds the information which keys should
 * 	be get (it is possible that more are retrieved) - an invalid name gets all keys
 * @param ks the (pre-initialized) KeySet returned with all keys found
 * 	will not be changed on error or if no update is required
 * @see ksLookup(), ksLookupByName() for powerful
 * 	lookups after the KeySet was retrieved
 * @see kdbSet() to save the configuration afterwards and kdbClose() to
 * 	finish affairs with the key database.
 * @retval 1 if the keys were retrieved successfully
 * @retval 0 if there was no update - no changes are made to the keyset then
 * @retval -1 on failure - no changes are made to the keyset then
 * @ingroup kdb
 */
int kdbGet(KDB *handle, KeySet *ks, Key *parentKey)
{
	if (!parentKey)
	{
		return -1;
	}

#if DEBUG && VERBOSE
	fprintf (stderr, "now in new kdbGet (%s)\n",
			keyName(parentKey));
#endif

	Split *split = elektraSplitNew();

	Key *initialParent = keyDup (parentKey);

	if(!handle || !ks)
	{
		ELEKTRA_SET_ERROR(37, parentKey,
				"handle or ks null pointer");
		goto error;
	}

	if(elektraSplitBuildup (split, handle, parentKey) == -1)
	{
		ELEKTRA_SET_ERROR(38, parentKey,
				"error in elektraSplitBuildup");
		goto error;
	}

	// Check if a update is needed at all
	switch(elektraGetCheckUpdateNeeded(split, parentKey))
	{
	case 0: // We don't need an update so let's do nothing
		keySetName (parentKey, keyName(initialParent));
		keyDel (initialParent);
		elektraSplitDel (split);
		return 0;
	case -1: goto error;
	// otherwise falltrough
	}

	if(elektraSplitAppoint (split, handle, ks) == -1)
	{
		ELEKTRA_SET_ERROR (38, parentKey, "error in elektraSplitAppoint");
		goto error;
	}

	/* Now do the real updating,
	  but not for bypassed keys in split->size-1 */
	if(elektraGetDoUpdate(split, parentKey) == -1)
	{
		goto error;
	}

	/* Now postprocess the updated keysets */
	if (elektraSplitGet (split, parentKey, handle) == -1)
	{
		ELEKTRA_SET_ERROR(8, parentKey, keyName(ksCurrent(ks)));
		goto error;
	}

	/* We are finished, now just merge everything to returned */
	ksClear (ks);
	elektraSplitMerge (split, ks);

	keySetName (parentKey, keyName(initialParent));
	keyDel (initialParent);
	elektraSplitDel (split);
	return 1;

error:
	keySetName (parentKey, keyName(initialParent));
	keyDel (initialParent);
	elektraSplitDel (split);
	return -1;
}

/**
 * @internal
 * @brief Does all set steps but not commit
 *
 * @param split all information for iteration
 * @param parentKey to add warnings (also passed to plugins for the same reason)
 * @param [out] errorKey may point to which key caused the error or 0 otherwise
 *
 * @retval -1 on error
 * @retval 0 on success
 */
static int elektraSetPrepare(Split *split, Key *parentKey, Key **errorKey)
{
	int any_error = 0;
	for(size_t p=0; p<COMMIT_PLUGIN; ++p)
	{
		for(size_t i=0; i<split->size;i++)
		{
			int ret = 0; // last return value

			Backend *backend = split->handles[i];
			ksRewind (split->keysets[i]);
			if(backend->setplugins[p])
			{
				if(p != 0)
				{
					keySetString (parentKey,
						keyString(split->parents[i]));
				}
				keySetName (parentKey,
					keyName(split->parents[i]));
				ret = backend->setplugins[p]->kdbSet (
						backend->setplugins[p],
						split->keysets[i],
						parentKey);
				if(p == 0)
				{
					keySetString (split->parents[i],
						keyString(parentKey));
				}
			}
			if (ret == -1)
			{
				// do not
				// abort because it might
				// corrupt the KeySet
				// and leads to warnings
				// because of .tmp files not
				// found
				*errorKey =
					ksCurrent(split->keysets[i]);

				// so better keep going, but of
				// course we will not commit
				any_error = -1;
			}
		}
	}
	return any_error;
}

/**
 * @internal
 * @brief Does the commit
 *
 * @param split all information for iteration
 * @param parentKey to add warnings (also passed to plugins for the same reason)
 */
static void elektraSetCommit(Split *split, Key *parentKey)
{
	for(size_t p=COMMIT_PLUGIN; p<NR_OF_PLUGINS; ++p)
	{
		for(size_t i=0; i<split->size;i++)
		{
			int ret = 0;
			Backend *backend = split->handles[i];

			ksRewind (split->keysets[i]);
			if(backend->setplugins[p])
			{
				keySetString(parentKey,
					keyString(split->parents[i]));
				keySetName(parentKey, keyName(split->parents[i]));
				ret = backend->setplugins[p]->kdbSet (
						backend->setplugins[p],
						split->keysets[i],
						parentKey);
			}

			if (ret == -1)
			{
				ELEKTRA_ADD_WARNING(80, parentKey,
						keyName(backend->mountpoint));
			}
		}
	}
}

/**
 * @internal
 * @brief Does the rollback
 *
 * @param split all information for iteration
 * @param parentKey to add warnings (also passed to plugins for the same reason)
 */
static void elektraSetRollback(Split *split, Key *parentKey)
{
	for(size_t p=0; p<NR_OF_PLUGINS; ++p)
	{
		for(size_t i=0; i<split->size; i++)
		{
			int ret = 0;
			Backend *backend = split->handles[i];

			ksRewind (split->keysets[i]);
			if(backend->errorplugins[p])
			{
				keySetName (parentKey, keyName(split->parents[i]));
				ret = backend->errorplugins[p]->kdbError (
						backend->errorplugins[p],
						split->keysets[i],
						parentKey);
			}

			if(ret == -1)
			{
				ELEKTRA_ADD_WARNING(81, parentKey,
						keyName(backend->mountpoint));
			}
		}
	}
}


/** @brief Set keys in an atomic and universal way.
 *
 * With @p parentKey you can only store a part of the given keyset.
 * When other keys also belong to a backend, they will be used too,
 * even when they are above @p parentKey.
 *
 * @par Details:
 *
 * If you pass a parentKey without a name the whole keyset will be set
 * in an atomic way.
 *
 * Each key is checked with keyNeedSync() before being actually committed. So
 * only changed keys are updated. If no key of a backend needs to be synced
 * any affairs to backends omitted and 0 is returned.
 *
 * @par Errors:
 *
 * If some error occurs, kdbSet() will stop. In this situation the KeySet
 * internal cursor will be set on the key that generated the error.
 *
 * None of the keys are actually commited.
 *
 * You should present the error message to the user and let the user decide what
 * to do. Possible solutions are:
 * - repeat the same kdbSet (for temporary errors)
 * - remove the key and set it again (for validation or type errors)
 * - change the value and try it again (for validation errors)
 * - do a kdbGet and then (for conflicts ...)
 *   - set the same keyset again (in favour of what was set by this user)
 *   - drop the old keyset (in favour of what was set elsewhere)
 * - export the configuration into a file (for unresolvable errors)
 *
 * @par Example of how this method can be used:
 * @code
int i;
KeySet *myConfig = ksNew(0,KS_END);
Key *parentKey = keyNew("system/sw/MyApp",KEY_END);
KDB *handle = kdbOpen(pkey);

kdbGet(handle, ks, parentKey); // kdbGet needs to be called first!
KeySet *base = ksDup(ks); // save a copy of original keyset

// change the keys within ks

KeySet *ours = ksDup(ks); // save a copy of our keyset
int ret=kdbSet(handle, ks, parentKey);
while (ret == -1) // as long as we have an error
{
	// We got an error. Warn user.
	Key *problemKey = ksCurrent(ks);
	// parentKey has the errorInformation
	// problemKey is the faulty key (may be null)
	int userInput = showElektraErrorDialog (parentKey, problemKey);
	switch (userInput)
	{
	case INPUT_USE_OURS:
		kdbGet(handle, ks, parentKey); // refresh key database
		ksDel(ks);
		ks = ours;
		break;
	case INPUT_DO_MERGE:
		kdbGet(handle, ks, parentKey); // refresh key database
		KeySet * res=doElektraMerge(ours, ks, base);
		ksDel(ks);
		ks = res;
		break;
	case INPUT_USE_THEIRS:
		// should always work, we just write what we got
		// but to be sure always give the user another way
		// to exit the loop
		kdbGet(handle, ks, parentKey); // refresh key database
		break;
	...
	}
	ret=kdbSet(handle, ks, parentKey);
}

ksDel (ours);
ksDel (base);
ksDel (ks); // delete the in-memory configuration

kdbClose(handle, parentKey); // no more affairs with the key database.
keyDel(parentKey);

 * @endcode
 *
 * showElektraErrorDialog() and doElektraMerge() need to be implemented
 * by the user. For doElektraMerge a 3-way merge algorithm exists in
 * libelektra-tools.
 *
 * @pre kdbGet() must be called before kdbSet():
 *    - initially (after kdbOpen())
 *    - after conflict errors in kdbSet().
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param ks a KeySet which should contain changed keys, otherwise nothing is done
 * @param parentKey holds the information below which key keys should be set, see above
 * @retval 1 on success
 * @retval 0 if nothing had to be done
 * @retval -1 on failure
 * @see keyNeedSync()
 * @see ksNext(), ksCurrent() for iteration over the keyset
 * @see kdbOpen() and kdbGet() that must be called first
 * @see kdbClose() that must be called afterwards
 * @ingroup kdb
 */
int kdbSet(KDB *handle, KeySet *ks, Key *parentKey)
{
	if(!parentKey)
	{
		return -1;
	}

#if DEBUG && VERBOSE
	fprintf(stderr, "now in new kdbSet (%s)\n", keyName(parentKey));
#endif

	if(!handle || !ks)
	{
		ELEKTRA_SET_ERROR (37, parentKey, "handle or ks null pointer");
		return -1;
	}

	Split *split = elektraSplitNew();
	Key *initialParent = keyDup(parentKey);
	Key *errorKey = 0;


	if(elektraSplitBuildup(split, handle, parentKey) == -1)
	{
		ELEKTRA_SET_ERROR(38, parentKey, "error in elektraSplitBuildup");
		goto error;
	}

	if(elektraSplitDivide(split, handle, ks) == -1)
	{
		ELEKTRA_SET_ERROR(8, parentKey, keyName(ksCurrent(ks)));
		goto error;
	}

	if(elektraSplitSync(split) == 0)
	{
		/* No update is needed */
		keySetName (parentKey, keyName(initialParent));
		keyDel (initialParent);
		elektraSplitDel (split);
		return 0;
	}

	elektraSplitPrepare(split);

	if (elektraSetPrepare(split, parentKey, &errorKey) == -1)
	{
		goto error;
	}

	elektraSetCommit(split, parentKey);

	elektraSplitUpdateSize(split);

	keySetName(parentKey, keyName(initialParent));
	keyDel(initialParent);
	elektraSplitDel(split);
	for (size_t i=0; i<ks->size; ++i) ks->array[i]->flags = 0;

	return 1;

error:
	elektraSetRollback(split, parentKey);

	if(errorKey)
	{
		Key *found = ksLookup(ks, errorKey, 0);
		if (!found)
		{
			ELEKTRA_ADD_WARNING(82, parentKey,
					keyName(errorKey));
		}
	}

	keySetName(parentKey, keyName(initialParent));
	keyDel(initialParent);
	elektraSplitDel(split);
	return -1;
}

/**
 * @}
 */
