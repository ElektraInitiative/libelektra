/**
 * @file
 *
 * @brief Methods to do various operations on Key metadata.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

/***************************************************************************
		      keymeta.c  -  Methods for Key manipulation
			     -------------------
    begin                : Fri Sep 26 2008
    copyright            : (C) 2008 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


/**
 * @defgroup keymeta Meta Info Manipulation Methods
 * @ingroup key
 * @brief Methods to do various operations on Key metadata.
 *
 * To use them:
 * @code
#include <kdb.h>
 * @endcode
 *
 * Next to \link keyname Name (key and owner) \endlink and
 * \link keyvalue value (data and comment) \endlink there
 * is the so called meta information inside every key.
 *
 * Key meta information are an unlimited number of key/value
 * pairs strongly related to a key. It main purpose is to
 * give keys special semantics, so that plugins can treat
 * them differently.
 *
 * Metakey, as opposed to Key,
 * deliberately has following limitations:
 * - no null values
 * - no binary data
 * - no modification of references (COW)
 * - no guarantee of ordering
 *
 * ## Examples for metadata
 *
 * File system information (see stat(2) for more information):
 * - uid: the user id (positive number)
 * - gid: the group id (positive number)
 * - mode: filesystem-like mode permissions (positive octal number)
 * - atime: When was the key accessed the last time.
 * - mtime: When was the key modified the last time.
 * - ctime: When the uid, gid or mode of a key changes.
 * (times are represented through a positive number as unix timestamp)
 *
 * The comment can contain userdata which directly
 * belong to that key. The name of the meta information
 * is "comment" for a general purpose comment about
 * the key. Multi-Language comments are also supported
 * by appending [LANG] to the name.
 *
 * Validators are regular expressions which are tested
 * against the key value. The metakey "validator" can
 * hold a regular expression which will be matched
 * against.
 *
 * Types can be expressed with the meta information
 * "type".
 *
 * The relevance of the key can be tagged with a value
 * from -20 to 20. Negative numbers are the more important
 * and must be present in order to start the program.
 *
 * A version of a key may be stored with "version".
 * Its format is full.major.minor where all of these
 * are integers.
 *
 * The order inside a persistent storage can be described
 * with the tag "order" which contains a positive number.
 *
 * The metakey "app" describes to which application a
 * key belongs. It can be used to remove keys from an
 * application no longer installed.
 *
 * The metakey "path" describes where the key is physically
 * stored.
 *
 * The "owner" is the user that owns the key. It only
 * works for the user:/ hierarchy. It rather says where
 * the key is stored and says nothing about the
 * filesystem properties.
 *
 */


#include <elektra/core/keyset.h>
#include <elektra/core/key.h>
#include <internal/kdb/config.h>
#include <internal/kdbprivate.h>

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

/**
 * Get the next metadata entry of a Key
 *
 * Keys have an internal cursor. Every
 * time keyNextMeta() is called the cursor is incremented and the new current
 * Name of Meta Information is returned.
 *
 * You'll get a NULL pointer if the metadata after the end of the Key was reached.
 * On subsequent calls of keyNextMeta() it will still return the NULL pointer.
 *
 * The @p key internal cursor will be changed, so it is not const.
 *
 * @note That the resulting key is guaranteed to have a value, because
 *       meta information has no binary or null pointer semantics.
 *
 * @note You must not delete or change the returned key,
 *    use keySetMeta() if you want to delete or change it.
 *
 * @param key the Key object to work with
 *
 * @return a key containing metadata
 * @retval 0 when the last Key has been reached
 * @retval 0 when Key is a NULL pointer
 *
 * @since 1.0.0
 * @ingroup keymeta
 *
 **/
const Key * keyNextMeta (Key * key)
{
	Key * ret;
	if (!key) return 0;
	if (!key->meta) return 0;

	ret = ksNext (key->meta);

	return ret;
}


/**
 * Do a shallow copy of metadata with name @p metaName from source to dest.
 *
 * Afterwards @p source and @p dest will have the same metadata referred with
 * @p metaName. If the Key with name @p metaName doesn't exist in @p source -
 * it gets deleted in @p dest.
 *
 * For example the metadata type is copied into the
 * Key k.
 *
 * @code
void l(Key *k)
{
	// receive c
	keyCopyMeta(k, c, "type");
	// the caller will see the changed key k
	// with the metadata "type" from c
}
 * @endcode
 *
 * The main purpose of this function is for plugins or
 * applications, which want to add the same metadata to
 * n keys. When you do that keySetMeta() will
 * take n times the memory for the key. This can be a
 * considerable amount of memory for many keys with
 * some metadata for each.
 *
 * To avoid that problem you can use keyCopyAllMeta()
 * or keyCopyMeta().
 *
 * @code
void o(KeySet *ks)
{
	Key *shared = keyNew ("/", KEY_END);
	keySetMeta(shared, "shared", "this metadata should be shared among many keys");

	for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
	{
		Key * current = ksAtCursor (ks, it);
		if (needs_shared_data(current)) keyCopyMeta(current, shared, "shared");
	}
}
 * @endcode
 *
 * @pre @p dest's metadata is not read-only
 * @post keyGetMeta(source, metaName) == keyGetMeta(dest, metaName)
 *
 * @param dest the destination where the metadata should be copied to
 * @param source the key where the metadata should be copied from
 * @param metaName the name of the metadata Key which should be copied
 *
 * @retval 1 if was successfully copied
 * @retval 0 if the metadata in dest was removed too
 * @retval -1 on null pointers (source or dest)
 * @retval -1 on memory problems
 * @retval -1 if metadata is read-only
 *
 * @since 1.0.0
 * @ingroup keymeta
 *
 * @see keyCopyAllMeta() copies all metadata from @p dest to @p src
 */
int keyCopyMeta (Key * dest, const Key * source, const char * metaName)
{
	Key * ret;

	if (!source) return -1;
	if (!dest) return -1;
	if (dest->hasReadOnlyMeta) return -1;

	ret = (Key *) keyGetMeta (source, metaName);

	if (!ret)
	{
		/*Make sure that dest also does not have metaName*/
		if (dest->meta)
		{
			Key * r;
			Key * target = (Key *) keyGetMeta (dest, metaName);
			r = ksLookup (dest->meta, target, KDB_O_POP);
			if (r)
			{
				/*It was already there, so lets drop that one*/
				keyDel (r);
			}
		}
		return 0;
	}

	/*Lets have a look if the key is already inserted.*/
	if (dest->meta)
	{
		Key * r;
		r = ksLookup (dest->meta, ret, KDB_O_POP);
		if (r && r != ret)
		{
			/*It was already there, so lets drop that one*/
			keyDel (r);
		}
	}
	else
	{
		/*Create a new place for meta information.*/
		dest->meta = ksNew (0, KS_END);
		if (!dest->meta)
		{
			return -1;
		}
	}

	// now we can simply append that key
	ksAppendKey (dest->meta, ret);

	return 1;
}

/**
 * Do a shallow copy of all metadata from source to dest.
 *
 * The key dest will additionally have all metadata
 * the source had.
 * Metadata not present in source will not be changed.
 * Metadata which was present in source and dest will
 * be overwritten.
 * If the @p dest Key is read-only it will not be changed.
 *
 * For example the metadata type is copied into the
 * Key k:
 *
 * @snippet keyMeta.c Basic Copy All
 *
 * The main purpose of this function is for plugins or
 * applications which want to add the same metadata to
 * n keys. When you do that with keySetMeta() it will
 * take n times the memory for the key. This can be
 * considerable amount of memory for many keys with
 * some metadata for each.
 *
 * To avoid that problem you can use keyCopyAllMeta()
 * or keyCopyMeta():
 *
 * @snippet keyMeta.c Shared Meta All
 *
 * @pre @p dest's metadata is not read-only
 * @post for every metaName present in source: keyGetMeta(source, metaName) == keyGetMeta(dest, metaName)
 *
 * @param dest the destination where the metadata should be copied too
 * @param source the key where the metadata should be copied from
 *
 * @retval 1 if metadata was successfully copied
 * @retval 0 if source did not have any metadata
 * @retval -1 on null pointer of dest or source
 * @retval -1 on memory problems
 *
 * @since 1.0.0
 * @ingroup keymeta
 * @see keyCopyMeta() for copying one metadata Key from @p dest to @p source
 */
int keyCopyAllMeta (Key * dest, const Key * source)
{
	if (!source) return -1;
	if (!dest) return -1;
	if (dest->hasReadOnlyMeta) return -1;

	if (ksGetSize (source->meta) > 0)
	{
		/*Make sure that dest also does not have metaName*/
		if (dest->meta)
		{
			ksAppend (dest->meta, source->meta);
		}
		else
		{
			dest->meta = ksDup (source->meta);
		}
		return 1;
	}

	return 0;
}

/**
 * Returns the Key for a metadata entry with name @p metaName.
 *
 * You are not allowed to modify the resulting key.
 *
 * If @p metaName does not start with 'meta:/',
 * it will be prefixed with 'meta:/'.
 *
 * @code
Key metaData = keyGetMeta(k, "type")
// keyType == "boolean"
char keyType[] = keyValue(metaData)
 * @endcode
 *
 * @note You must not delete or change the returned key,
 *    use keySetMeta() if you want to delete or change it.
 *
 * @pre @p key contains metadata
 * @pre @p metaName is prefixed with "meta:/"
 *
 * @param key the Key from which to get metadata
 * @param metaName the name of the meta information you want the Key from.
 *
 * @return value of meta-information if meta-information is found
 * @retval 0 if key or metaName is NULL
 * @retval 0 if no such metaName is found
 *
 * @since 1.0.0
 * @ingroup keymeta
 * @see keySetMeta() for setting metadata
 * @see keyMeta() for getting the KeySet containing metadata
 **/
const Key * keyGetMeta (const Key * key, const char * metaName)
{
	Key * ret;
	Key * search;

	if (!key) return 0;
	if (!metaName) return 0;
	if (!key->meta) return 0;

	if (strncmp (metaName, "meta:/", sizeof ("meta:/") - 1) == 0)
	{
		search = keyNew (metaName, KEY_END);
	}
	else
	{
		search = keyNew ("meta:/", KEY_END);
		keyAddName (search, metaName);
	}

	ret = ksLookup (key->meta, search, 0);

	keyDel (search);

	return ret;
}


/**
 * Set a new metadata Key
 *
 * Will set a new metadata pair with name @p metaName and value
 * @p newMetaString.
 *
 * Will add a new metadata Key, if @p metaName was unused until now.
 *
 * It will modify an existing Pair of metadata if
 * @p metaName was already present.
 *
 * It will remove a metadata Key if @p newMetaString is 0.
 *
 * If @p metaName does not start with 'meta:/',
 * it will be prefixed with 'meta:/'.
 *
 * @pre @p metaName is prefixed with "meta:/"
 * @pre @p key's metadata is not read-only
 * @post The value in @p key's metadata Keyset for @p metaName is @p newMetaString
 *
 * @param key Key whose metadata should be set
 * @param metaName name of the metadata Key that should be set
 * @param newMetaString new value for the metadata Key
 *
 * @return size (>0) of @p newMetaString if metadata has been
 *         successfully added
 * @retval 0 if the meta-information for metaName was removed
 * @retval -1 if key or metaName is 0
 * @retval -1 if system is out of memory
 * @retval -1 if @p metaName is not a valid metadata name
 *
 * @since 1.0.0
 * @ingroup keymeta
 * @see keyGetMeta() for getting the value of a metadata Key
 * @see keyMeta() for getting the KeySet containing metadata
 **/
ssize_t keySetMeta (Key * key, const char * metaName, const char * newMetaString)
{
	Key * toSet;
	ssize_t metaNameSize;
	ssize_t metaStringSize = 0;

	if (!key) return -1;
	if (key->hasReadOnlyMeta) return -1;
	if (!metaName) return -1;
	metaNameSize = elektraStrLen (metaName);
	if (metaNameSize == -1) return -1;
	if (newMetaString) metaStringSize = elektraStrLen (newMetaString);

	// optimization: we have nothing and want to remove something:
	if (!key->meta && !newMetaString) return 0;

	if (strncmp (metaName, "meta:/", sizeof ("meta:/") - 1) == 0)
	{
		toSet = keyNew (metaName, KEY_END);
	}
	else
	{
		toSet = keyNew ("meta:/", KEY_END);
		keyAddName (toSet, metaName);
	}
	if (!toSet) return -1;

	/*Lets have a look if the key is already inserted.*/
	if (key->meta)
	{
		Key * ret;
		ret = ksLookup (key->meta, toSet, KDB_O_POP);
		if (ret)
		{
			/*It was already there, so lets drop that one*/
			keyDel (ret);
			key->needsSync = true;
		}
	}

	if (newMetaString != NULL)
	{
		/*Add the meta information to the key*/
		keySetRaw (toSet, newMetaString, metaStringSize);
	}
	else
	{
		/*The request is to remove the meta string.
		  So simply drop it.*/
		keyDel (toSet);
		return 0;
	}

	if (!key->meta)
	{
		/*Create a new place for meta information.*/
		key->meta = ksNew (0, KS_END);
		if (!key->meta)
		{
			keyDel (toSet);
			return -1;
		}
	}

	toSet->hasReadOnlyName = true;
	toSet->hasReadOnlyValue = true;
	toSet->hasReadOnlyMeta = true;

	ksAppendKey (key->meta, toSet);
	key->needsSync = true;
	return metaStringSize;
}

/**
 * Returns the KeySet holding the given Key's metadata
 *
 * Use keySetMeta() to populate the metadata KeySet of a Key.
 *
 * @snippet keyMetaKeySet.c Basic keyMeta
 *
 * Iterate the returned metadata KeySet like any other KeySet.
 *
 * @snippet keyMetaKeySet.c Iterate keyMeta
 *
 * Use ksLookup() or keyGetMeta() to retrieve a single value for a given Key.
 *
 * @snippet keyMetaKeySet.c Lookup keyMeta
 *
 * @note You are not allowed to modify the name of KeySet's Keys or delete them.
 * @note You must not delete the returned KeySet.
 * @note Adding a key with metadata to the KeySet is an error.
 *
 * @post for the returned KeySet ks: keyGetMeta(key, metaName) ==
 * ksLookupByName(ks, metaName)
 * @post @p key contains a KeySet for the metadata
 *
 * @param key the Key from which to get the metadata KeySet
 *
 * @return the KeySet holding the metadata
 * @retval 0 if the Key is 0
 * @retval 0 if the Key has no metadata
 *
 * @since 1.0.0
 * @ingroup keymeta
 * @see keySetMeta() for setting a metadata Key
 * @see keyGetMeta() for getting a metadata Key
 **/
KeySet * keyMeta (Key * key)
{
	if (!key) return 0;
	if (!key->meta) key->meta = ksNew (0, KS_END);

	return key->meta;
}
