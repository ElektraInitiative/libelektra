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
#include <elektra/kdb.h>
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
 * works for the user/ hierarchy. It rather says where
 * the key is stored and says nothing about the
 * filesystem properties.
 *
 */


#include <elektra/kdb.h>
#include <kdbconfig.h>
#include <kdbprivate.h>

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


/**Rewind the internal iterator to first metadata.
 *
 * Use it to set the cursor to the beginning of the Key Meta Infos.
 * keyCurrentMeta() will then always return NULL afterwards. So
 * you want to keyNextMeta() first.
 *
 * @code
Key *key;
const Key *meta;

keyRewindMeta (key);
while ((meta = keyNextMeta (key))!=0)
{
	printf ("name: %s, value: %s", keyName(meta), keyString(meta));
}
 * @endcode
 *
 * @param key the key object to work with
 * @retval 0 on success
 * @retval 0 if there is no meta information for that key
 *         (keyNextMeta() will always return 0 in that case)
 * @retval -1 on NULL pointer
 * @see keyNextMeta(), keyCurrentMeta()
 * @see ksRewind() for pedant in iterator interface of KeySet
 * @ingroup keymeta
 **/
int keyRewindMeta (Key * key)
{
	if (!key) return -1;
	if (!key->meta) return 0;

	return ksRewind (key->meta);
}

/** Iterate to the next meta information.
 *
 * Keys have an internal cursor that can be reset with keyRewindMeta(). Every
 * time keyNextMeta() is called the cursor is incremented and the new current
 * Name of Meta Information is returned.
 *
 * You'll get a NULL pointer if the meta information after the end of the Key was reached.
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
 * @param key the key object to work with
 * @return a key representing meta information
 * @retval 0 when the end is reached
 * @retval 0 on NULL pointer
 *
 * @see ksNext() for pedant in iterator interface of KeySet
 * @ingroup keymeta
 **/
const Key * keyNextMeta (Key * key)
{
	Key * ret;
	if (!key) return 0;
	if (!key->meta) return 0;

	ret = ksNext (key->meta);

	return ret;
}

/**Returns the value of a meta-information which is current.
 *
 * The pointer is NULL if you reached the end or after
 * ksRewind().
 *
 * @note You must not delete or change the returned key,
 *    use keySetMeta() if you want to delete or change it.
 *
 * @param key the key object to work with
 * @return a buffer to the value pointed by @p key's cursor
 * @retval 0 on NULL pointer
 * @see keyNextMeta(), keyRewindMeta()
 *
 * @see ksCurrent() for pedant in iterator interface of KeySet
 * @ingroup keymeta
 **/
const Key * keyCurrentMeta (const Key * key)
{
	Key * ret;
	if (!key) return 0;
	if (!key->meta) return 0;

	ret = ksCurrent (key->meta);

	return ret;
}

/**Do a shallow copy of metadata from source to dest.
 *
 * The key dest will have the same metadata referred with
 * metaName afterwards then source.
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
 * applications which want to add the same metadata to
 * n keys. When you do that with keySetMeta() it will
 * take n times the memory for the key. This can be
 * considerable amount of memory for many keys with
 * some metadata for each.
 *
 * To avoid that problem you can use keyCopyAllMeta()
 * or keyCopyMeta().
 *
 * @code
void o(KeySet *ks)
{
	Key *current;
	Key *shared = keyNew (0);
	keySetMeta(shared, "shared", "this metadata should be shared among many keys");

	ksRewind(ks);
	while ((current = ksNext(ks)) != 0)
	{
		if (needs_shared_data(current)) keyCopyMeta(current, shared, "shared");
	}
}
 * @endcode
 *
 * @post keyGetMeta(source, metaName) == keyGetMeta(dest, metaName)
 *
 * @retval 1 if was successfully copied
 * @retval 0 if the metadata in dest was removed too
 * @retval -1 on null pointers (source or dest)
 * @retval -1 on memory problems
 * @param dest the destination where the metadata should be copied too
 * @param source the key where the metadata should be copied from
 * @param metaName the name of the metadata which should be copied
 * @ingroup keymeta
 */
int keyCopyMeta (Key * dest, const Key * source, const char * metaName)
{
	Key * ret;

	if (!source) return -1;
	if (!dest) return -1;
	if (dest->flags & KEY_FLAG_RO_META) return -1;

	ret = (Key *) keyGetMeta (source, metaName);

	if (!ret)
	{
		/*Make sure that dest also does not have metaName*/
		if (dest->meta)
		{
			Key * r;
			r = ksLookup (dest->meta, ret, KDB_O_POP);
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

/**Do a shallow copy of all metadata from source to dest.
 *
 * The key dest will additionally have all metadata
 * the source had.
 * Meta data not present in source will not be changed.
 * Meta data which was present in source and dest will
 * be overwritten.
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
 * @post for every metaName present in source: keyGetMeta(source, metaName) == keyGetMeta(dest, metaName)
 *
 * @retval 1 if was successfully copied
 * @retval 0 if source did not have any metadata
 * @retval -1 on null pointer of dest or source
 * @retval -1 on memory problems
 * @param dest the destination where the metadata should be copied too
 * @param source the key where the metadata should be copied from
 * @ingroup keymeta
 */
int keyCopyAllMeta (Key * dest, const Key * source)
{
	if (!source) return -1;
	if (!dest) return -1;
	if (dest->flags & KEY_FLAG_RO_META) return -1;

	if (source->meta)
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

/** Returns the value of a meta-information given by name.
 *
 * You are not allowed to modify the resulting key.
 *
 * @code
int f(Key *k)
{
	if (!strcmp(keyValue(keyGetMeta(k, "type")), "boolean"))
	{
		// the type of the key is boolean
	}
}
 * @endcode
 *
 * @note You must not delete or change the returned key,
 *    use keySetMeta() if you want to delete or change it.
 *
 * @param key the key object to work with
 * @param metaName the name of the meta information you want the value from
 * @retval 0 if the key or metaName is 0
 * @retval 0 if no such metaName is found
 * @return value of meta-information if meta-information is found
 * @see keySetMeta()
 * @ingroup keymeta
 **/
const Key * keyGetMeta (const Key * key, const char * metaName)
{
	Key * ret;
	Key * search;

	if (!key) return 0;
	if (!metaName) return 0;
	if (!key->meta) return 0;

	search = keyNew (0);
	elektraKeySetName (search, metaName, KEY_META_NAME | KEY_EMPTY_NAME);

	ret = ksLookup (key->meta, search, 0);

	keyDel (search);

	return ret;
}


/**Set a new meta-information.
 *
 * Will set a new meta-information pair consisting of
 * metaName and newMetaString.
 *
 * Will add a new Pair for meta-information if metaName was
 * not added up to now.
 *
 * It will modify an existing Pair of meta-information if the
 * the metaName was inserted already.
 *
 * It will remove a meta information if newMetaString is 0.
 *
 * @param key the key object to work with
 * @param metaName the name of the meta information where you
 *                 want to change the value
 * @param newMetaString the new value for the meta information
 * @retval -1 on error if key or metaName is 0, out of memory
 *         or names are not valid
 * @retval 0 if the meta-information for metaName was removed
 * @return size (>0) of newMetaString if meta-information was
 *         successfully added
 * @see keyGetMeta()
 * @ingroup keymeta
 **/
ssize_t keySetMeta (Key * key, const char * metaName, const char * newMetaString)
{
	Key * toSet;
	char * metaStringDup;
	ssize_t metaNameSize;
	ssize_t metaStringSize = 0;

	if (!key) return -1;
	if (key->flags & KEY_FLAG_RO_META) return -1;
	if (!metaName) return -1;
	metaNameSize = elektraStrLen (metaName);
	if (metaNameSize == -1) return -1;
	if (newMetaString) metaStringSize = elektraStrLen (newMetaString);

	// optimization: we have nothing and want to remove something:
	if (!key->meta && !newMetaString) return 0;

	toSet = keyNew (0);
	if (!toSet) return -1;

	elektraKeySetName (toSet, metaName, KEY_META_NAME | KEY_EMPTY_NAME);

	/*Lets have a look if the key is already inserted.*/
	if (key->meta)
	{
		Key * ret;
		ret = ksLookup (key->meta, toSet, KDB_O_POP);
		if (ret)
		{
			/*It was already there, so lets drop that one*/
			keyDel (ret);
			key->flags |= KEY_FLAG_SYNC;
		}
	}

	if (newMetaString)
	{
		/*Add the meta information to the key*/
		metaStringDup = elektraStrNDup (newMetaString, metaStringSize);
		if (!metaStringDup)
		{
			// TODO: actually we might already have changed
			// the key
			keyDel (toSet);
			return -1;
		}

		if (toSet->data.v && !test_bit (toSet->flags, KEY_FLAG_MMAP_DATA)) elektraFree (toSet->data.v);
		clear_bit (toSet->flags, (keyflag_t) KEY_FLAG_MMAP_DATA);
		toSet->data.c = metaStringDup;
		toSet->dataSize = metaStringSize;
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

	set_bit (toSet->flags, KEY_FLAG_RO_NAME);
	set_bit (toSet->flags, KEY_FLAG_RO_VALUE);
	set_bit (toSet->flags, KEY_FLAG_RO_META);

	ksAppendKey (key->meta, toSet);
	key->flags |= KEY_FLAG_SYNC;
	return metaStringSize;
}
