/**
 * @file
 *
 * @brief Methods for Key manipulation.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#if DEBUG && defined(HAVE_STDIO_H)
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

#include "elektra/kdb.h"
#include "kdbprivate.h"


/**
 * @defgroup key Key
 *
 * @brief Key is an essential class that encapsulates key @link keyname name @endlink,
 * @link keyvalue value @endlink and @link keymeta metainfo @endlink.
 *
 * To use it include:
 * @code
#include <elektra/kdb.h>
 * @endcode
 *
 * Key properties are:
 * - @link keyname Key name @endlink
 * - @link keyvalue Key value @endlink
 * - @link keymeta Key metadata @endlink, including but not limited to:
 *   - @link keyGetComment() Key comment @endlink
 *   - @link keyGetOwner() Key owner @endlink
 *   - @link keymeta UID, GID and filesystem-like mode permissions @endlink
 *   - @link keymeta Mode, change and modification times @endlink
 *
 * @par ABI
 * Due to ABI compatibility, the @p Key structure is not defined in kdb.h,
 * only declared. So you can only declare @p pointers to @p Keys in your
 * program, and allocate and free memory for them with keyNew()
 * and keyDel() respectively.
 *
 *
 * @par Reference Counting
 * Every key has its reference counter (see keyGetRef() for longer
 * explanation) that will be initialized
 * with 0, that means a subsequent call of keyDel() will delete
 * the key. If you append the key to a keyset the reference counter
 * will be incremented by one (see keyIncRef()) and the key can't be
 * deleted by a keyDel().
 *
 * @par
 * As you can imagine this refcounting allows you to put the Key in your
 * own data structures.
 * It can be a very powerful feature, e.g. if you need your own-defined
 * ordering or different Models of your configuration.
 */


/*
 * @internal
 *
 * Allocates and initializes a key
 * @returns 0 if allocation did not work, the key otherwise
 */
static Key * elektraKeyMalloc (void)
{
	Key * key = (Key *) elektraMalloc (sizeof (Key));
	if (!key) return 0;
	keyInit (key);

	return key;
}


/**
 * A practical way to fully create a Key object in one step.
 *
 * To just get a key object, simple do:
 *
 * @snippet keyNew.c Simple
 *
 * keyNew() allocates memory for a key object and keyDel() cleans
 * everything up.
 *
 * We can also give an empty key name and a KEY_END tag with the same
 * effect as before:
 *
 * @snippet keyNew.c Alternative
 *
 * But we can also give the key a proper name right from the start:
 *
 * @snippet keyNew.c With Name
 *
 * If you want the key object to contain a name, value, comment and other
 * meta info read on.
 *
 * @note When you already have a key with similar properties its
 * easier to keyDup() the key.
 *
 * You can call keyNew() in many different ways depending on the attribute tags you
 * pass as parameters. Tags are represented as #keyswitch_t values, and
 * tell keyNew() which Key attribute comes next.
 * The Key attribute tags are the following:
 * - ::KEY_VALUE \n
 *   Next parameter is a pointer to the value that will be used.
 *   If no ::KEY_BINARY was used before, a string is assumed.
 *   @snippet keyNew.c With Value
 * - ::KEY_SIZE \n
 *   Define a maximum length of the value. This is only used when setting
 *   a binary key.
 *   @snippet keyNew.c With Size
 * - ::KEY_META \n
 *   Next two parameter is a metaname and a metavalue. See keySetMeta().
 *   @snippet keyNew.c With Meta
 * - ::KEY_END \n
 *   Must be the last parameter passed to keyNew(). It is always
 *   required, unless the @p keyName is 0.
 * - ::KEY_FLAGS \n
 *   Bitwise disjunction of flags, which don't require one or more values.
 *   recommended way to set multiple flags. overrides previously defined flags.
 *   @snippet keyNew.c With Flags
 * - ::KEY_BINARY \n
 *   Allows one to change the key to a binary key.
 *   Make sure that you also pass ::KEY_SIZE before you set the value.
 *   Otherwise it will be cut off with first \\0 in the string.
 *   So this flag toggle from keySetString() to keySetBinary().
 *   If no value (nor size) is given, it will be a NULL key.
 *   @snippet keyNew.c With Binary
 * - ::KEY_CASCADING_NAME allow the name to start with /
 *   useful for ksLookup() and kdbGet() parent/lookup keys
 * - ::KEY_META_NAME allow the name to start with arbitrary namespaces
 *   useful to compare with metakeys
 *
 *
 *
 * @deprecated The flags below are deprecated and ::KEY_META should be
 * preferred. They remain some time, however, for compatibility:
 * - ::KEY_DIR \n
 *   Define that the key is a directory rather than an ordinary key.
 *   This means its executable bits in its mode are set.
 *   But even without this option the key can have subkeys.
 *   See keySetDir().
 * - ::KEY_OWNER \n
 *   Next parameter is the owner. See keySetOwner().
 * - ::KEY_UID, ::KEY_GID \n
 *   Next parameter is taken as the UID (uid_t) or GID (gid_t) that will
 *   be defined on the key.
 *   See keySetUID() and keySetGID().
 * - ::KEY_MODE \n
 *   Next parameter is taken as mode permissions (int) to the key.
 *   See keySetMode().
 *   @snippet keyNew.c With Mode
 * - ::KEY_COMMENT \n
 *   Next parameter is a comment. See keySetComment().
 *   @snippet keyNew.c With Everything
 *
 *
 *
 * @param name a valid name to the key, or NULL to get a simple
 * 	initialized, but really empty, object
 * @see keyDel()
 * @return a pointer to a new allocated and initialized Key object.
 * @retval NULL on allocation error or if an invalid @p name was passed (see keySetName()).
 * @ingroup key
 *
 */
Key * keyNew (const char * name, ...)
{
	Key * k;

	if (!name)
	{
		k = elektraKeyMalloc ();
	}
	else
	{
		va_list va;
		va_start (va, name);
		k = keyVNew (name, va);
		va_end (va);
	}

	return k;
}


/**
 * @copydoc keyNew
 *
 * @pre caller must use va_start and va_end on va
 * @param va the variadic argument list
 */
Key * keyVNew (const char * name, va_list va)
{
	Key * key = elektraKeyMalloc ();
	if (!key) return 0;
	keyVInit (key, name, va);
	return key;
}

/**
 * Return a duplicate of a key.
 *
 * Memory will be allocated as needed for dynamic properties.
 *
 * The new key will not be member of any KeySet and
 * will start with a new reference counter at 0. A
 * subsequent keyDel() will delete the key.
 *
 * @code
int f (const Key * source)
{
	Key * dup = keyDup (source);
	// work with duplicate
	keyDel (dup);
	// everything related to dup is freed
	// and source is unchanged
}
 * @endcode
 *
 * Like for a new key after keyNew() a subsequent ksAppend()
 * makes a KeySet to take care of the lifecycle of the key.
 *
 * @code
int g (const Key * source, KeySet * ks)
{
	Key * dup = keyDup (source);
	// work with duplicate
	ksAppendKey (ks, dup);
	// ksDel(ks) will also free the duplicate
	// source remains unchanged.
}
 * @endcode
 *
 * Duplication of keys should be preferred to keyNew(),
 * because data like owner can be filled with a copy
 * of the key instead of asking the environment.
 * It can also be optimized in the checks, because the keyname
 * is known to be valid.
 *
 * @param source has to be an initialized source Key
 * @retval 0 failure or on NULL pointer
 * @return a fully copy of source on success
 * @see ksAppend(), keyDel(), keyNew()
 * @ingroup key
 */
Key * keyDup (const Key * source)
{
	Key * dest = 0;

	if (!source) return 0;

	dest = elektraKeyMalloc ();
	if (!dest) return 0;

	/* Copy the struct data */
	*dest = *source;

	/* get rid of properties bound to old key */
	dest->ksReference = 0;
	dest->flags = KEY_FLAG_SYNC;

	/* prepare to set dynamic properties */
	dest->key = dest->data.v = dest->meta = 0;

	/* copy dynamic properties */
	if (keyCopy (dest, source) == -1)
	{
		keyDel (dest);
		return 0;
	}

	return dest;
}


/**
 * Copy or Clear a key.
 *
 * Most often you may prefer keyDup() which allocates
 * a new key and returns a duplication of another key.
 *
 * But when you need to copy into an existing key, e.g.
 * because it was passed by a pointer in a function
 * you can do so:
 *
 * @snippet keyCopy.c Basic Usage
 *
 * The reference counter will not be changed for
 * both keys. Affiliation to keysets
 * are also not affected.
 *
 * The metadata will be duplicated for the destination
 * key. So it will not take much additional space, even
 * with lots of metadata.
 *
 * When you pass a NULL-pointer as source the
 * data of dest will be cleaned completely
 * (except reference counter, see keyClear()) and
 * you get a fresh dest key:
 *
 * @snippet keyCopy.c Clear
 *
 * If you want to copy everything, except e.g. the value
 * you can use keyCopy() too:
 *
 * @snippet keyCopy.c Copy Without Value
 *
 * Restrain from coping everything yourself, because it will lead to
 * wrong metadata and is not able to copy empty or cascading names:
 *
 * @snippet keyCopy.c Individual Copy
 *
 *
 * @param dest the key which will be written to
 * @param source the key which should be copied
 *     or NULL to clean the destination key
 * @ingroup key
 * @retval -1 on failure when a NULL pointer
 *     was passed for dest or a dynamic property could not
 *     be written. The content will be unmodified then.
 * @retval 0 when dest was cleaned
 * @retval 1 when source was successfully copied
 * @see keyDup() to get a duplication of a key
 */
int keyCopy (Key * dest, const Key * source)
{
	if (!dest) return -1;

	if (test_bit (dest->flags, KEY_FLAG_RO_NAME) || test_bit (dest->flags, KEY_FLAG_RO_VALUE) ||
	    test_bit (dest->flags, KEY_FLAG_RO_META))
	{
		return -1;
	}

	if (!source)
	{
		keyClear (dest);
		return 0;
	}

	// remember dynamic memory to be removed
	char * destKey = dest->key;
	void * destData = dest->data.c;
	KeySet * destMeta = dest->meta;

	// duplicate dynamic properties
	if (source->key)
	{
		dest->key = elektraStrNDup (source->key, source->keySize + source->keyUSize);
		if (!dest->key) goto memerror;
	}
	else
	{
		dest->key = 0;
	}

	if (source->data.v)
	{
		dest->data.v = elektraStrNDup (source->data.v, source->dataSize);
		if (!dest->data.v) goto memerror;
	}
	else
	{
		dest->data.v = 0;
	}

	if (source->meta)
	{
		dest->meta = ksDup (source->meta);
		if (!dest->meta) goto memerror;
	}
	else
	{
		dest->meta = 0;
	}

	// successful, now do the irreversible stuff: we obviously modified dest
	set_bit (dest->flags, KEY_FLAG_SYNC);

	// copy sizes accordingly
	dest->keySize = source->keySize;
	dest->keyUSize = source->keyUSize;
	dest->dataSize = source->dataSize;

	// free old resources of destination
	if (!test_bit (dest->flags, KEY_FLAG_MMAP_KEY)) elektraFree (destKey);
	if (!test_bit (dest->flags, KEY_FLAG_MMAP_DATA)) elektraFree (destData);
	ksDel (destMeta);

	return 1;

memerror:
	elektraFree (dest->key);
	elektraFree (dest->data.v);
	ksDel (dest->meta);

	dest->key = destKey;
	dest->data.v = destData;
	dest->meta = destMeta;
	return -1;
}


/**
 * A destructor for Key objects.
 *
 * Every key created by keyNew() must be
 * deleted with keyDel().
 *
 * It is safe to delete keys which are
 * in a keyset, the number of references
 * will be returned then.
 *
 * It is save to delete a nullpointer,
 * -1 will be returned then.
 *
 * It is also save to delete a multiple
 * referenced key, nothing will happen
 * then and the reference counter will
 * be returned.
 *
 * @param key the key object to delete
 * @see keyNew(), keyIncRef(), keyGetRef()
 * @return the value of the reference counter
 *         if the key is within keyset(s)
 * @retval 0 when the key was freed
 * @retval -1 on null pointers
 * @ingroup key
 *
 */
int keyDel (Key * key)
{
	int rc;

	if (!key) return -1;

	if (key->ksReference > 0)
	{
		return key->ksReference;
	}

	int keyInMmap = test_bit (key->flags, KEY_FLAG_MMAP_STRUCT);

	rc = keyClear (key);

	if (!keyInMmap)
	{
		elektraFree (key);
	}

	return rc;
}

/**
 * Key Object Cleaner.
 *
 * Will reset all internal data.
 *
 * After this call you will receive a fresh
 * key.
 *
 * The reference counter will stay unmodified.
 *
 * @note that you might also clear() all aliases
 * with this operation.
 *
 * @code
int f (Key *k)
{
	keyClear (k);
	// you have a fresh key k here
	keySetString (k, "value");
	// the caller will get an empty key k with an value
}
 * @endcode
 *
 * @retval returns 0 on success
 * @retval -1 on null pointer
 *
 * @param key the key object to work with
 * @ingroup key
 */
int keyClear (Key * key)
{
	if (!key)
	{
		return -1;
	}

	size_t ref = 0;

	ref = key->ksReference;

	int keyStructInMmap = test_bit (key->flags, KEY_FLAG_MMAP_STRUCT);

	if (key->key && !test_bit (key->flags, KEY_FLAG_MMAP_KEY)) elektraFree (key->key);
	if (key->data.v && !test_bit (key->flags, KEY_FLAG_MMAP_DATA)) elektraFree (key->data.v);
	if (key->meta) ksDel (key->meta);

	keyInit (key);

	if (keyStructInMmap) key->flags |= KEY_FLAG_MMAP_STRUCT;

	/* Set reference properties */
	key->ksReference = ref;

	return 0;
}


/**
 * Increment the viability of a key object.
 *
 * This function is intended for applications
 * using their own reference counter for
 * key objects. With it you can increment
 * the reference and thus avoid destruction
 * of the object in a subsequent keyDel().
 *
 * The reference counter can't be incremented
 * once it reached SSIZE_MAX. In that situation
 * nothing will happen and SSIZE_MAX will be
 * returned.
 *
 * @note keyDup() will reset the references for dupped key.
 *
 * @return the value of the new reference counter
 * @retval -1 on null pointer
 * @retval SSIZE_MAX when maximum exceeded
 * @param key the key object to work with
 * @see keyGetRef() for longer explanation, keyDecRef(), keyDel()
 * @ingroup key
 */
ssize_t keyIncRef (Key * key)
{
	if (!key) return -1;

	if (key->ksReference < SSIZE_MAX)
		return ++key->ksReference;
	else
		return SSIZE_MAX;
}


/**
 * Decrement the viability of a key object.
 *
 * The references will be decremented for ksPop() or successful calls
 * of ksLookup() with the option KDB_O_POP.
 * It will also be decremented with an following keyDel() in
 * the case that an old key is replaced with another key with
 * the same name.
 *
 * The reference counter can't be decremented
 * once it reached 0. In that situation
 * nothing will happen and 0 will be
 * returned.
 *
 * @note keyDup() will reset the references for dupped key.
 *
 * @return the value of the new reference counter
 * @retval -1 on null pointer
 * @retval 0 when the key is ready to be freed
 * @param key the key object to work with
 * @see keyGetRef() for longer explanation, keyDel(), keyIncRef()
 * @ingroup key
 */
ssize_t keyDecRef (Key * key)
{
	if (!key) return -1;

	if (key->ksReference > 0)
		return --key->ksReference;
	else
		return 0;
}


/**
 * Return how many references the key has.
 *
 * The reference counting is the essential property of keys to make sure
 * that they can be put safely into data structures. E.g. if you put
 * a Key into a KeySet:
 *
 * @snippet keyNew.c Ref in KeySet
 *
 * You can even add the key to more KeySets:
 *
 * @snippet keyNew.c Ref in multiple KeySets
 *
 * If you increment only by one with keyIncRef() the same as said above
 * is valid:
 *
 * @snippet keyNew.c Ref
 *
 * or use keyIncRef() more than once:
 *
 * @snippet keyNew.c Multi Ref
 *
 * The key won't be deleted by a keyDel() as long refcounter is not 0.
 *
 * The references will be incremented on successful calls to
 * ksAppendKey() or ksAppend().
 *
 * @note keyDup() will reset the references for dupped key.
 *
 * For your own applications you can use
 * keyIncRef() and keyDecRef() for reference
 * counting, too.
 *
 * @param key the key object to work with
 * @return the number of references
 * @retval -1 on null pointer
 * @see keyIncRef() and keyDecRef()
 * @ingroup key
 **/
ssize_t keyGetRef (const Key * key)
{
	if (!key) return -1;

	return key->ksReference;
}
