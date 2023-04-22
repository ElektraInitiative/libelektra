/**
 * @file
 *
 * @brief Methods for Key manipulation.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#include "kdblogger.h"
#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <stdio.h>

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "kdb.h"
#include "kdbprivate.h"
#include <kdbassert.h>

/**
 * @defgroup key Key
 *
 * @brief Key is an essential class that encapsulates key @link keyname name @endlink,
 * @link keyvalue value @endlink and @link keymeta metainfo @endlink.
 *
 * To use it include:
 * @code
#include <kdb.h>
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
 *
 * @par Copy-On-Write
 * Keys employ copy-on-write techniques to minimize memory footprint.
 * If keys are copied or duplicated, they will point at the same name
 * and value as the source key.
 * Only if this data is changed, additional memory is allocated.
 */

/**
 * A practical way to fully create a Key object in one step.
 *
 * To just get a key object, simple do:
 *
 * @snippet keyNew.c With Name
 *
 * keyNew() allocates memory for a key object and keyDel() cleans
 * everything up.
 *
 * If you want the key object to contain a name, value, comment and other
 * meta info read on.
 *
 * @note When you already have a key with similar properties its
 * easier to keyDup() the key.
 *
 * You can call keyNew() in many different ways depending on the attribute tags you
 * pass as parameters. Tags are represented as #elektraKeyFlags values, and
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
 *
 *
 *   @snippet keyNew.c With Everything
 *
 * @pre @p name is a valid Key name
 * @pre Variable arguments are a valid combination
 * @post returns a new, fully initialized Key object with the valid Key name and all data given by variable arguments
 *
 * @param name a valid name to the key (see keySetName())
 *
 * @return a pointer to a new allocated and initialized Key object.
 * @retval NULL on allocation error or if an invalid @p name was passed (see keySetName()).
 *
 * @since 1.0.0
 * @ingroup key
 * @see keyDel() for deallocating a created Key object
 * @see keySetName() for rules about which names are considered valid
 */
Key * keyNew (const char * name, ...)
{
	if (!name) return NULL;

	va_list va;
	va_start (va, name);
	Key * k = keyVNew (name, va);
	va_end (va);

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
	if (!name) return NULL;

	Key * key = elektraCalloc (sizeof (Key));

	elektraKeyFlags action = 0;
	size_t value_size = 0;
	void * value = 0;
	void (*func) (void) = 0;
	int flags = 0;

	// flags that can be set via KEY_FLAGS
	int allFlags = KEY_BINARY | KEY_LOCK_META | KEY_LOCK_NAME | KEY_LOCK_VALUE;

	while ((action = va_arg (va, elektraKeyFlags)))
	{
		switch (action)
		{
		/* flags with an argument */
		case KEY_SIZE:
			value_size = va_arg (va, size_t);
			break;
		case KEY_VALUE:
			value = va_arg (va, void *);
			if (value_size && keyIsBinary (key))
				keySetBinary (key, value, value_size);
			else if (keyIsBinary (key))
				keySetBinary (key, value, elektraStrLen (value));
			else
				keySetString (key, value);
			break;
		case KEY_FUNC:
			func = va_arg (va, void (*) (void));
			keySetBinary (key, &func, sizeof (func));
			break;
		case KEY_META:
			value = va_arg (va, char *);
			/* First parameter is name */
			keySetMeta (key, value, va_arg (va, char *));
			break;

		/* flags without an argument */
		case KEY_FLAGS:
			flags |= (va_arg (va, int) & allFlags);
			if (test_bit (flags, KEY_BINARY)) keySetMeta (key, "binary", "");
			break;
		case KEY_BINARY:
			keySetMeta (key, "binary", ""); // FALLTHROUGH
		case KEY_LOCK_NAME:
		case KEY_LOCK_VALUE:
		case KEY_LOCK_META:
			flags |= action;
			break;
		default:
			ELEKTRA_ASSERT (0, "Unknown option " ELEKTRA_UNSIGNED_LONG_LONG_F " in keyNew", (kdb_unsigned_long_long_t) action);
			break;
		}
	}

	if (keySetName (key, name) < 0)
	{
		ELEKTRA_LOG_WARNING ("Invalid name: %s", name);
		elektraFree (key);
		return NULL;
	}

	keyLock (key, flags);
	return key;
}

/**
 * Copy or clear a key.
 *
 * Depending on the chosen @p flags keyCopy() only copies
 * certain parts of @p source into @p dest.
 *
 * * If #KEY_CP_NAME is set, the key name will be copied
 *   from @p source to @p dest.
 * * If #KEY_CP_META is set, the meta keys will be copied
 *   from @p source to @p dest.
 * * If #KEY_CP_VALUE is set, the key value will be copied
 *   from @p source to @p dest.
 *   Additionally, if @p source is a binary key (keyIsBinary()),
 *   @p dest will also be
 *   marked as binary. This means that even if #KEY_CP_META is
 *   not set, the `binary` meta key will be copied with
 *   #KEY_CP_VALUE.
 * * If #KEY_CP_STRING is set, the key value will be copied
 *   from @p source to @p dest, but only, if @p source is
 *   _not_ a binary key (keyIsBinary()). If @p source is binary,
 *   keyCopy() fails. If @p dest is binary, it will still be
 *   marked as binary after the copy.
 *   This cannot be used together with #KEY_CP_VALUE.
 *   The main purpose of #KEY_CP_STRING is for copying _into_
 *   known string keys. It ensure that you don't accidentally
 *   convert string keys into binary keys.
 *
 * There is also the shorthand #KEY_CP_ALL. It is equivalent
 * to `KEY_CP_NAME | KEY_CP_VALUE | KEY_CP_META`,
 * i.e. all key data supported by keyCopy() will be copied
 * from @p source to @p dest.
 *
 * Use this function when you need to copy into an existing key, e.g.
 * because it was passed by a pointer in a function
 * you can do so:
 *
 * @snippet keyCopy.c Basic Usage
 *
 * Most often you will want to duplicate an existing key.
 * For this purpose the alias keyDup() exists. Calling
 *
 * @snippet keyCopy.c Dup Key
 *
 * is equivalent to
 *
 * @snippet keyCopy.c Duplicate Key
 *
 * The reference counter will not be changed for both keys.
 * Affiliation to keysets are also not affected.
 *
 * Since metadata uses copy-on-write semantics there is only a
 * constant memory cost to copying metadata.
 *
 * When you pass a NULL-pointer as @p source the pieces of @p dest
 * specified by @p flags will be cleared.
 *
 * Calling `keyCopy (dest, NULL, KEY_CP_ALL)` is different from calling keyClear().
 * The key will not be fully reset, the reference counter and internal flags
 * will remain unchanged. Additionally, keyCopy() respects keyLock() state,
 * while keyClear() always works.
 *
 * @snippet keyCopy.c Clear
 *
 * @pre @p dest must be a valid Key (created with keyNew)
 * @pre @p dest must not have read-only flags set
 * @pre @p source must be a valid Key or NULL
 * @invariant Key name stays valid until delete
 * @post Value from Key source is written to Key dest
 *
 * @param dest the key which will be written to
 * @param source the key which should be copied
 *     or NULL to clear the data of @p dest
 * @param flags specifies which parts of the key should be copied
 *
 * @return @p dest
 * @retval NULL on memory allocation problems
 * @retval NULL when a part of @p dest that should be modified (e.g. name, value) was marked read-only,
 *              e.g. the name of @p dest will be read-only if @p dest is part of a KeySet
 * @retval NULL when @p dest is NULL
 * @retval NULL when both #KEY_CP_VALUE and #KEY_CP_STRING are set in @p flags
 * @retval NULL when both #KEY_CP_STRING is set in @p flags and @p source is a binary key (keyIsBinary())
 *
 * @since 0.9.5
 * @ingroup key
 * @see keyDup() for duplicating an existing Key
 */
Key * keyCopy (Key * dest, const Key * source, elektraCopyFlags flags)
{
	if (dest == NULL) return NULL;

	if (dest->hasReadOnlyName && test_bit (flags, KEY_CP_NAME)) return NULL;
	if (dest->hasReadOnlyValue && test_bit (flags, KEY_CP_VALUE)) return NULL;
	if (dest->hasReadOnlyMeta && test_bit (flags, KEY_CP_META)) return NULL;

	if (test_bit (flags, KEY_CP_STRING) && test_bit (flags, KEY_CP_VALUE)) return NULL;

	if (source == NULL)
	{
		if (test_bit (flags, KEY_CP_NAME))
		{
			keySetName (dest, "/");
		}
		if (test_bit (flags, KEY_CP_VALUE))
		{
			keySetRaw (dest, NULL, 0);
		}
		if (test_bit (flags, KEY_CP_META))
		{
			ksClear (dest->meta);
		}
		return dest;
	}

	if (test_bit (flags, KEY_CP_STRING) && keyIsBinary (source)) return NULL;

	if (source == dest) return dest;

	// remember original data of dest
	Key orig = *dest;
	if (orig.keyName != NULL) keyNameRefInc (orig.keyName);
	if (orig.keyData != NULL) keyDataRefInc (orig.keyData);

	// duplicate dynamic properties
	if (test_bit (flags, KEY_CP_NAME))
	{
		if (dest->keyName != NULL)
		{
			keyNameRefDecAndDel (dest->keyName);
			dest->keyName = NULL;
		}

		if (source->keyName != NULL)
		{
			dest->keyName = source->keyName;
			keyNameRefInc (dest->keyName);
		}
		else
		{
			keySetName (dest, "/");
		}
	}

	if (test_bit (flags, KEY_CP_STRING) || test_bit (flags, KEY_CP_VALUE))
	{
		if (dest->keyData != NULL)
		{
			keyDataRefDecAndDel (dest->keyData);
			dest->keyData = NULL;
		}

		if (source->keyData != NULL)
		{
			dest->keyData = source->keyData;
			keyDataRefInc (dest->keyData);

			if (!test_bit (flags, KEY_CP_META) && keyIsBinary (source))
			{
				keySetMeta (dest, "binary", "");
			}
		}
	}

	if (test_bit (flags, KEY_CP_META))
	{
		if (source->meta != NULL)
		{
			dest->meta = ksDup (source->meta);
			if (!dest->meta) goto memerror;
		}
		else
		{
			dest->meta = NULL;
		}
	}

	// free old resources of destination
	keyNameRefDecAndDel (orig.keyName);
	keyDataRefDecAndDel (orig.keyData);
	if (test_bit (flags, KEY_CP_META)) ksDel (orig.meta);

	return dest;

memerror:
	keyNameRefDecAndDel (dest->keyName);
	keyDataRefDecAndDel (dest->keyData);
	ksDel (dest->meta);

	*dest = orig;

	return NULL;
}

static void keyClearNameValue (Key * key)
{
	keyNameRefDecAndDel (key->keyName);
	key->keyName = NULL;

	keyDataRefDecAndDel (key->keyData);
	key->keyData = NULL;
}


/**
 * A destructor for Key objects.
 *
 * Every Key created by keyNew() must be deleted with keyDel().
 *
 * When the reference counter of @p key is non-zero, this function
 * will do nothing and simply return the current value of the
 * reference counter.
 *
 * It is therefore safe to call `keyDel (k)` on any `Key * k`.
 *
 * @post all memory related to @p key will be freed
 *
 * @param key the Key object to delete
 *
 * @retval 0 when the Key was freed
 * @retval -1 on NULL pointers
 * @return the value of the reference counter, if it was non-zero
 *
 * @since 1.0.0
 * @ingroup key
 * @see keyNew()    for creating a new Key
 * @see keyIncRef() for more information about the reference counter
 */
int keyDel (Key * key)
{
	if (key == NULL)
	{
		return -1;
	}

	if (key->refs > 0)
	{
		return key->refs;
	}

	int keyInMmap = key->isInMmap;

	keyClearNameValue (key);

	ksDel (key->meta);

	if (!keyInMmap)
	{
		elektraFree (key);
	}

	return 0;
}

/**
 * Will clear all internal data of a Key.
 *
 * After this call you will receive a fresh
 * Key - with no value, metadata or name.
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
	// you have a fresh Key k here
	keySetString (k, "value");
	// the caller will get an empty Key k with an value
}
 * @endcode
 *
 * @post @p key's name is "/"
 * @post @p key's metadata is empty
 *
 * @param key the Key that should be cleared
 *
 * @retval 0 on success
 * @retval -1 on NULL pointer
 *
 * @since 1.0.0
 * @ingroup key
 * @see keyDel() for completely deleting a Key
 */
int keyClear (Key * key)
{
	if (!key)
	{
		return -1;
	}

	size_t ref = 0;

	ref = key->refs;

	int keyStructInMmap = key->isInMmap;

	keyClearNameValue (key);

	ksDel (key->meta);

	keyInit (key);
	key->isInMmap = keyStructInMmap;

	keySetName (key, "/");

	/* Set reference properties */
	key->refs = ref;

	return 0;
}


/**
 * Increment the reference counter of a Key object.
 *
 * As long as the reference counter is non-zero, `keyDel()` operations on @p key
 * will be a no-op and return an error code.
 *
 * Elektra's system for reference counting is not based on a concept
 * of shared ownership. It is more similar to a shared lock, where the counter
 * is used to keep track of how many clients hold the lock.
 *
 * Initially, the reference counter will be 0. This is can be interpreted as
 * the lock being unlocked. When you increment the reference counter, the lock
 * becomes locked and `keyDel()` is blocked and fails. Only when the reference
 * counter is fully decremented back down to 0 again, will `keyDel()` work again.
 *
 * @note The reference counter can never exceed `UINT16_MAX - 1`. `UINT16_MAX` is
 * reserved as an error code.
 *
 * @post @p key's reference counter is > 0
 * @post @p key's reference counter is <= UINT16_MAX - 1
 *
 * @param key the Key object whose reference counter should be increased
 *
 * @return the updated value of the reference counter
 * @retval UINT16_MAX on NULL pointer
 * @retval UINT16_MAX when the reference counter already was the maximum value `UINT16_MAX - 1`,
 *         the reference counter will not be modified in this case
 *
 * @since 1.0.0
 * @ingroup key
 * @see keyGetRef() to retrieve the current reference count
 * @see keyDecRef() for decreasing the reference counter
 * @see keyDel()    for deleting a Key
 */
uint16_t keyIncRef (Key * key)
{
	if (key == NULL)
	{
		return UINT16_MAX;
	}

	if (key->refs == UINT16_MAX - 1)
	{
		return UINT16_MAX;
	}

	return ++key->refs;
}


/**
 * Decrement the reference counter of a Key object.
 *
 * As long as the reference counter is non-zero, `keyDel()` operations on @p key
 * will be a no-op and return an error code.
 *
 * @post @p key's reference counter is >= 0
 * @post @p key's reference counter is < SSIZE_MAX
 *
 * @param key the Key object whose reference counter should get decreased
 *
 * @return the updated value of the reference counter
 * @retval UINT16_MAX on NULL pointer
 * @retval 0 when the reference counter already was the minimum value 0,
 *         the reference counter will not be modified in this case
 *
 * @since 1.0.0
 * @ingroup key
 * @see keyGetRef() to retrieve the current reference count
 * @see keyIncRef() for increasing the reference counter and for a more complete
 *                  explanation of the reference counting system
 * @see keyDel()    for deleting a Key
 */
uint16_t keyDecRef (Key * key)
{
	if (key == NULL)
	{
		return UINT16_MAX;
	}

	if (key->refs == 0)
	{
		return 0;
	}

	return --key->refs;
}


/**
 * Return the current reference counter value of a Key object.
 *
 * @param key the Key whose reference counter to retrieve
 *
 * @return the value of the @p key's reference counter
 * @retval -1 on NULL pointer
 *
 * @since 1.0.0
 * @ingroup key
 * @see keyIncRef() for increasing the reference counter and for a more complete
 *                  explanation of the reference counting system
 * @see keyDecRef() for decreasing the reference counter
 **/
uint16_t keyGetRef (const Key * key)
{
	if (key == NULL)
	{
		return UINT16_MAX;
	}

	return key->refs;
}


/**
 * Permanently lock parts of a Key.
 *
 * This can be:
 * - KEY_LOCK_NAME to lock the name
 * - KEY_LOCK_VALUE to lock the value
 * - KEY_LOCK_META to lock the metadata
 *
 * To unlock the Key, duplicate it.
 *
 * It is also possible to lock the Key when it is created with
 * keyNew().
 *
 * Some data structures need to lock the Key (most likely
 * its name), so that the ordering does not get confused.
 *
 * @post keyIsLocked(key, what) == what
 *
 * @param key the Key that should be locked
 * @param what the parts of the Key that should be locked (see above)
 *
 * @return the bits that were successfully locked
 * @retval 0 if everything was locked before
 * @retval -1 if it could not be locked (NULL pointer)
 *
 * @since 1.0.0
 * @ingroup key
 * @see keyIsLocked() for checking whether a Key is locked
 * @see keyNew() for creating a new Key
 * @see keyDup() for duplicating an existing Key
 * @see ksAppendKey() appends a Key to a keyset (and locks it)
 */
int keyLock (Key * key, elektraLockFlags what)
{
	if (!key) return -1;

	int lockedBits = 0;

	if (test_bit (what, KEY_LOCK_NAME))
	{
		if (!key->hasReadOnlyName) lockedBits |= KEY_LOCK_NAME;
		key->hasReadOnlyName = true;
	}

	if (test_bit (what, KEY_LOCK_VALUE))
	{
		if (!key->hasReadOnlyValue) lockedBits |= KEY_LOCK_VALUE;
		key->hasReadOnlyValue = true;
	}

	if (test_bit (what, KEY_LOCK_META))
	{
		if (!key->hasReadOnlyMeta) lockedBits |= KEY_LOCK_META;
		key->hasReadOnlyMeta = true;
	}

	return lockedBits;
}

/**
 * Checks which parts of a Key are locked.
 *
 * @param key the Key that should be checked for locks
 * @param what the parts of the Key that should checked for locks
 *
 * @return the bits that are locked
 * @retval 0 if nothing is locked
 * @retval -1 on error (NULL pointer)
 *
 * @since 1.0.0
 * @ingroup key
 * @see keyLock() for locking a Key
 */
int keyIsLocked (const Key * key, elektraLockFlags what)
{
	if (!key) return -1;

	int locked = 0;
	if (test_bit (what, KEY_LOCK_NAME) && key->hasReadOnlyName) locked |= KEY_LOCK_NAME;
	if (test_bit (what, KEY_LOCK_VALUE) && key->hasReadOnlyValue) locked |= KEY_LOCK_VALUE;
	if (test_bit (what, KEY_LOCK_META) && key->hasReadOnlyMeta) locked |= KEY_LOCK_META;

	return locked;
}
