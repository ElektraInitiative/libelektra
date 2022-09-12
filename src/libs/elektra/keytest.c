/**
 * @file
 *
 * @brief Methods for making tests
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
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

#include "kdb.h"
#include "kdbinternal.h"
#include "kdbprivate.h"


/**
 * @defgroup keytest Methods for Making Tests
 * @ingroup key
 * @brief Methods to do various tests on Keys
 *
 * With exception of the parameters of keyCmp(), the following contract holds for all parameters of type Key:
 * @pre The Key has been properly initialized via keyNew()
 * @invariant All parts of the Key remain unchanged
 * @post All parts of the Key are unchanged
 *
 * To use them:
 * @code
#include <kdb.h>
 * @endcode
 *
 *
 */

/**
 * @internal
 *
 * Clear sync flag of a key.
 *
 * @param key the key object to work with
 * @retval -1 on null key
 * @return new flags for that key otherwise
 * @ingroup keytest
 *
 */
int elektraKeyClearSync (ElektraKey * key)
{
	if (!key) return -1;

	key->flags &= ~ELEKTRA_KEY_FLAG_SYNC;
	return key->flags;
}


/**
 * Test if a key needs to be synced to backend storage.
 *
 * If any Key modification took place the Key will be flagged
 * so that kdbSet() knows which keys were modified
 * and which not.
 *
 * After keyNew() the flag will normally be set, but after kdbGet()
 * and kdbSet() the flag will be removed. When you modify the key
 * the flag will be set again.
 *
 * In your application you can make use of that flag to know
 * if you changed something in a key after a kdbGet() or kdbSet().
 *
 * @note Note that the sync status will be updated on any change,
 * including metadata.
 *
 * @deprecated The handling of synchronization is done internally and
 * does not need to be checked by neither application nor plugins.
 *
 * @param key the Key which should be checked
 *
 * @retval 1 if @p key was changed in memory
 * @retval 0 if @p key wasn't changed
 * @retval -1 on NULL pointer
 *
 * @since 1.0.0
 * @ingroup keytest
 * @see keyNew(), keyDup() Keys need to be synced after calling those functions
 */
int elektraKeyNeedSync (const ElektraKey * key)
{
	if (!key) return -1;

	return (key->flags & ELEKTRA_KEY_FLAG_SYNC) == ELEKTRA_KEY_FLAG_SYNC;
}


int elektraKeyIsSpec (const ElektraKey * key)
{
	return elektraKeyGetNamespace (key) == ELEKTRA_NS_SPEC;
}

int elektraKeyIsProc (const ElektraKey * key)
{
	return elektraKeyGetNamespace (key) == ELEKTRA_NS_PROC;
}


int elektraKeyIsDir (const ElektraKey * key)
{
	return elektraKeyGetNamespace (key) == ELEKTRA_NS_DIR;
}


/**
 * @internal
 *
 * Check whether a key is under the @p system namespace or not
 *
 * @param key the key object to work with
 * @retval 1 if key name begins with @p system, 0 otherwise
 * @see keyIsUser(), keySetName(), keyName()
 * @ingroup keytest
 *
 */
int elektraKeyIsSystem (const ElektraKey * key)
{
	return elektraKeyGetNamespace (key) == ELEKTRA_NS_SYSTEM;
}


/**
 * @internal
 *
 * Check whether a key is under the @p user namespace or not.
 *
 * @param key the key object to work with
 * @retval 1 if key name begins with @p user, 0 otherwise
 * @see keyIsSystem(), keySetName(), keyName()
 * @ingroup keytest
 *
 */
int elektraKeyIsUser (const ElektraKey * key)
{
	return elektraKeyGetNamespace (key) == ELEKTRA_NS_USER;
}

/**
 * Check if the Key @p check is below the Key @p key or not.
 *
 * Example:
 @verbatim
 key user:/sw/app
 check user:/sw/app/key
 @endverbatim
 *
 * returns true because @p check is below @p key
 *
 * Example:
 @verbatim
 key user:/sw/app
 check user:/sw/app/folder/key
 @endverbatim
 *
 * returns also true because @p check is indirectly below @p key
 *
 * Obviously, there is no Key above a namespace (e.g. user, system, /):
 *
 @verbatim
 key *
 check user
 @endverbatim
 *
 * @param key the Key object to check against
 * @param check the Key object for which it should be checked whether it is
 * below @p key
 *
 * @retval 1 if @p check is below @p key
 * @retval 0 if it is not below or if it is the same key
 * @retval -1 if key or check is null
 *
 * @since 1.0.0
 * @ingroup keytest
 * @see keyIsDirectlyBelow() for checking whether a Key is directly below another
 * @see keyGetName(), keySetName() for getting / setting the Key's name
 *
 */

int elektraKeyIsBelow (const ElektraKey * key, const ElektraKey * check)
{
	if (key == NULL || check == NULL)
	{
		return -1;
	}

	// same key, only if namespace and size are equal
	// size alone could be equal with cascading keys
	return elektraKeyIsBelowOrSame (key, check) && elektraKeyGetUnescapedNameSize (key) != elektraKeyGetUnescapedNameSize (check) &&
	       (elektraKeyGetNamespace (key) == elektraKeyGetNamespace (check) || elektraKeyGetNamespace (check) == ELEKTRA_NS_CASCADING ||
		elektraKeyGetNamespace (key) == ELEKTRA_NS_CASCADING);
}


/**
 * Check if a key is below or same.
 *
 * @param key the key object to work with
 * @see keyIsBelow()
 */
int elektraKeyIsBelowOrSame (const ElektraKey * key, const ElektraKey * check)
{
	if (key == NULL || check == NULL)
	{
		return -1;
	}

	const char * above = elektraKeyUnescapedName (key);
	const char * below = elektraKeyUnescapedName (check);

	size_t sizeAbove = elektraKeyGetUnescapedNameSize (key);
	size_t sizeBelow = elektraKeyGetUnescapedNameSize (check);

	if ((sizeAbove == 3 && above[0] == ELEKTRA_NS_CASCADING && sizeBelow == 3 && below[0] != ELEKTRA_NS_CASCADING) ||
	    (sizeBelow == 3 && below[0] == ELEKTRA_NS_CASCADING && sizeAbove == 3 && above[0] != ELEKTRA_NS_CASCADING))
	{
		// cascading root compared to other root
		return 0;
	}


	if (sizeAbove == 3)
	{
		// root key, ignore trailing slash
		sizeAbove -= 1;
	}

	if (sizeBelow == 3)
	{
		// root key, ignore trailing slash
		sizeBelow -= 1;
	}

	if ((above[0] != ELEKTRA_NS_CASCADING && below[0] == ELEKTRA_NS_CASCADING) ||
	    (below[0] != ELEKTRA_NS_CASCADING && above[0] == ELEKTRA_NS_CASCADING))
	{
		// cascading, ignore namespaces
		++above;
		--sizeAbove;

		++below;
		--sizeBelow;
	}

	if (sizeAbove > sizeBelow)
	{
		return 0;
	}

	return memcmp (above, below, sizeAbove) == 0;
}


/**
 * Check whether the Key @p check is directly below the Key @p key.
 *
 @verbatim
Example:
key user:/sw/app
check user:/sw/app/key
 @endverbatim
*
* returns true because check is directly below key
*
 @verbatim
Example:
key user:/sw/app
check user:/sw/app/folder/key
 @endverbatim
 *
 * does not return true, because it is only indirectly below
 *
 * @param key the Key object to check against
 * @param check the Key object for which it should be checked whether it is
 * directly below @p key
 *
 * @retval 1 if @p check is directly below @p key
 * @retval 0 if @p check is not directly below @p key or if it is the same
 * @retval -1 on null pointer
 *
 * @since 1.0.0
 * @ingroup keytest
 * @see keyIsBelow() for checking whether a Key is below another
 * @see keyGetName(), keySetName() for getting / setting the Key's name
 *
 */
int elektraKeyIsDirectlyBelow (const ElektraKey * key, const ElektraKey * check)
{
	if (key == NULL || check == NULL)
	{
		return -1;
	}

	const char * above = elektraKeyUnescapedName (key);
	const char * below = elektraKeyUnescapedName (check);

	size_t sizeAbove = elektraKeyGetUnescapedNameSize (key);
	size_t sizeBelow = elektraKeyGetUnescapedNameSize (check);

	if (sizeAbove == 3)
	{
		// root key, ignore trailing slash
		sizeAbove -= 1;
	}

	if (sizeBelow == 3)
	{
		// root key, ignore trailing slash
		sizeBelow -= 1;
	}

	if ((above[0] != ELEKTRA_NS_CASCADING && below[0] == ELEKTRA_NS_CASCADING) ||
	    (below[0] != ELEKTRA_NS_CASCADING && above[0] == ELEKTRA_NS_CASCADING))
	{
		// cascading, ignore namespaces
		++above;
		--sizeAbove;

		++below;
		--sizeBelow;
	}
	if (sizeAbove >= sizeBelow)
	{
		return 0;
	}

	size_t nextPartSize = strlen (below + sizeAbove);
	return memcmp (above, below, sizeAbove) == 0 && sizeAbove + nextPartSize + 1 == sizeBelow;
}

/**
 * Check if the value of a @p key is of binary type.
 *
 * The function checks if the value of @p key is binary. Contrary to string
 * values binary values can have '\\0' inside the value and may not be
 * terminated by a null character. Their disadvantage is that you need to pass
 * their size.
 *
 * Make sure to use this function and don't test the binary type another way to
 * ensure compatibility and to write less error prone programs.
 *
 * @param key the Key to check
 *
 * @retval 1 if the value of @p key is binary
 * @retval 0 if the value of @p key is not binary
 * @retval -1 on NULL pointer
 *
 * @ingroup keytest
 * @see keyGetBinary(), keySetBinary() for getting / setting a Key's value as binary
 */
int elektraKeyIsBinary (const ElektraKey * key)
{
	if (!key) return -1;

	return elektraKeyGetMeta (key, "binary") != 0;
}


/**
 * Check if the value of @p key is of string type.
 *
 * String values are null terminated and are not allowed to have any '\\0'
 * characters inside the string.
 *
 * Make sure to use this function and don't test the string type another way to
 * ensure compatibility and to write less error prone programs.
 *
 * @param key the Key to check
 *
 * @retval 1 if the value of @p key is string
 * @retval 0 if the value of @p key is not string
 * @retval -1 on NULL pointer
 *
 * @ingroup keytest
 * @see keyGetString(), keySetString() for getting / setting a Key's value as string
 */
int elektraKeyIsString (const ElektraKey * key)
{
	if (!key) return -1;

	return elektraKeyGetMeta (key, "binary") == 0;
}


/**
 * @internal
 *
 * Compare 2 keys.
 *
 * The returned flags bit array has 1s (differ) or 0s (equal) for each key
 * meta info compared, that can be logically ORed using @c #elektraKeyFlags flags.
 * @link elektraKeyFlags::KEY_NAME KEY_NAME @endlink,
 * @link elektraKeyFlags::KEY_VALUE KEY_VALUE @endlink,
 * @link elektraKeyFlags::KEY_COMMENT KEY_COMMENT @endlink,
 * @link elektraKeyFlags::KEY_META KEY_META @endlink (will be set in addition to owner and comment),
 *
 * @par A very simple example would be
 * @code
 Key *key1, *key;
 uint32_t changes;

// omited key1 and key2 initialization and manipulation

changes=keyCompare(key1,key2);

if (changes == 0) printf("key1 and key2 are identicall\n");

if (changes & KEY_VALUE)
printf("key1 and key2 have different values\n");

 *
 * @endcode
 *
 *
 * @par Example of very powerful specific Key lookup in a KeySet:
 * @code
 Key *base = keyNew ("/sw/MyApp/something", KEY_END);
 KDB *handle = kdbOpen(base);
 KeySet *ks=ksNew(0, KS_END);
 Key *current;
 uint32_t match;
 uint32_t interests;


 kdbGet(handle, ks, base);

// we are interested only in name and/or value
interests=(KEY_NAME | KEY_VALUE);

ksRewind(ks);
while ((current=ksNext(ks))) {
match=keyCompare(current,base);

if ((~match & interests) == interests) {
printf("Key %s has same name, value, and sync status
of base key",keyName(current));
}
// continue walking in the KeySet....
}

ksDel(ks);
kdbClose (handle, base);
keyDel(base);
* @endcode
*
* @return a bit array pointing the differences
* @param key1 first key
* @param key2 second key
* @see #elektraKeyFlags
* @ingroup keytest
	*/
elektraKeyFlags elektraKeyCompare (const ElektraKey * key1, const ElektraKey * key2)
{
	if (!key1 && !key2) return 0;
	if (!key1 || !key2) return ELEKTRA_KEY_NULL;

	elektraKeyFlags ret = 0;
	ssize_t nsize1 = elektraKeyGetNameSize (key1);
	ssize_t nsize2 = elektraKeyGetNameSize (key2);
	const char * name1 = elektraKeyName (key1);
	const char * name2 = elektraKeyName (key2);
	const ElektraKey * comment1 = elektraKeyGetMeta (key1, "comment");
	const ElektraKey * comment2 = elektraKeyGetMeta (key2, "comment");
	const void * value1 = elektraKeyValue (key1);
	const void * value2 = elektraKeyValue (key2);
	ssize_t size1 = elektraKeyGetValueSize (key1);
	ssize_t size2 = elektraKeyGetValueSize (key2);

	// TODO: might be (binary) by chance
	if (strcmp (elektraKeyString (comment1), elektraKeyString (comment2))) ret |= ELEKTRA_KEY_COMMENT;

	if (elektraKeyCompareMeta (key1, key2)) ret |= ELEKTRA_KEY_META;

	if (nsize1 != nsize2)
		ret |= ELEKTRA_KEY_NAME;
	else if (!name1 || !name2)
		ret |= ELEKTRA_KEY_NAME;
	else if (strcmp (name1, name2))
		ret |= ELEKTRA_KEY_NAME;


	if (size1 != size2)
		ret |= ELEKTRA_KEY_VALUE;
	else if (!value1 || !value2)
		ret |= ELEKTRA_KEY_VALUE;
	else if (memcmp (value1, value2, size1))
		ret |= ELEKTRA_KEY_VALUE;

	// TODO: rewind metadata to previous position
	return ret;
}

/**
 * @brief Compares metadata of two keys
 *
 * @retval KEY_META if there is a difference
 * @retval 0 if metadata is identical
 */
int elektraKeyCompareMeta (const ElektraKey * k1, const ElektraKey * k2)
{
	const ElektraKey * meta1;

	ElektraKey * key1 = (ElektraKey *) k1;
	ElektraKey * key2 = (ElektraKey *) k2;

	elektraKeyRewindMeta (key1);
	elektraKeyRewindMeta (key2);
	while ((meta1 = elektraKeyNextMeta (key1)) != 0)
	{
		const ElektraKey * meta2 = elektraKeyNextMeta (key2);
		if (!meta2)
		{
			return ELEKTRA_KEY_META;
		}

		if (strcmp (elektraKeyName (meta1), elektraKeyName (meta2))) return ELEKTRA_KEY_META;
		if (strcmp (elektraKeyString (meta1), elektraKeyString (meta2))) return ELEKTRA_KEY_META;
	}

	// TODO: rewind metadata to previous position
	return 0;
}
