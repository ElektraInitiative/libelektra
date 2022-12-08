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

#include <elektra/kdb.h>
#include "kdbinternal.h"
#include <elektra/kdbprivate.h>


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
#include <elektra/kdb.h>
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
 * @ingroup keytest
 *
 */
void keyClearSync (Key * key)
{
	if (!key) return;

	key->needsSync = false;
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
int keyNeedSync (const Key * key)
{
	if (!key) return -1;

	return key->needsSync ? 1 : 0;
}


int keyIsSpec (const Key * key)
{
	return keyGetNamespace (key) == KEY_NS_SPEC;
}

int keyIsProc (const Key * key)
{
	return keyGetNamespace (key) == KEY_NS_PROC;
}


int keyIsDir (const Key * key)
{
	return keyGetNamespace (key) == KEY_NS_DIR;
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
int keyIsSystem (const Key * key)
{
	return keyGetNamespace (key) == KEY_NS_SYSTEM;
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
int keyIsUser (const Key * key)
{
	return keyGetNamespace (key) == KEY_NS_USER;
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

int keyIsBelow (const Key * key, const Key * check)
{
	if (key == NULL || check == NULL)
	{
		return -1;
	}

	// same key, only if namespace and size are equal
	// size alone could be equal with cascading keys
	return keyIsBelowOrSame (key, check) && keyGetUnescapedNameSize (key) != keyGetUnescapedNameSize (check) &&
	       (keyGetNamespace (key) == keyGetNamespace (check) || keyGetNamespace (check) == KEY_NS_CASCADING ||
		keyGetNamespace (key) == KEY_NS_CASCADING);
}


/**
 * Check if a key is below or same.
 *
 * @param key the key object to work with
 * @see keyIsBelow()
 */
int keyIsBelowOrSame (const Key * key, const Key * check)
{
	if (key == NULL || check == NULL)
	{
		return -1;
	}

	const char * above = keyUnescapedName (key);
	const char * below = keyUnescapedName (check);

	size_t sizeAbove = keyGetUnescapedNameSize (key);
	size_t sizeBelow = keyGetUnescapedNameSize (check);

	if ((sizeAbove == 3 && above[0] == KEY_NS_CASCADING && sizeBelow == 3 && below[0] != KEY_NS_CASCADING) ||
	    (sizeBelow == 3 && below[0] == KEY_NS_CASCADING && sizeAbove == 3 && above[0] != KEY_NS_CASCADING))
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

	if ((above[0] != KEY_NS_CASCADING && below[0] == KEY_NS_CASCADING) ||
	    (below[0] != KEY_NS_CASCADING && above[0] == KEY_NS_CASCADING))
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
int keyIsDirectlyBelow (const Key * key, const Key * check)
{
	if (key == NULL || check == NULL)
	{
		return -1;
	}

	const char * above = keyUnescapedName (key);
	const char * below = keyUnescapedName (check);

	size_t sizeAbove = keyGetUnescapedNameSize (key);
	size_t sizeBelow = keyGetUnescapedNameSize (check);

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

	if ((above[0] != KEY_NS_CASCADING && below[0] == KEY_NS_CASCADING) ||
	    (below[0] != KEY_NS_CASCADING && above[0] == KEY_NS_CASCADING))
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
int keyIsBinary (const Key * key)
{
	if (!key) return -1;

	return keyGetMeta (key, "binary") != 0;
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
int keyIsString (const Key * key)
{
	if (!key) return -1;

	return keyGetMeta (key, "binary") == 0;
}
