/**
 * @file
 *
 * @brief Methods for Key name manipulation.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

/** @class doxygenNamespaces
 *
 * @brief .
 *
 * .
 *
 * - @p spec/something for specification of other keys.
 * - @p proc/something for in-memory keys, e.g. commandline.
 * - @p dir/something for dir keys in current working directory
 * - @p system/something for system keys in /etc or /
 * - @p user/something for user keys in home directory
 * - @p user:username/something for other users (deprecated: kdbGet() + kdbSet() currently unsupported)
 * - @p /something for cascading keys (actually refers to one of the above, see also ksLookup())
 *
 */


/**
 * @defgroup keyname Name Manipulation Methods
 * @ingroup key
 * @brief Methods to do various operations on Key names.
 *
 * To use them:
 * @code
#include <kdb.h>
* @endcode
 *
 * These functions make it easier for C programmers to work with key names.
 *
 *
 * @par Terminology of Key Names
 * - A *key name* (see keySetName() and keyName()) defines the
 *   place of a key within the key database.
 *   To be unique, it is always absolute and canonical.
 * - Key names are composed out of many *key name parts* split by a
 *   separator. These *key name parts* do not contain an unescaped
 *   separator.
 * - A *key base name* (see keySetBaseName() and keyAddBaseName()) is
 *   the last part of the key name.
 * - A *C-String* is a null terminated sequence of characters.
 *   So \\0 (null-character) must not occur within a C-String.
 *
 * @par Namespaces
 * A namespace denotes the place the key comes from:
 *
 * @copydetails doxygenNamespaces
 *
 *
 * @note The rules are currently not formally specified and are subject
 * of change in the next major release.
 * So, always prefer:
 * - To use keySetName() and keyAddName() to get the canonified version of the keyname
 * - To use keySetBaseName() and keyAddBaseName() to get an escaped key
 *   name part.
 * - Not to escape or canonify with your own algorithms!
 * - To use keyUnescapedName() and keyBaseName() to have access to the
 *   key name without escape sequences (key name parts are null
 *   terminated)
 * - Not to unescape the strings yourself!
 *
 *
 * @par Syntax for Key Names
 * Key names and key name parts have following goals:
 * - The C-String passed to keySetName() and keyAddName() may be any
 *   C-String.
 * - The *key name parts* (e.g. keySetBaseName(), keyBaseName()) may
 *   be any C-String.
 * Escaping is needed to achieve both goals.
 *
 *
 * @par Semantics for Key Name Parts
 * - \% denotes an empty key name part.
 *
 *
 * @par Canonicalization for Key Names
 * - / (slash) is the separator between key name parts.
 * - // is shortened to /
 * - trailing / (slashes) are removed
 * - . (dot) and .. (dot-dot) is removed in an canonical key name, with
 *   following rules:
 *   - /./ is shortened to /
 *   - _/../ is shortened to _
 *
 *
 * @par Conventions for key names
 * - Key name parts starting with \# are array elements.
 *   Then only _ (underscore) followed by 0-9 is allowed.
 *   So we have the regular expression #[_]*[0-9]+ with the further
 *   limitation that the number of _ is defined by the number of
 *   digits-1.
 * - Key name parts starting with _ are reserved for special purposes
 *   (if you use this within a plugin you still have to make sure _ is
 *   escaped properly)
 * - Key name parts starting with @ are reserved for special purposes
 *   (if you use this within a plugin you still have to make sure @ is
 *   escaped properly)
 * - If any key name part starts with . (dot) it means the key is
 *   inactive, see keyIsInactive().
 *
 *
 * @par Escaping rules
 * - \\ (backslash) is the escape character for the situations as
 *   described here (and only these).
 *   The \\ character must only be escaped, when one of the following
 *   rules apply.
 * - Stray escape characters are only possible in the end of the string.
 * - \\/ allows one to escape / (any uneven number of \\).
 *   Does not introduce a new part.
 * - Any uneven number N of \\ before / allows you to escape / with the
 *   N/2 of \\ prefixed.
 *   Does not introduce a new part.
 * - \\\\/ allows one to use \\ as character before / and introduces a new
 *   part.
 * - Any even number N of \\ before / allows you to have N/2 of \\
 *   prefixed before a / which introduces a new part.
 * - Use \\. and \\.. if you want your key name part to represent . and ..
 * - \\\\. and \\\\.. allows us to use \\ as character before . and .. (and so on)
 * - Use \\% if you want your key name part to start with \% (and does
 *   not represent an empty name)
 * - Using \\\\% allows one to use \\ as character before \% (and so on)

 *
 *
 * @par Semantics for Key Name Specifications
 * - _ denotes that the key name part is
 *   arbitrary (syntax as described above).
 * - \# denotes that the key name part
 *   has array syntax.
 * - names surrounded by \% (e.g. \%profile\%)
 *   denotes a placeholder.
 *
 *
 * @{
 */


#include "kdbprivate.h"
#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <kdbassert.h>

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <ctype.h>

#include "kdb.h"
#include "kdbhelper.h"
#include "kdbinternal.h"


/*******************************************
 *    General name manipulation methods    *
 *******************************************/


/**
 * Returns a pointer to the abbreviated real internal @p key name.
 *
 * This is a much more efficient version of keyGetName() and can use
 * it if you are responsible enough to not mess up things. You are not allowed
 * to change anything in the returned array. The content of that string
 * may change after keySetName() and similar functions. If you need a copy of the name,
 * consider using keyGetName().
 *
 * The name will be without owner, see keyGetFullName() if
 * you need the name with its owner.
 *
 * @retval "" when there is no keyName. The reason is
 * @code
key=keyNew(0);
keySetName(key,"");
keyName(key); // you would expect "" here
keyDel(key);
 * @endcode
 *
 * Valid key names are:
 *
 * @copydetails doxygenNamespaces
 *
 * @note Note that the Key structure keeps its own size field that is calculated
 * by library internal calls, so to avoid inconsistencies, you
 * must never use the pointer returned by keyName() method to set a new
 * value. Use keySetName() instead.
 *
 * @param key the key object to work with
 * @return a pointer to the keyname which must not be changed.
 * @retval "" when there is no (an empty) keyname
 * @retval 0 on NULL pointer
 * @see keyGetNameSize() for the string length
 * @see keyGetFullName(), keyGetFullNameSize() to get the full name
 * @see keyGetName() as alternative to get a copy
 * @see keyOwner() to get a pointer to owner
 * @see keyUnescapedName to get an unescaped key name
 * @ingroup keyname
 */
const char * keyName (const Key * key)
{
	if (!key) return 0;

	return key->key == NULL ? "" : key->key;
}

/**
 * Bytes needed to store the key name without owner.
 *
 * For an empty key name you need one byte to store the ending NULL.
 * For that reason 1 is returned.
 *
 * @param key the key object to work with
 * @return number of bytes needed, including ending NULL, to store key name
 * 	without owner
 * @retval 1 if there is is no key Name
 * @retval -1 on NULL pointer
 * @see keyGetName(), keyGetFullNameSize()
 * @see keyGetUnescapedNameSize to get size of unescaped name
 * @ingroup keyname
 */
ssize_t keyGetNameSize (const Key * key)
{
	if (!key) return -1;

	// TODO (kodebach): change to return size_t
	return key->keySize == 0 ? 1 : key->keySize;
}


/**
 * @brief Returns a keyname which is null separated and does not use backslash for escaping
 *
 * Slashes are replaced with null bytes.
 * So cascading keys start with a null byte.
 * Otherwise escaped characters, e.g. non-hierarchy slash, will be unescaped.
 *
 * This name is essential if you want to iterate over parts of the key
 * name, want to compare keynames and want to check relations of keys in
 * the hierarchy.
 *
 * @param key the object to work with
 *
 * @see keyGetUnescapedNameSize()
 * @see keyName() for escaped variant
 * @retval 0 on null pointers
 * @retval "" if no name
 * @return the name in its unescaped form
 */
const void * keyUnescapedName (const Key * key)
{
	if (!key) return 0;
	return key->ukey == NULL ? "" : key->ukey;
}


/**
 * @brief return size of unescaped name with embedded and terminating null characters
 *
 * @param key the object to work with
 *
 * @see keyUnescapedName()
 * @see keyGetNameSize() for size of escaped variant
 * @retval -1 on null pointer
 * @retval 0 if no name
 */
ssize_t keyGetUnescapedNameSize (const Key * key)
{
	if (!key) return -1;

	// TODO (kodebach): change to return size_t
	return key->keyUSize == 0 ? 1 : key->keyUSize;
}


/**
 * Get abbreviated key name (without owner name).
 *
 * When there is not enough space to write the name,
 * nothing will be written and -1 will be returned.
 *
 * maxSize is limited to SSIZE_MAX. When this value
 * is exceeded -1 will be returned. The reason for that
 * is that any value higher is just a negative return
 * value passed by accident. Of course elektraMalloc is not
 * as failure tolerant and will try to allocate.
 *
 * @code
char *getBack = elektraMalloc (keyGetNameSize(key));
keyGetName(key, getBack, keyGetNameSize(key));
 * @endcode
 *
 * @return number of bytes written to @p returnedName
 * @retval 1 when only a null was written
 * @retval -1 when keyname is longer then maxSize or 0 or any NULL pointer
 * @param key the key object to work with
 * @param returnedName pre-allocated memory to write the key name
 * @param maxSize maximum number of bytes that will fit in returnedName, including the final NULL
 * @see keyGetNameSize(), keyGetFullName(), keyGetFullNameSize()
 * @ingroup keyname
 */
ssize_t keyGetName (const Key * key, char * returnedName, size_t maxSize)
{
	// TODO (kodebach): change to return size_t
	if (!key) return -1;

	if (!returnedName) return -1;

	if (!maxSize) return -1;

	if (maxSize > SSIZE_MAX) return -1;

	if (!key->key)
	{
		/*errno=KDB_ERR_NOKEY;*/
		returnedName[0] = 0;
		return 1;
	}

	if (key->keySize > maxSize)
	{
		/*errno=KDB_ERR_TRUNC;*/
		return -1;
	}

	strncpy (returnedName, key->key, maxSize); // TODO (kodebach): memcpy

	return key->keySize;
}

// TODO (kodebach): document
size_t keyGetUnescapedName (const Key * key, char * returnedName, size_t maxSize)
{
	if (!key) return 0;
	if (!returnedName) return 0;

	if (!key->ukey)
	{
		returnedName[0] = 0;
		return 1;
	}

	if (key->keySize > maxSize)
	{
		return -1;
	}

	memcpy (returnedName, key->ukey, maxSize);

	return key->keySize;
}


/*static void elektraHandleUserName (Key * key, const char * newName) // TODO (kodebach): needed??
{
	const size_t userLength = sizeof ("user");
	key->keyUSize = key->keySize = userLength;

	const char delim = newName[userLength - 1];
	// no owner, we are finished
	if (delim == '/' || delim == '\0') return;
	ELEKTRA_ASSERT (delim == ':', "delimiter in user-name not `:' but `%c'", delim);

	// handle owner (compatibility, to be removed)
	keyNameGetOneLevel (newName, &key->keyUSize);
	const size_t ownerLength = key->keyUSize - userLength;
	++key->keyUSize;
	char * owner = elektraMalloc (ownerLength + 1);
	if (!owner) return; // out of memory, ok for owner
	strncpy (owner, newName + userLength, ownerLength);
	owner[ownerLength] = 0;
	keySetOwner (key, owner);
	elektraFree (owner);
}*/

/**
 * Set a new name to a key.
 *
 * A valid name is one of the forms:
 * @copydetails doxygenNamespaces
 *
 * An invalid name either has an invalid namespace or
 * a wrongly escaped \\ at the end of the name.
 *
 * See @link keyname key names @endlink for the exact rules.
 *
 * The last form has explicitly set the owner, to let the library
 * know in which user folder to save the key. A owner is a user name.
 * If it is not defined (the second form) current user is used.
 *
 * You should always follow the guidelines for key tree structure creation.
 *
 * A private copy of the key name will be stored, and the @p newName
 * parameter can be freed after this call.
 *
 * .., . and / will be handled as in filesystem paths. A valid name will be build
 * out of the (valid) name what you pass, e.g. user///sw/../sw//././MyApp -> user/sw/MyApp
 *
 * On invalid names, NULL or "" the name will be "" afterwards.
 *
 *
 * @retval size in bytes of this new key name including ending NULL
 * @retval 0 if newName is an empty string or a NULL pointer (name will be empty afterwards)
 * @retval -1 if newName is invalid (name will be empty afterwards)
 * @retval -1 if key was inserted to a keyset before
 * @param key the key object to work with
 * @param newName the new key name
 * @see keyNew(), keySetOwner()
 * @see keyGetName(), keyGetFullName(), keyName()
 * @see keySetBaseName(), keyAddBaseName() to manipulate a name
 * @ingroup keyname
 */
ssize_t keySetName (Key * key, const char * newName)
{
	// TODO (kodebach): change to return size_t
	return elektraKeySetName (key, newName, 0);
}

static int elektraKeyNameValidatePart (const char * name, size_t len)
{
	// TODO (kodebach): check and document

	if (len == 0) return 1;
	if (name[0] == '@') return 0;
	if (name[0] == '%' && len > 1) return 0;

	// check for dangling escape
	const char * last = name + len - 1;
	int danglingEscape = 0;
	while (*last == '\\')
	{
		--last;
		danglingEscape = !danglingEscape;
	}

	if (danglingEscape) return 0;

	if (name[0] == '#')
	{
		size_t underscores = 0;
		while (name[1 + underscores] == '_')
		{
			++underscores;
		}

		size_t digits = 0;
		while (isdigit (name[1 + underscores + digits]))
		{
			++digits;
		}

		return len == 1 + underscores + digits && (underscores == 0 || underscores + 1 == digits) &&
		       (digits < 20 || (digits == 20 && strncmp (&name[1 + underscores], "18446744073709551615", 20) <= 0));
	}

	return 1;
}

int elektraKeyNameValidate (const char * name, const char * prefix)
{
	// TODO (kodebach): return required buffer size
	if (!name) return 0;

	size_t parts = 0;

	// TODO (kodebach): check and document
	if (prefix == NULL)
	{
		const char * slash = strchr (name, '/');
		if (slash == NULL) return 0;

		// TODO (kodebach): : after namespace
		size_t len = slash - name;
		if (slash != name && elektraReadNamespace (name, len) == KEY_NS_NONE)
		{
			// wrong namespace
			return 0;
		}

		if (slash[1] == '.' && slash[2] == '.' && slash[3] == '/')
		{
			// first part is /../
			return 0;
		}
	}
	else
	{
		// count parts in prefix (excluding namespace)
		// TODO (kodebach): : after namespace, just start there
		const char * slash = prefix;
		const char * end = prefix + strlen (prefix) - 1;
		while ((slash = strchr (slash, '/')) != NULL)
		{
			if (slash < end && *(slash - 1) != '\\')
			{
				++parts;
			}

			++slash;
		}
	}

	const char * lastSlash = name;
	const char * nextSlash;
	while ((nextSlash = strchr (lastSlash, '/')) != NULL)
	{
		while (nextSlash != NULL && nextSlash != name && *(nextSlash - 1) == '\\')
		{
			// skip escaped slashes
			nextSlash = strchr (nextSlash + 1, '/');
		}

		if (nextSlash == NULL)
		{
			break;
		}

		size_t len = nextSlash - lastSlash;
		if (!elektraKeyNameValidatePart (lastSlash, len))
		{
			return 0;
		}

		if (len == 2 && lastSlash[0] == '.' && lastSlash[1] == '.')
		{
			// removing part
			if (parts == 0)
			{
				// error, no part left to remove
				return 0;
			}
			--parts;
		}
		else if (len > 1 || (len == 1 && lastSlash[0] != '.'))
		{
			++parts;
		}

		lastSlash = nextSlash + 1;
	}

	return elektraKeyNameValidatePart (lastSlash, strlen (lastSlash));
}

// assume name is validated with elektraKeyNameValidate, otherwise behaviour undef
// return size including zero terminator -> 0 is error
size_t elektraKeyNameCanonicalize (const char * name, char ** canonicalName, size_t offset)
{
	if (!name) return 0;

	// TODO (kodebach): check and document
	// ensure buffer is big enough
	size_t outLen = strlen (name) + offset + 3;
	elektraRealloc ((void **) canonicalName, outLen);

	char * cname = *canonicalName + offset;
	char * outPtr = cname;

	const char * lastSlash = name;

	int slashes = 0;

	const char * nextSlash;
	while ((nextSlash = strchr (lastSlash, '/')) != NULL)
	{
		while (nextSlash != NULL && nextSlash != name && *(nextSlash - 1) == '\\')
		{
			// skip escaped slashes
			nextSlash = strchr (nextSlash + 1, '/');
		}

		if (nextSlash == NULL)
		{
			break;
		}

		// handle part between lastSlash and nextSlash
		size_t len = nextSlash - lastSlash;
		if (len > 0)
		{
			++slashes;
		}

		if (len == 1 && lastSlash[0] == '.')
		{
			// /./ -> ignore
		}
		else if (len == 2 && lastSlash[0] == '.' && lastSlash[1] == '.')
		{
			// /../ -> move outPtr back to previous part
			--outPtr;
			do
			{
				--outPtr;
				while (*outPtr != '/' && outPtr > *canonicalName)
				{
					--outPtr;
				}
				// skip escaped slashes
			} while (*(outPtr - 1) == '\\' && outPtr > *canonicalName);
			// go back forward so we are after the slash
			++outPtr;
		}
		/*else if (lastSlash[1] == '.')
		{
			// inactive key
			// TODO: mark inactive with flag?
		}*/
		else if (lastSlash[0] == '#')
		{
			// array part -> ensure prefix and copy
			size_t underscores = 0;
			while (lastSlash[1 + underscores] == '_')
			{
				++underscores;
			}

			size_t digits = 0;
			while (isdigit (lastSlash[1 + underscores + digits]))
			{
				++digits;
			}

			*outPtr++ = '#';
			if (underscores == 0 && digits > 0)
			{
				outLen += digits - 1;
				elektraRealloc ((void **) canonicalName, outLen);
				for (size_t i = 0; i < digits - 1; ++i)
				{
					*outPtr++ = '_';
				}
			}
			memcpy (outPtr, lastSlash + 1, underscores + digits);
			outPtr += underscores + digits;
			*outPtr++ = '/';
		}
		else if (len > 0 || lastSlash == name)
		{
			// normal part -> just copy
			memcpy (outPtr, lastSlash, len);
			outPtr += len;
			*outPtr++ = '/';
		}

		lastSlash = nextSlash + 1;
	}

	size_t lastLen = strlen (lastSlash);
	if (lastLen > 1 || (lastLen == 1 && lastSlash[0] != '.'))
	{
		strncpy (outPtr, lastSlash, lastLen);
		outPtr += lastLen;
	}
	else if (slashes > 1 || offset > 0)
	{
		--outPtr;
	}

	// terminate
	*outPtr++ = '\0';

	return outPtr - *canonicalName;
}

// assumes name is canonical
size_t elektraKeyNameUnescape (const char * name, char ** unescapedName, size_t offset)
{
	if (!name) return 0;

	// TODO (kodebach): check and document
	// ensure buffer is big enough
	elektraRealloc ((void **) unescapedName, strlen (name) + offset + 2);

	char * uname = *unescapedName + offset;
	char * outPtr = uname;

	const char * lastSpecial = name;

	if (offset == 0)
	{
		if (name[0] != '/')
		{
			// TODO (kodebach): colon after namespace
			const char * firstSlash = strchr (name, '/');
			*outPtr++ = elektraReadNamespace (name, firstSlash - name);
			lastSpecial = firstSlash;
		}
		else
		{
			*outPtr++ = KEY_NS_CASCADING;
		}
	}


	// copy piecewise between special characters
	const char * nextSpecial;
	while ((nextSpecial = strpbrk (lastSpecial, "%\\")) != NULL)
	{
		// copy stuff before special
		if (nextSpecial > lastSpecial)
		{
			size_t len = nextSpecial - lastSpecial;
			memcpy (outPtr, lastSpecial, len);

			// replace / with \0
			for (size_t i = 0; i < len; ++i, ++outPtr)
			{
				if (*outPtr == '/')
				{
					*outPtr = '\0';
				}
			}
		}

		if (*nextSpecial == '\\')
		{
			*outPtr++ = *++nextSpecial;
		}
		else if (*nextSpecial == '%' && outPtr != uname && *(outPtr - 1) != '\0')
		{
			*outPtr++ = '%';
		}

		lastSpecial = nextSpecial + 1;
	}

	// copy rest of name
	size_t len = strlen (lastSpecial);
	memcpy (outPtr, lastSpecial, len);

	// replace / with \0
	for (size_t i = 0; i < len; ++i, ++outPtr)
	{
		if (*outPtr == '/')
		{
			*outPtr = '\0';
		}
	}

	// terminate
	*outPtr++ = '\0';

	return outPtr - *unescapedName;
}

// TODO (kodebach): remove options
ssize_t elektraKeySetName (Key * key, const char * newName, option_t options)
{
	// TODO (kodebach): change to return size_t
	if (!key) return -1;
	if (test_bit (key->flags, KEY_FLAG_RO_NAME)) return -1;

	clear_bit (key->flags, (keyflag_t) KEY_FLAG_MMAP_KEY);

	if (test_bit (key->flags, KEY_FLAG_MMAP_KEY))
	{
		// key was in mmap region, clear flag and set NULL to allow realloc
		key->key = NULL;
		key->ukey = NULL;
		clear_bit (key->flags, (keyflag_t) KEY_FLAG_MMAP_KEY);
	}

	if (newName == NULL || strlen (newName) == 0)
	{
		return -1;
	}

	if (!elektraKeyNameValidate (newName, NULL))
	{
		// error invalid name
		return -1;
	}

	size_t keySize = elektraKeyNameCanonicalize (newName, &key->key, 0);
	if (keySize == 0)
	{
		// error
		return -1;
	}
	key->keySize = keySize;

	ssize_t keyUSize = elektraKeyNameUnescape (key->key, &key->ukey, 0);
	if (keyUSize == 0)
	{
		// error, MUST be out of memory error
		return -1;
	}
	key->keyUSize = keyUSize;

	set_bit (key->flags, KEY_FLAG_SYNC);

	if (!(options & KEY_META_NAME) && keyGetNamespace (key) != KEY_NS_META) keySetOwner (key, NULL);
	return key->keySize;
}


/**
 * Bytes needed to store the key name including user domain and ending NULL.
 *
 * @param key the key object to work with
 * @return number of bytes needed to store key name including user domain
 * @retval 1 on empty name
 * @retval -1 on NULL pointer
 * @see keyGetFullName(), keyGetNameSize()
 * @ingroup keyname
 */
ssize_t keyGetFullNameSize (const Key * key)
{
	size_t returnedSize = 0;

	if (!key) return -1;

	if (!key->key) return 1;

	returnedSize = elektraStrLen (key->key);

	if (keyNameIsUser (key->key) && keyGetMeta (key, "owner")) returnedSize += keyGetOwnerSize (key);

	/*
	   After 2 elektraStrLen() calls looks like we counted one more NULL.
	   But we need this byte count because a full key name has an
	   additional ':' char.
	*/

	return returnedSize;
}


/**
 * Get key full name, including the user domain name.
 *
 * @return number of bytes written
 * @retval 1 on empty name
 * @retval -1 on NULL pointers
 * @retval -1 if maxSize is 0 or larger than SSIZE_MAX
 * @param key the key object
 * @param returnedName pre-allocated memory to write the key name
 * @param maxSize maximum number of bytes that will fit in returnedName, including the final NULL
 * @ingroup keyname
 */
ssize_t keyGetFullName (const Key * key, char * returnedName, size_t maxSize)
{
	size_t userSize = sizeof ("user") - 1;
	ssize_t length;
	ssize_t maxSSize;
	char * cursor;

	if (!key) return -1;
	if (!returnedName) return -1;
	if (!maxSize) return -1;

	if (maxSize > SSIZE_MAX) return -1;
	maxSSize = maxSize;

	length = keyGetFullNameSize (key);
	if (length == 1)
	{
		/*errno=KDB_ERR_NOKEY;*/
		returnedName[0] = 0;
		return length;
	}
	else if (length < 0)
		return length;
	else if (length > maxSSize)
	{
		/* errno=KDB_ERR_TRUNC; */
		return -1;
	}

	cursor = returnedName;
	if (keyIsUser (key))
	{
		strncpy (cursor, key->key, userSize);
		cursor += userSize;
		if (keyGetMeta (key, "owner"))
		{
			*cursor = ':';
			++cursor;
			size_t ownerSize = keyGetValueSize (keyGetMeta (key, "owner")) - 1;
			strncpy (cursor, keyValue (keyGetMeta (key, "owner")), ownerSize);
			cursor += ownerSize;
		}
		strcpy (cursor, key->key + userSize);
	}
	else
		strcpy (cursor, key->key);

	return length;
}


/**
 * @brief Returns a pointer to the internal unescaped key name where the @p basename starts.
 *
 * This is a much more efficient version of keyGetBaseName() and you should
 * use it if you are responsible enough to not mess up things. The name might
 * change or even point to a wrong place after a keySetName(). So make
 * sure to copy the memory before the name changes.
 *
 * keyBaseName() returns "" when there is no keyBaseName. The reason is
 * @snippet testabi_key.c base0 empty
 *
 * And there is also support for really empty basenames:
 * @snippet testabi_key.c base1 empty
 *
 * @note You must never use the pointer returned by keyBaseName()
 * method to change the name, but you should use keySetBaseName()
 * instead.
 *
 * @note Do not assume that keyBaseName() points to the same region as
 * keyName() does.
 *
 * @param key the object to obtain the basename from
 * @return a pointer to the basename
 * @retval "" when the key has no (base)name
 * @retval 0 on NULL pointer
 * @see keyGetBaseName(), keyGetBaseNameSize()
 * @see keyName() to get a pointer to the name
 * @see keyOwner() to get a pointer to the owner
 * @ingroup keyname
 */
const char * keyBaseName (const Key * key)
{
	if (!key) return 0;
	if (!key->key) return "";

	const char * baseName = key->ukey + key->keyUSize - 2;
	while (*baseName != '\0')
	{
		--baseName;
	}
	return baseName + 1;
}


/**
 * Calculates number of bytes needed to store basename of @p key.
 *
 * Key names that have only root names (e.g. @c "system" or @c "user"
 * or @c "user:domain" ) does not have basenames, thus the function will
 * return 1 bytes to store "".
 *
 * Basenames are denoted as:
 * - @c system/some/thing/basename -> @c basename
 * - @c user:domain/some/thing/base\\/name > @c base\\/name
 *
 * @param key the key object to work with
 * @return size in bytes of @p key's basename including ending NULL
 * @see keyBaseName(), keyGetBaseName()
 * @see keyName(), keyGetName(), keySetName()
 * @ingroup keyname
 */
ssize_t keyGetBaseNameSize (const Key * key)
{
	const char * baseName = keyBaseName (key);
	if (!baseName) return -1;

	return elektraStrLen (baseName);
}


/**
 * Calculate the basename of a key name and put it in @p returned finalizing
 * the string with NULL.
 *
 * Some examples:
 * - basename of @c system/some/keyname is @c keyname
 * - basename of @c "user/tmp/some key" is @c "some key"
 *
 * @param key the key to extract basename from
 * @param returned a pre-allocated buffer to store the basename
 * @param maxSize size of the @p returned buffer
 * @return number of bytes copied to @p returned
 * @retval 1 on empty name
 * @retval -1 on NULL pointers
 * @retval -1 when maxSize is 0 or larger than SSIZE_MAX
 * @see keyBaseName(), keyGetBaseNameSize()
 * @see keyName(), keyGetName(), keySetName()
 * @ingroup keyname
 */
ssize_t keyGetBaseName (const Key * key, char * returned, size_t maxSize)
{
	if (!key) return -1;
	if (!returned) return -1;
	if (!maxSize) return -1;

	if (maxSize > SSIZE_MAX) return -1;
	ssize_t maxSSize = maxSize;

	if (!key->key)
	{
		returned[0] = 0;
		return 1;
	}

	ssize_t baseSize = keyGetBaseNameSize (key);
	if (maxSSize < baseSize)
	{
		return -1;
	}

	const char * baseName = keyBaseName (key);

	if (!baseName)
	{
		return -1;
	}

	strncpy (returned, baseName, baseSize); // TODO (kodebach): memcpy
	return baseSize;
}

size_t elektraKeyNameEscapePart (const char * part, char ** escapedPart)
{
	// TODO (kodebach): array parts
	if (!part) return 0;

	if (strlen (part) == 0)
	{
		elektraRealloc ((void **) escapedPart, 2);
		**escapedPart = '%';
		*(*escapedPart + 1) = '\0';
		return 1;
	}

	int specialStart = strchr (".%@/\\", part[0]) != NULL;
	size_t special = specialStart ? 1 : 0;

	const char * cur = part + 1;
	while ((cur = strpbrk (cur, "/\\")) != NULL)
	{
		++special;
		++cur;
	}

	elektraRealloc ((void **) escapedPart, strlen (part) + special + 1);

	char * outPtr = *escapedPart;

	if (specialStart)
	{
		*outPtr++ = '\\';
	}

	const char * last = part;
	while ((cur = strpbrk (last, "/\\")) != NULL)
	{
		size_t len = cur - last;
		memcpy (outPtr, last, len);
		outPtr += len;

		if (cur > part)
		{
			// start backslash already added above
			*outPtr++ = '\\';
		}
		*outPtr++ = *cur;

		last = cur + 1;
	}

	size_t len = strlen (last);
	memcpy (outPtr, last, len);
	outPtr += len;
	*outPtr++ = '\0';

	return outPtr - *escapedPart - 1;
}

static size_t keyAddBaseNameInternal (Key * key, const char * baseName)
{
	size_t unescapedSize;
	size_t escapedSize;
	char * escaped = NULL;
	int hasPath = key->keyUSize > 3;

	if (baseName == NULL)
	{
		// for keySetBaseName
		unescapedSize = 0;
		escapedSize = 0;
	}
	else
	{
		unescapedSize = strlen (baseName);
		escapedSize = elektraKeyNameEscapePart (baseName, &escaped);
		if (escapedSize == 0)
		{
			// error
			return -1;
		}

		if (hasPath)
		{
			// more than just namespace -> must add separator
			++escapedSize;
			++unescapedSize;
		}
	}

	size_t newKeySize = key->keySize + escapedSize;
	size_t newKeyUSize = key->keyUSize + unescapedSize;
	if (test_bit (key->flags, KEY_FLAG_MMAP_KEY))
	{
		// key was in mmap region, clear flag and copy to malloced buffer
		char * tmp = elektraMalloc (newKeySize);
		memcpy (tmp, key->key, key->keySize);
		key->key = tmp;

		tmp = elektraMalloc (newKeyUSize);
		memcpy (tmp, key->ukey, key->keyUSize);
		key->ukey = tmp;

		clear_bit (key->flags, (keyflag_t) KEY_FLAG_MMAP_KEY);
	}
	else
	{
		elektraRealloc ((void **) &key->key, newKeySize);
		elektraRealloc ((void **) &key->ukey, newKeyUSize);
	}

	if (baseName == NULL)
	{
		// for keySetBaseName
		key->key[key->keySize - 1] = '\0';
		key->ukey[key->keyUSize - 1] = '\0';

		return key->keySize;
	}

	// add escaped name
	if (hasPath)
	{
		// more than just namespace -> must add separator
		key->key[key->keySize - 1] = '/';
		memcpy (&key->key[key->keySize], escaped, escapedSize);
	}
	else
	{
		memcpy (&key->key[key->keySize - 1], escaped, escapedSize);
	}
	elektraFree (escaped);

	// set keySize and terminate escaped name
	key->keySize += escapedSize;
	key->key[key->keySize - 1] = '\0';

	// add unescaped name
	if (hasPath)
	{
		// more than just namespace -> must add separator
		key->ukey[key->keyUSize - 1] = '\0';
		memcpy (&key->ukey[key->keyUSize], baseName, unescapedSize);
	}
	else
	{
		memcpy (&key->ukey[key->keyUSize - 1], baseName, unescapedSize);
	}

	// set keyUSize and terminate escaped name
	key->keyUSize += unescapedSize;
	key->ukey[key->keyUSize - 1] = '\0';

	set_bit (key->flags, KEY_FLAG_SYNC);
	return key->keySize;
}

/**
 * Adds @p baseName (that will be escaped) to the current key name.
 *
 * A new baseName will be added, no other part of the key name will be
 * affected.
 *
 * Assumes that @p key is a directory and will append @p baseName to it.
 * The function adds the path separator for concatenating.
 *
 * So if @p key has name @c "system/dir1/dir2" and this method is called with
 * @p baseName @c "mykey", the resulting key will have the name
 * @c "system/dir1/dir2/mykey".
 *
 * When @p baseName is 0 nothing will happen and the size of the name is returned.
 *
 * The escaping rules apply as in @link keyname above @endlink.
 *
 * A simple example is:
 * @snippet keyBasename.c add base basic
 *
 * E.g. if you add . it will be escaped:
 * @snippet testabi_key.c base1 add
 *
 * @see keySetBaseName() to set a base name
 * @see keySetName() to set a new name.
 *
 * @param key the key object to work with
 * @param baseName the string to append to the name
 * @return the size in bytes of the new key name including the ending NULL
 * @retval -1 if the key had no name
 * @retval -1 on NULL pointers
 * @retval -1 if key was inserted to a keyset before
 * @retval -1 on allocation errors
 * @ingroup keyname
 *
 */
ssize_t keyAddBaseName (Key * key, const char * baseName)
{
	// TODO (kodebach): change to return size_t
	if (!key) return -1;
	if (!baseName) return -1;
	if (test_bit (key->flags, KEY_FLAG_RO_NAME)) return -1;
	if (!key->key) return -1;

	return keyAddBaseNameInternal (key, baseName);
}

/**
 * @brief Add an already escaped name to the keyname.
 *
 * The same way as in keySetName() this method finds the canonical pathname:
 * - it will ignore /./
 * - it will remove a level when /../ is used
 * - it will remove multiple slashes ////
 *
 * For example:
 * @snippet keyName.c add name
 *
 * Unlike keySetName() it adds relative to the previous name and
 * cannot change the namespace of a key.
 * For example:
 * @snippet keyName.c namespace
 *
 * The passed name needs to be valid according the @link keyname key name rules @endlink.
 * It is not allowed to:
 * - be empty
 * - end with unequal number of \\
 *
 * @param key the key where a name should be added
 * @param newName the new name to append
 *
 * @since 0.8.11
 *
 * @retval size of the new key
 * @retval -1 if key is a null pointer or the key has an empty name
 * @retval -1 if newName is not a valid escaped name
 * @retval -1 on allocation errors
 * @retval -1 if key was inserted to a keyset before
 * @retval 0 if nothing was done because newName had only slashes, is too short, is empty or is null
 * @ingroup keyname
 */
ssize_t keyAddName (Key * key, const char * newName)
{
	// TODO (kodebach): error codes
	if (!key) return -1;
	if (test_bit (key->flags, KEY_FLAG_RO_NAME)) return -1;
	if (!newName) return -1;

	while (*newName == '/')
	{
		// skip leading slashes
		++newName;
		if (*newName == '.' && (*(newName + 1) == '/' || *(newName + 1) == '\0'))
		{
			// also skip /./ parts
			newName += 2;
		}
	}

	if (strlen (newName) == 0) return key->keySize;

	if (!elektraKeyNameValidate (newName, key->key))
	{
		// error invalid name suffix
		return -1;
	}

	// implementation note: don't modify key before this line, after this line only fail on malloc problems

	if (test_bit (key->flags, KEY_FLAG_MMAP_KEY))
	{
		// key was in mmap region, clear flag and copy to malloced buffer
		char * tmp = elektraMalloc (key->keySize);
		memcpy (tmp, key->key, key->keySize);
		key->key = tmp;

		tmp = elektraMalloc (key->keyUSize);
		memcpy (tmp, key->ukey, key->keyUSize);
		key->ukey = tmp;

		clear_bit (key->flags, (keyflag_t) KEY_FLAG_MMAP_KEY);
	}

	if (key->key[key->keySize - 2] == '/')
	{
		key->keySize--;
	}
	else
	{
		key->key[key->keySize - 1] = '/';
	}
	ssize_t keySize = elektraKeyNameCanonicalize (newName, &key->key, key->keySize);
	if (keySize == 0)
	{
		// error
		return -1;
	}

	key->keySize = keySize;


	if (key->ukey[key->keyUSize - 2] == '\0')
	{
		key->keyUSize--;
	}
	ssize_t keyUSize = elektraKeyNameUnescape (newName, &key->ukey, key->keyUSize);
	if (keyUSize == 0)
	{
		// error, MUST be out of memory error
		return -1;
	}
	key->keyUSize = keyUSize;

	set_bit (key->flags, KEY_FLAG_SYNC);
	return key->keySize;
}

static const char * elektraKeyFindBaseNamePtr (Key * key)
{
	// TODO (kodebach): check and document, : after namespace
	const char * slash = strchr (key->key, '/');
	const char * end = key->key + key->keySize - 2;
	const char * lastPart = slash == end ? NULL : slash;
	while ((slash = strchr (slash + 1, '/')) != NULL)
	{
		if (*(slash - 1) != '\\')
		{
			lastPart = slash;
		}
	}
	return lastPart;
}

/**
 * Sets @c baseName as the new basename for @c key.
 *
 * Only the baseName will be affected and no other part of the key.
 *
 * A simple example is:
 * @snippet keyBasename.c set base basic
 *
 * All text after the last @c '/' in the @p key keyname is erased and
 * @p baseName is appended.
 * If @p baseName is 0 (NULL), then the last part of the keyname is
 * removed without replacement.
 *
 * Let us suppose @p key has name @c "system/dir1/dir2/key1". If @p baseName
 * is @c "key2", the resulting key name will be @c "system/dir1/dir2/key2".
 * If @p baseName is 0 (NULL), the resulting key name will
 * be @c "system/dir1/dir2".
 * If @p baseName is empty, the resulting key name will
 * be @c "system/dir1/dir2/%", where @c "%" denotes an empty base name,
 * as also shown in the following code:
 *
 * @snippet testabi_key.c base2
 *
 * keySetBaseName() does proper escaping on the supplied name argument.
 *
 * You can use character sequences as @c baseName (e.g. @c "." (dot), @c ".."
 * (dot-dot), @c "%" (empty basename)). They will be properly escaped
 * and will not have their usual meaning.
 *
 * @see keyname for more details on special names
 *
 * If you want to add and not change the basename, use keyAddBaseName()
 * instead. If you do not want escaping, use keyAddName() instead.
 *
 * @see keyAddBaseName() to add a basename instead of changing it
 * @see keyAddName() to add a name without escaping
 * @see keySetName() to set a completely new name
 *
 * To add an inactive key name, use:
 * @snippet testabi_key.c base1
 *
 * @param key the key object to work with
 * @param baseName the string used to overwrite the basename of the key
 * @return the size in bytes of the new key name
 * @retval -1 on NULL pointers in key
 * @retval -1 if key was inserted to a keyset before
 * @retval -1 on allocation errors
 * @ingroup keyname
 */
ssize_t keySetBaseName (Key * key, const char * baseName)
{
	// TODO (kodebach): change to return size_t
	if (!key) return -1;
	if (test_bit (key->flags, KEY_FLAG_RO_NAME)) return -1;
	if (!key->key) return -1;

	// adjust sizes to exclude base name
	const char * baseNamePtr = elektraKeyFindBaseNamePtr (key);
	if (baseNamePtr == NULL)
	{
		return -1;
	}
	key->keySize = baseNamePtr - key->key + 1;

	const char * ubaseNamePtr = key->ukey + key->keyUSize - 2;
	while (*ubaseNamePtr != '\0')
	{
		--ubaseNamePtr;
	}
	key->keyUSize = ubaseNamePtr - key->ukey + 1;

	// add new base name, only resizes buffer when baseName == NULL
	return keyAddBaseNameInternal (key, baseName);
}

/**
 * For currently valid namespaces see #elektraNamespace.
 *
 * @since 0.8.10
 * Added method to kdbproposal.h
 *
 * To handle every possible cases (including namespaces) a key can have:
 * @snippet namespace.c namespace
 *
 * To loop over all valid namespaces use:
 * @snippet namespace.c loop
 *
 * @note This method might be enhanced. You do not have any guarantee
 * that, when for a specific name #KEY_NS_META
 * is returned today, that it still will be returned after the next
 * recompilation. So make sure that your compiler gives you a warning
 * for unhandled switches (gcc: -Wswitch or -Wswitch-enum if you
 * want to handle default) and look out for those warnings.
 *
 * @param key the key object to work with
 * @return the namespace of a key.
 */
elektraNamespace keyGetNamespace (const Key * key)
{
	if (!key) return KEY_NS_NONE;
	return (elektraNamespace) key->ukey[0];
}

// TODO (kodebach): replace ssize_t with size_t?
ssize_t keySetNamespace (Key * key, elektraNamespace ns)
{
	// TODO (kodebach): document and correct escaped key
	if (!key) return -1;
	if (ns == KEY_NS_NONE || ns == KEY_NS_EMPTY) return -1;

	if (ns == key->ukey[0]) return key->keySize;

	size_t oldNamespaceLen;
	switch (key->ukey[0])
	{
	case KEY_NS_USER:
		oldNamespaceLen = sizeof ("user") - 1;
		break;
	case KEY_NS_SYSTEM:
		oldNamespaceLen = sizeof ("system") - 1;
		break;
	case KEY_NS_DIR:
		oldNamespaceLen = sizeof ("dir") - 1;
		break;
	case KEY_NS_META:
		oldNamespaceLen = sizeof ("meta") - 1;
		break;
	case KEY_NS_SPEC:
		oldNamespaceLen = sizeof ("spec") - 1;
		break;
	case KEY_NS_PROC:
		oldNamespaceLen = sizeof ("proc") - 1;
		break;
	case KEY_NS_CASCADING:
		oldNamespaceLen = 0;
		break;
	/* TODO (kodebach): case KEY_NS_DEFAULT:
		oldNamespaceLen = sizeof("default") -1;
		break;*/
	default:
		return -1;
	}

	size_t newNamespaceLen;
	const char * newNamespace;
	switch (ns)
	{
	case KEY_NS_USER:
		newNamespaceLen = sizeof ("user") - 1;
		newNamespace = "user";
		break;
	case KEY_NS_SYSTEM:
		newNamespaceLen = sizeof ("system") - 1;
		newNamespace = "system";
		break;
	case KEY_NS_DIR:
		newNamespaceLen = sizeof ("dir") - 1;
		newNamespace = "dir";
		break;
	case KEY_NS_META:
		newNamespaceLen = sizeof ("meta") - 1;
		newNamespace = "meta";
		break;
	case KEY_NS_SPEC:
		newNamespaceLen = sizeof ("spec") - 1;
		newNamespace = "spec";
		break;
	case KEY_NS_PROC:
		newNamespaceLen = sizeof ("proc") - 1;
		newNamespace = "proc";
		break;
	case KEY_NS_CASCADING:
		newNamespaceLen = 0;
		newNamespace = "";
		break;
	/* TODO (kodebach): case KEY_NS_DEFAULT:
		newNamespaceLen = sizeof("default") -1;
		newNamespace = "default";
		break;*/
	default:
		return -1;
	}

	elektraRealloc ((void **) &key->key, key->keySize - oldNamespaceLen + newNamespaceLen);
	memmove (key->key + newNamespaceLen, key->key + oldNamespaceLen, key->keySize - oldNamespaceLen);
	memcpy (key->key, newNamespace, newNamespaceLen);
	key->keySize += newNamespaceLen - oldNamespaceLen;
	key->key[key->keySize - 1] = '\0';

	key->ukey[0] = ns;

	return key->keySize;
}


/**
 * @}
 */
