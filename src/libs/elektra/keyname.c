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
 * - @p spec:/something for specification of other keys.
 * - @p proc:/something for in-memory keys, e.g. commandline.
 * - @p dir:/something for dir keys in current working directory
 * - @p system:/something for system keys in /etc or /
 * - @p user:/something for user keys in home directory
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

/**
 * Helper method: returns a pointer to the start of the last part of the given key name
 *
 * @param name a canonical key name
 * @param len the length of the key name
 *
 * @returns pointer to the start of the last part within @p name
 */
static char * findStartOfLastPart (char * name, size_t len)
{
	char * colon = strchr (name, ':');
	char * start = colon == NULL ? name : colon + 1;
	++start; // start after first slash

	char * cur = start + len - (start - name) - 1;

	if (cur == start) return NULL; // no base name

	while (cur >= start)
	{
		--cur;
		while (cur >= start && *cur != '/')
		{
			--cur;
		}

		size_t backslashes = 0;
		while (cur - backslashes > start && *(cur - backslashes - 1) == '\\')
		{
			++backslashes;
		}

		if (backslashes % 2 == 0)
		{
			break;
		}
	}

	return cur < start - 1 ? NULL : cur;
}


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
 * @see keyGetName() as alternative to get a copy
 * @see keyUnescapedName to get an unescaped key name
 * @ingroup keyname
 */
const char * keyName (const Key * key)
{
	if (!key) return 0;

	ELEKTRA_ASSERT (key->key != NULL, "invalid name");
	return key->key;
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
 * @see keyGetName()
 * @see keyGetUnescapedNameSize to get size of unescaped name
 * @ingroup keyname
 */
ssize_t keyGetNameSize (const Key * key)
{
	if (!key) return -1;

	return key->keySize;
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
	ELEKTRA_ASSERT (key->ukey != NULL, "invalid name");
	return key->ukey;
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

	return key->keyUSize;
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
 * @see keyGetNameSize()
 * @ingroup keyname
 */
ssize_t keyGetName (const Key * key, char * returnedName, size_t maxSize)
{
	if (!key) return -1;

	if (!returnedName) return -1;

	if (!maxSize) return -1;

	if (maxSize > SSIZE_MAX) return -1;

	if (!key->key)
	{
		returnedName[0] = 0;
		return 1;
	}

	if (key->keySize > maxSize)
	{
		return -1;
	}

	memcpy (returnedName, key->key, key->keySize);

	return key->keySize;
}

/**
 * Copies the unescaped name of a key into provided buffer.
 * We will only copy the full name, if the buffer is to small an error code is returned.
 *
 * To ensure your buffer is big enough, you can use keyGetUnescapedNameSize() to find the correct size.
 *
 * @param key          the Key to extract the unescaped name from
 * @param returnedName output buffer
 * @param maxSize      maximum bytes that can be copied into @p returnedName
 *
 * @pre @p key MUST be a valid #Key and `key != NULL`
 * @pre @p returnedName MUST be allocated to be at least @p maxSize bytes big and `returnedName != NULL`
 *
 * @retval -1 precondition error
 * @retval -2 the size of the unescaped name is bigger then @p maxSize
 * @returns otherwise, the actual size of the unescaped name, i.e. the number of bytes copied into @p returnedName
 */
ssize_t keyGetUnescapedName (const Key * key, char * returnedName, size_t maxSize)
{
	if (!key) return -1;
	if (!returnedName) return -1;

	if (!key->ukey)
	{
		returnedName[0] = 0;
		return 1;
	}

	if (key->keyUSize > maxSize)
	{
		return -2;
	}

	memcpy (returnedName, key->ukey, maxSize);

	return key->keyUSize;
}

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
 * out of the (valid) name what you pass, e.g. user:///sw/../sw//././MyApp -> user:/sw/MyApp
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
 * @see keyNew()
 * @see keyGetName(), keyName()
 * @see keySetBaseName(), keyAddBaseName() to manipulate a name
 * @ingroup keyname
 */
ssize_t keySetName (Key * key, const char * newName)
{
	if (!key) return -1;
	if (test_bit (key->flags, KEY_FLAG_RO_NAME)) return -1;
	if (newName == NULL || strlen (newName) == 0) return -1;

	if (!elektraKeyNameValidate (newName, true))
	{
		// error invalid name
		return -1;
	}

	// from now on this function CANNOT fail -> we may modify the key

	if (test_bit (key->flags, KEY_FLAG_MMAP_KEY))
	{
		// key was in mmap region, clear flag and set NULL to allow realloc
		key->key = NULL;
		key->keySize = 0;
		key->ukey = NULL;
		key->keyUSize = 0;
		clear_bit (key->flags, (keyflag_t) KEY_FLAG_MMAP_KEY);
	}

	elektraKeyNameCanonicalize (newName, &key->key, &key->keySize, 0, &key->keyUSize);

	elektraRealloc ((void **) &key->ukey, key->keyUSize);

	elektraKeyNameUnescape (key->key, key->ukey);

	set_bit (key->flags, KEY_FLAG_SYNC);

	return key->keySize;
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
 * @pre @p key MUST be a valid #Key
 *
 * @since 0.8.11
 *
 * @retval -1 if `key == NULL`, @p key is read-only, `newName == NULL` or @p newName is not a valid escaped name
 * @returns new size of the escaped name of @p key
 *
 * @ingroup keyname
 */
ssize_t keyAddName (Key * key, const char * newName)
{
	if (!key) return -1;
	if (test_bit (key->flags, KEY_FLAG_RO_NAME)) return -1;
	if (!newName) return -1;

	while (*newName == '/')
	{
		// skip leading slashes
		++newName;
		if (*newName == '.' && *(newName + 1) == '/')
		{
			// also skip /./ parts
			newName += 2;
		}
	}

	if (strlen (newName) == 0) return key->keySize;

	if (!elektraKeyNameValidate (newName, false))
	{
		// error invalid name suffix
		return -1;
	}

	// from now on this function CANNOT fail -> we may modify the key

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

	elektraKeyNameCanonicalize (newName, &key->key, &key->keySize, key->keySize, &key->keyUSize);

	elektraRealloc ((void **) &key->ukey, key->keyUSize);

	elektraKeyNameUnescape (key->key, key->ukey);

	set_bit (key->flags, KEY_FLAG_SYNC);
	return key->keySize;
}

/**
 * Takes an escaped key name and validates it.
 * Complete key names must inlcude a namespace or a leading slash.
 *
 * @param name       The escaped key name to check
 * @param isComplete Whether or not @p name is supposed to be a complete key name
 *
 * @retval #true If @p name is a valid key name.
 * @retval #false Otherwise
 *
 * @ingroup keyname
 */
bool elektraKeyNameValidate (const char * name, bool isComplete)
{
	if (name == NULL || (strlen (name) == 0 && isComplete)) return 0;

	if (isComplete)
	{
		const char * colon = strchr (name, ':');
		if (colon != NULL)
		{
			if (elektraReadNamespace (name, colon - name - 1) == KEY_NS_NONE)
			{
				ELEKTRA_LOG_DEBUG ("Illegal namespace '%.*s': %s", (int) (colon - name - 1), name, name);
				return 0;
			}

			if (*(colon + 1) != '/')
			{
				ELEKTRA_LOG_DEBUG ("Missing slash after namespace: %s", name);
				return 0;
			}

			name = colon + 1;
		}

		if (*name != '/')
		{
			ELEKTRA_LOG_DEBUG ("Illegal name start; expected (namespace +) slash: %s", name);
			return 0;
		}

		if (*(name + 1) == '%' && *(name + 2) == '\0')
		{
			ELEKTRA_LOG_DEBUG ("Illegal escaped part; first part cannot be empty (collides with root key): %s", name);
			return 0;
		}
	}
	else
	{
		if (*name == '%' && *(name + 1) == '\0')
		{
			ELEKTRA_LOG_DEBUG ("Illegal escaped part; first part cannot be empty (collides with root key): %s", name);
			return 0;
		}
	}

	const char * cur = name;
	while ((cur = strchr (cur, '\\')) != NULL)
	{
		++cur;
		switch (*cur)
		{
		case '\0':
			ELEKTRA_LOG_DEBUG ("Dangling escape: %s", name);
			return 0;
		case '\\':
		case '/':
			++cur;
			// allowed anywhere
			continue;
		case '%':
			if (*(cur - 2) == '/' && (*(cur + 1) == '/' || *(cur + 1) == '\0')) continue;

			break;
		case '.':
			if (*(cur - 2) == '/' && (*(cur + 1) == '/' || *(cur + 1) == '\0')) continue;
			if (*(cur - 2) == '/' && *(cur + 1) == '.' && (*(cur + 2) == '/' || *(cur + 2) == '\0')) continue;

			break;
		case '#': {
			const char * end = cur + 1;
			while (isdigit (*end))
			{
				++end;
			}
			size_t len = end - cur;

			bool check1 = *end == '/' || *end == '\0';
			bool check2 = len < 20 || strncmp (cur + 1, "9223372036854775807", 19) <= 0;
			bool check3 = *(cur + 1) != '0';

			if (check1 && check2 && check3) continue;

			break;
		}
		}


		ELEKTRA_LOG_DEBUG ("Illegal escape '\\%c': %s", *cur, name);
		return 0;
	}

	return 1;
}

/**
 * Takes a valid (non-)canonical key name and produces its canonical form.
 * As a side-effect it can also calculate the size of the corresponding unescaped key name.
 *
 * @param name          The key name that is processed
 * @param canonicalName Output buffer for the canonical name
 * @param canonicalSizePtr Pointer to size of @p canonicalName
 * @param offset        Offset into @p canonicalName
 * @param usizePtr      Output variable for the size of the unescaped name
 *
 * @pre @p name MUST be a valid (non-)canonical key name. If it is not, the result is undefined
 * @pre @p canonicalName MUST be a valid first argument for elektraRealloc() when cast to void**
 * @pre @p canonicalSizePtr >= @p offset
 * @pre @p offset MUST be 0 or `*canonicalName + offset` MUST point to the zero-termintor of a valid canonical key name that starts at
 * `*canonicalName`
 * @pre if @p offset is 0 then `*usizePtr` MUST 0, otherwise `*usizePtr` MUST be the correct unescaped size of the existing canonical name
 * in `*canonicalName`
 *
 * @see elektraKeyNameValidate
 *
 * @ingroup keyname
 */
void elektraKeyNameCanonicalize (const char * name, char ** canonicalName, size_t * canonicalSizePtr, size_t offset, size_t * usizePtr)
{
	size_t nameLen = strlen (name) + 1;

	// ensure output is at least as big as input
	// output buffer will be enlarged when needed
	// at the end we shrink to the correct size
	if (offset + nameLen + 1 > *canonicalSizePtr)
	{
		*canonicalSizePtr = offset + nameLen;
		elektraRealloc ((void **) canonicalName, *canonicalSizePtr);
	}

	char * outPtr;
	size_t usize;

	// stores start of first part
	size_t rootOffset;
	if (offset == 0)
	{
		// namespace byte
		usize = 1;
		outPtr = *canonicalName;

		if (*name != '/')
		{
			// find end of namespace
			const char * colon = strchr (name, ':');

			// copy namespace
			memcpy (outPtr, name, colon - name + 1);
			outPtr += colon - name + 1;
			name = colon + 1;
		}

		rootOffset = outPtr - *canonicalName;

		// handle root slash
		*outPtr++ = '/';
		usize += 1;
	}
	else
	{
		usize = *usizePtr;
		outPtr = *canonicalName + offset - 2;

		if (usize == 3)
		{
			// root key -> re-use trailing slash
			--usize;
			++outPtr;
		}
		else
		{
			// not root key -> add trailing slash
			*++outPtr = '/';
			++outPtr;
		}

		// find root slash
		if (**canonicalName == '/')
		{
			rootOffset = 0;
		}
		else
		{
			rootOffset = strchr (*canonicalName, ':') - *canonicalName + 1;
		}
	}

	while (*name != '\0')
	{
		// STATE: name -- on part separator or first char of part
		//        outPtr -- start of new part
		//        usize -- namespace + separator + previous parts + separator
		//     -> skip part separator
		if (*name == '/')
		{
			++name;
		}

		// STATE: name -- first char of part
		//        outPtr -- start of new part
		//        usize -- namespace + separator + previous parts + separator
		//     -> skip all leading slashes ...
		while (*name == '/')
		{
			++name;
		}

		// ... then check for special part
		switch (*name)
		{
		case '\0':
			// STATE: name -- end of string
			//        outPtr -- trailing slash
			//        usize -- namespace + separator + previous parts + separator
			//     -> next iteration: loop will end, trailing slash will be replaced with terminator
			continue;
		case '.':
			switch (*(name + 1))
			{
			case '/':
			case '\0':
				// dot-part -> skip
				++name;
				// STATE: name -- part separator
				//        outPtr -- trailing slash
				//        usize -- namespace + separator + previous parts + separator
				//     -> next iteration: trailing slash will be overwritten
				continue;
			case '.':
				if (*(name + 2) == '/' || *(name + 2) == '\0')
				{
					// dot-dot-part -> go back one part

					// 1. terminate outPtr so that findStartOfLastPart works
					*outPtr = '\0';

					// 2. find start of previous part
					char * newOutPtr = findStartOfLastPart (*canonicalName, outPtr - *canonicalName);

					// 3. calculate unescaped length of part, including separator
					size_t ulen = outPtr - newOutPtr - 1;

					// 4. adjust pointers
					name += 2;
					outPtr = newOutPtr + 1;

					// 5. if previous part is empty ('%') ...
					if (ulen == 2 && *newOutPtr + 1 == '%')
					{
						// 5a. ... then adjust len
						ulen = 1;
					}
					else
					{
						// 5b. ... else account of escape sequences
						size_t escapes = 0;
						for (size_t i = 1; i + 1 < ulen; ++i)
						{
							if (*(newOutPtr + i) == '\\')
							{
								++escapes;
								++i; // skip next character
							}
						}
						ulen -= escapes;
					}

					// 6. adjust usize
					ELEKTRA_ASSERT (ulen <= usize - 2, "usize underflow");
					usize -= ulen;

					// STATE: name -- part separator
					//        outPtr -- start of this part
					//        usize -- namespace + separator + parts before previous part + separator
					//     -> next iteration: part will be overwritten
					continue;
				}
				break;
			}
			break;
		case '%':
			if (*(name + 1) == '/' || *(name + 1) == '\0')
			{
				// empty part -> copy but only count / in usize
				++name;
				*outPtr++ = '%';
				*outPtr++ = '/';
				usize++;

				// STATE: name -- on part separator or end of string
				//        outPtr -- after last part
				//        usize -- namespace + separator + previous parts + separator + (empty part) + separator
				continue;
			}
			break;
		case '#': {
			// find end of digits
			const char * end = name + 1;
			while (isdigit (*end))
			{
				++end;
			}
			size_t len = end - name - 1;

			bool check1 = *end == '/' || *end == '\0';
			bool check2 = *(name + 1) != '0';

			if (len > 0 && check1 && check2 && (len < 19 || (len == 19 && strncmp (name + 1, "9223372036854775807", 19) <= 0)))
			{
				// non-canonical array part -> add underscores

				// but first update buffer
				size_t pos = outPtr - *canonicalName;

				*canonicalSizePtr += offset + len - 1;
				elektraRealloc ((void **) canonicalName, *canonicalSizePtr);
				outPtr = *canonicalName + pos;

				*outPtr = '#';
				memset (outPtr + 1, '_', len - 1);
				memcpy (outPtr + len, name + 1, len);
				outPtr += 2 * len;
				*outPtr++ = '/';
				usize += 2 * len + 1;
				name = end;

				// STATE: name -- part separator
				//        outPtr -- after this part
				//        usize -- namespace + separator + previous parts + separator + current part + separator
				//     -> next iteration: new part will be started
				continue;
			}
			break;
		}
		}

		// STATE: name -- first char of non-special part
		//        outPtr -- start of new part
		//        usize -- previous parts + separator
		//     -> find end of part
		const char * end = name;
		while (*end != '\0')
		{
			size_t backslashes = 0;
			while (*end == '\\')
			{
				++backslashes;
				++end;
			}

			usize += backslashes / 2;
			if (*end == '\0' || (*end == '/' && backslashes % 2 == 0))
			{
				break;
			}

			++end;
			usize += 1;
		}

		size_t len = end - name;

		// STATE: name -- start of part
		//        end -- end of part
		//        outPtr -- start of new part
		//        usize -- previous parts + separator + current part
		//     -> copy part +  part separator
		memcpy (outPtr, name, len);
		outPtr += len;
		*outPtr++ = '/';
		usize += 1;
		name += len;

		// STATE: name -- on part separator or end of string
		//        outPtr -- after last part
		//        usize -- namespace + separator + previous parts + separator + current part + separator
	}

	// terminate
	if (outPtr > *canonicalName + rootOffset + 1)
	{
		// not a root key -> need to replace trailing slash
		--outPtr;
	}
	else if (offset == 0 || usize == 2)
	{
		// root key -> don't replace slash, need to adjust usize
		++usize;
	}
	*outPtr = '\0';

	// output size and shrink buffer
	*canonicalSizePtr = outPtr - *canonicalName + 1;
	elektraRealloc ((void **) canonicalName, *canonicalSizePtr);

	// output unescape size if requested
	if (usizePtr != NULL)
	{
		*usizePtr = usize;
	}
}

/**
 * Takes a canonical key name and unescapes it.
 *
 * @param canonicalName The canonical name to unescape
 * @param unescapedName Output buffer for the unescaped name
 *
 * @pre @p canonicalName MUST be a canonical key name. If this is not the case, the result is undefined.
 * @pre @p unescapedName MUST be allocated to the correct size.
 *
 * @see elektraKeyNameCanonicalize
 *
 * @ingroup keyname
 */
void elektraKeyNameUnescape (const char * canonicalName, char * unescapedName)
{
	char * outPtr = unescapedName;

	if (canonicalName[0] != '/')
	{
		// translate namespace
		const char * colon = strchr (canonicalName, ':');
		*outPtr++ = elektraReadNamespace (canonicalName, colon - canonicalName);
		canonicalName = colon + 1;
	}
	else
	{
		// set cascading namespace
		*outPtr++ = KEY_NS_CASCADING;
	}

	while (*canonicalName != '\0')
	{
		switch (*canonicalName)
		{
		case '\\':
			// escape sequence: skip backslash ...
			++canonicalName;

			// ... and output escaped char
			*outPtr++ = *canonicalName;
			++canonicalName;
			break;
		case '/':
			// new part: output zero-byte part separator and ...
			*outPtr++ = '\0';
			++canonicalName;

			// ... check for empty part
			if (*canonicalName == '%' && (*(canonicalName + 1) == '/' || *(canonicalName + 1) == '\0'))
			{
				// skip percent
				++canonicalName;
			}
			break;
		default:
			// nothing special, just output
			*outPtr++ = *canonicalName;
			++canonicalName;
			break;
		}
	}

	// terminate
	*outPtr = '\0';
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
 * Key names that have only root names (e.g. @c "system:/" or @c "user:/"
 * or @c "user:domain" ) does not have basenames, thus the function will
 * return 1 bytes to store "".
 *
 * Basenames are denoted as:
 * - @c system:/some/thing/basename -> @c basename
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
 * - basename of @c system:/some/keyname is @c keyname
 * - basename of @c "user:/tmp/some key" is @c "some key"
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
	if (key == NULL || returned == NULL) return -1;
	if (maxSize == 0 || maxSize > SSIZE_MAX) return -1;

	if (key->key == NULL)
	{
		*returned = '\0';
		return 1;
	}

	const char * baseName = keyBaseName (key);
	if (baseName == NULL)
	{
		return -1;
	}

	size_t baseSize = strlen (baseName) + 1;
	if (baseSize > maxSize)
	{
		return -1;
	}

	memcpy (returned, baseName, baseSize);
	return baseSize;
}

/**
 * Takes a single key name part and produces its escaped form.
 *
 * @param part A single key name part, i.e. contained '/' will be escaped, '\0' terminates part
 * @param escapedPart Output buffer for the escaped form
 *
 * @pre @p escapedPart MUST be a valid first argument for elektraRealloc() when cast to `void**`
 *
 * @returns The size of the escaped form excluding the zero terminator
 */
size_t elektraKeyNameEscapePart (const char * part, char ** escapedPart)
{
	if (!part) return 0;

	size_t partLen = strlen (part);

	// first check for special parts
	if (partLen == 0)
	{
		// actually empty part
		elektraRealloc ((void **) escapedPart, 2);
		strcpy (*escapedPart, "%");
		return 1;
	}

	switch (part[0])
	{
	case '%':
		if (partLen == 1)
		{
			// escaped empty part
			elektraRealloc ((void **) escapedPart, 3);
			strcpy (*escapedPart, "\\%");
			return 2;
		}
		break;
	case '.':
		switch (part[1])
		{
		case '\0':
			// dot part
			elektraRealloc ((void **) escapedPart, 3);
			strcpy (*escapedPart, "\\.");
			return 2;
		case '.':
			if (partLen == 2)
			{
				// dot-dot part
				elektraRealloc ((void **) escapedPart, 4);
				strcpy (*escapedPart, "\\..");
				return 3;
			}
			break;
		}
		break;
	case '#':
		if (partLen > 1)
		{
			// possibly (non-)canonical array part
			size_t digits = 0;
			while (isdigit (part[1 + digits]))
			{
				++digits;
			}

			if (digits > 1 && part[1] != '0' &&
			    (digits < 19 || (digits == 19 && strncmp (&part[1], "9223372036854775807", 19) <= 0)))
			{
				// non-canonical array part -> need to escape
				elektraRealloc ((void **) escapedPart, partLen + 2);
				**escapedPart = '\\';
				memcpy (*escapedPart + 1, part, partLen + 1);
				return partLen + 1;
			}
		}
		break;
	}

	// not a special part -> just escape backslash and slash
	size_t special = 0;
	const char * cur = part;
	while ((cur = strpbrk (cur, "/\\")) != NULL)
	{
		++special;
		++cur;
	}

	elektraRealloc ((void **) escapedPart, partLen + special + 1);

	cur = part;
	char * outPtr = *escapedPart;
	while (*cur != '\0')
	{
		if (*cur == '/' || *cur == '\\')
		{
			*outPtr++ = '\\';
		}
		*outPtr++ = *cur++;
	}
	*outPtr = '\0';

	return outPtr - *escapedPart;
}

/**
 * Internal helper for keyAddBaseName() and keySetBaseName()
 */
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
 * So if @p key has name @c "system:/dir1/dir2" and this method is called with
 * @p baseName @c "mykey", the resulting key will have the name
 * @c "system:/dir1/dir2/mykey".
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
	if (!key) return -1;
	if (!baseName) return -1;
	if (test_bit (key->flags, KEY_FLAG_RO_NAME)) return -1;
	if (!key->key) return -1;

	return keyAddBaseNameInternal (key, baseName);
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
 * Let us suppose @p key has name @c "system:/dir1/dir2/key1". If @p baseName
 * is @c "key2", the resulting key name will be @c "system:/dir1/dir2/key2".
 * If @p baseName is 0 (NULL), the resulting key name will
 * be @c "system:/dir1/dir2".
 * If @p baseName is empty, the resulting key name will
 * be @c "system:/dir1/dir2/%", where @c "%" denotes an empty base name,
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
	if (!key) return -1;
	if (test_bit (key->flags, KEY_FLAG_RO_NAME)) return -1;
	if (!key->key) return -1;

	// adjust sizes to exclude base name
	const char * baseNamePtr = findStartOfLastPart (key->key, key->keySize);
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

	if (key->keyUSize == 2)
	{
		// happens if we only have one part after namespace
		++key->keySize;
		++key->keyUSize;
	}

	// add new base name, only resizes buffer when baseName == NULL
	return keyAddBaseNameInternal (key, baseName);
}

/**
 * For currently valid namespaces see #elektraNamespace.
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
 *
 * @ingroup keyname
 */
elektraNamespace keyGetNamespace (const Key * key)
{
	if (!key) return KEY_NS_NONE;
	return (elektraNamespace) key->ukey[0];
}

/**
 * Changes the namespace of a key. The rest of the name remains unchanged.
 *
 * @param key The #Key whose namespace will be changed
 * @param ns  The new namespace of for @p key
 *
 * @pre @p ns MUST be a valid namespace and not #KEY_NS_NONE
 * @pre @p key MUST be a valid #Key, especially `key != NULL`
 *
 * @retval -1 precondition error
 * @returns the new size the @p{key}'s escaped name
 *
 * @ingroup keyname
 */
ssize_t keySetNamespace (Key * key, elektraNamespace ns)
{
	if (!key) return -1;
	if (ns == KEY_NS_NONE) return -1;

	if (ns == key->ukey[0]) return key->keySize;

	size_t oldNamespaceLen;
	switch (key->ukey[0])
	{
	case KEY_NS_USER:
		oldNamespaceLen = sizeof ("user:") - 1;
		break;
	case KEY_NS_SYSTEM:
		oldNamespaceLen = sizeof ("system:") - 1;
		break;
	case KEY_NS_DIR:
		oldNamespaceLen = sizeof ("dir:") - 1;
		break;
	case KEY_NS_META:
		oldNamespaceLen = sizeof ("meta:") - 1;
		break;
	case KEY_NS_SPEC:
		oldNamespaceLen = sizeof ("spec:") - 1;
		break;
	case KEY_NS_PROC:
		oldNamespaceLen = sizeof ("proc:") - 1;
		break;
	case KEY_NS_CASCADING:
		oldNamespaceLen = 0;
		break;
	case KEY_NS_DEFAULT:
		oldNamespaceLen = sizeof ("default:") - 1;
		break;
	default:
		return -1;
	}

	const char * newNamespace;
	switch (ns)
	{
	case KEY_NS_USER:
		newNamespace = "user:";
		break;
	case KEY_NS_SYSTEM:
		newNamespace = "system:";
		break;
	case KEY_NS_DIR:
		newNamespace = "dir:";
		break;
	case KEY_NS_META:
		newNamespace = "meta:";
		break;
	case KEY_NS_SPEC:
		newNamespace = "spec:";
		break;
	case KEY_NS_PROC:
		newNamespace = "proc:";
		break;
	case KEY_NS_CASCADING:
		newNamespace = "";
		break;
	case KEY_NS_DEFAULT:
		newNamespace = "default:";
		break;
	default:
		return -1;
	}

	size_t newNamespaceLen = strlen (newNamespace);

	if (newNamespaceLen > oldNamespaceLen)
	{
		// buffer growing -> realloc first
		elektraRealloc ((void **) &key->key, key->keySize - oldNamespaceLen + newNamespaceLen);
	}

	memmove (key->key + newNamespaceLen, key->key + oldNamespaceLen, key->keySize - oldNamespaceLen);

	if (newNamespaceLen < oldNamespaceLen)
	{
		// buffer growing -> realloc after
		elektraRealloc ((void **) &key->key, key->keySize - oldNamespaceLen + newNamespaceLen);
	}

	memcpy (key->key, newNamespace, newNamespaceLen);
	key->keySize += newNamespaceLen;
	key->keySize -= oldNamespaceLen;
	key->key[key->keySize - 1] = '\0';

	key->ukey[0] = ns;

	return key->keySize;
}


/**
 * @}
 */
