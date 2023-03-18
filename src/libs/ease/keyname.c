/**
 * @file
 *
 * @brief Methods for accessing key names.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb.h>

#include <string.h>

/**
 * @brief get relative position of key based on parentKey
 *
 * @pre  parentKey is either the same key as cur, or one of its parents
 * @post a pointer to the relevant part of the parent key's name, the full
 * name if there is no relation to the parentKey
 *
 * If the parentKey does not fulfill the precondition, the result won't be the
 * correct relative key of cur.
 *
 * @param cur the key below parentKey we want to get the relative basename of
 * @param parentKey the key that defines the root/base
 *
 * @return a pointer to the relative part name of the key cur
 */
const char * elektraKeyGetRelativeName (Key const * cur, Key const * parentKey)
{
	if (cur == NULL || parentKey == NULL) return NULL;

	ssize_t offset = 0;

	if (strcmp (keyName (parentKey), "/") != 0)
	{
		offset = keyGetNameSize (parentKey);
		if (keyGetUnescapedNameSize (parentKey) == 3)
		{
			--offset;
		}
		if (keyName (parentKey)[0] == '/' && keyName (cur)[0] != '/')
		{
			offset += strstr (keyName (cur), keyName (parentKey)) - keyName (cur);
		}
	}
	if (offset == keyGetNameSize (cur))
	{
		offset = keyGetNameSize (cur) - 1; // equality of the keys
	}
	else if (offset > keyGetNameSize (cur))
	{
		offset = 0; // no relation or invalid arguments, return full name
	}
	return keyName (cur) + offset;
}
