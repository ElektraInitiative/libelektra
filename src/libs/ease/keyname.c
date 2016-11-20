/**
 * @file
 *
 * @brief Methods for accessing key names.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <kdb.h>

#include <string.h>

/**
 * @brief get relative position of key based on parentKey
 *
 * @param cur the key below parentKey we want to get the relative basename of
 * @param parentKey the key that defines the root/base
 *
 * @return a pointer to the name of the key cur
 */
const char * elektraKeyGetRelativeName (Key const * cur, Key const * parentKey)
{
	size_t offset = 0;

	if (strcmp (keyName (parentKey), "/"))
	{
		offset = keyGetNameSize (parentKey);
		if (keyName (parentKey)[0] == '/' && keyName (cur)[0] != '/')
		{
			offset += strstr (keyName (cur), keyName (parentKey)) - keyName (cur);
		}
	}
	return keyName (cur) + offset;
}
