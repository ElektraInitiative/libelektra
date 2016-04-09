/**
 * @file
 *
 * @brief Methods for accessing key names.
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
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
	const size_t parentSize = keyGetNameSize (parentKey);

	if (!strcmp (keyName (parentKey), "/"))
	{
		return keyName (cur);
	}
	else if (keyName (parentKey)[0] == '/' && keyName (cur)[0] != '/')
	{
		size_t offset = strchr (keyName (cur) + 1, '/') - keyName (cur);
		return keyName (cur) + strlen (keyName (parentKey)) + offset + 1;
	}
	else
	{
		return keyName (cur) + parentSize;
	}
}
