/**
 * @file
 *
 * @brief Reference methods.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/ease/reference.h>
#include <internal/utility/compare.h>
#include <internal/utility/alloc.h>
#include <string.h>


/**
 * Check whether a reference is redundant (i.e. it can be expressed in less characters) or not.
 *
 * This can be used to give a warning to users, because redundant references are often
 * mistakes. Using "../some/key/../path" instead of "../some/path" may indicate, that either
 * a mistake was made while editing, or the concept of references was misunderstood.
 *
 * @param reference the reference to check
 * @retval 1 if the reference is redundant
 * @retval 0 otherwise
 */
int elektraIsReferenceRedundant (const char * reference)
{
	const char * cur = reference;
	while (strncmp (cur, "../", 3) == 0)
	{
		cur += 3;
	}

	return strstr (reference, "/./") != NULL || strstr (cur, "/../") != NULL ? 1 : 0;
}

/**
 * Resolve reference into a full keyname.
 *
 * References operate like UNIX paths, with some additions:
 *  - '.' refers to the current key
 *  - '..' refers to the parent of the current key
 *  - '@' refers to the parentKey of KDB
 *  - references starting with anything but './', '../' and '@/' as absolute,
 *    only embedded '/./' and '/../' as well as '/.' and '/..' at the end will
 *    be resolved (like in any keySetName call)
 *
 * @param reference The reference to resolve
 * @param baseKey   The key identified by the reference "./"
 * @param parentKey The key identified by the reference "@/"
 *
 * @return a newly allocated string, containing the full keyname;
 *         has to be disposed with elektraFree()
 */
char * elektraResolveReference (const char * reference, const Key * baseKey, const Key * parentKey)
{
	if (reference == NULL || strlen (reference) == 0)
	{
		return NULL;
	}

	Key * fullReference = keyNew ("/", KEY_END);

	if (elektraStrNCmp (reference, "@/", 2) == 0)
	{
		keySetName (fullReference, keyName (parentKey));
		keyAddName (fullReference, &reference[2]);
	}
	else if (elektraStrNCmp (reference, "./", 2) == 0)
	{
		keySetName (fullReference, keyName (baseKey));
		keyAddName (fullReference, &reference[2]);
	}
	else if (elektraStrNCmp (reference, "../", 3) == 0)
	{
		keySetName (fullReference, keyName (baseKey));
		keyAddName (fullReference, reference);
	}
	else
	{
		keySetName (fullReference, reference);
	}

	char * result = elektraStrDup (keyName (fullReference));
	keyDel (fullReference);

	return result;
}
