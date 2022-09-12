/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "xmltool.h"

#include <stdlib.h>
#include <string.h>

static ElektraKey * commonParent (ElektraKey * firstKey, ElektraKey * secondKey, size_t maxSize)
{
	// First we find the common prefix of the first two keys.
	// NOTE: a common prefix is not necessarily a common parent
	//   e.g. system:/abc/d is a common prefix of system:/abc/de and system:/abc/df,
	//   but the common parent would be system:/abc

	const char * firstName = keyName (firstKey);
	const char * secondName = keyName (secondKey);

	size_t commonLength = 0;
	for (size_t i = 0; i < maxSize; ++i)
	{
		if (firstName[i] == '\0' || secondName[i] == '\0')
		{
			break;
		}

		if (firstName[i] != secondName[i])
		{
			break;
		}

		commonLength = i + 1;
	}

	if (commonLength == 0)
	{
		return NULL;
	}

	// We now extract the common prefix ...
	char * commonPrefix = strndup (firstName, commonLength);

	// ... and adjust it to a common parent.
	ElektraKey * common = keyNew (commonPrefix, ELEKTRA_KEY_END);
	if (commonPrefix[commonLength - 1] != '/')
	{
		keySetBaseName (common, NULL);
	}

	free (commonPrefix);

	if ((size_t) keyGetNameSize (common) > maxSize)
	{
		keyDel (common);
		return NULL;
	}

	return common;
}


/**
 * @internal
 *
 * Calculates the common parent to all keys in @p ks.
 *
 * This is a c-helper function, you need not implement it in bindings.
 *
 * Given the @p ks KeySet, calculates the parent name for all the keys.
 * So if @p ks contains these keys:
 *
 * @code
 *   system:/sw/xorg/Monitors/Monitor1/vrefresh
 *   system:/sw/xorg/Monitors/Monitor1/hrefresh
 *   system:/sw/xorg/Devices/Device1/driver
 *   system:/sw/xorg/Devices/Device1/mode
 * @endcode
 *
 * The common parent is @p system:/sw/xorg .
 *
 * On the other hand, if we have this KeySet:
 *
 * @code
 *   system:/some/thing
 *   system:/other/thing
 *   user:/unique/thing
 * @endcode
 *
 * No common parent is possible, so @p returnedCommonParent will contain nothing.
 *
 * @param working the Keyset to work with
 * @param returnedCommonParent a pre-allocated buffer that will receive the
 *        common parent, if found
 * @param maxSize size of the pre-allocated @p returnedCommonParent buffer
 * @return size in bytes of the parent name, or 0 if there is no common parent (with length <= maxSize)
 */
size_t ksGetCommonParentName (ElektraKeyset * working, char * returnedCommonParent, size_t maxSize)
{
	if (maxSize > SSIZE_MAX) return 0;
	if (ksGetSize (working) < 1) return 0;

	if (ksGetSize (working) == 1)
	{
		return keyGetName (ksAtCursor (working, 0), returnedCommonParent, maxSize);
	}

	// Get common parent of first two keys in the KeySet.

	ElektraKey * common = commonParent (ksAtCursor (working, 0), ksAtCursor (working, 1), maxSize);

	if (common == NULL)
	{
		*returnedCommonParent = '\0';
		return 0;
	}

	// We then check if all keys in the KeySet are below the parent we found.
	ElektraKeyset * cut = ksCut (working, common);

	while (ksGetSize (working) != 0)
	{
		// If not all keys match, we find the common prefix of common and the first non-matching key.
		ElektraKey * nextKey = ksAtCursor (working, 0);

		ksAppend (working, cut);
		ksDel (cut);

		ElektraKey * newCommon = commonParent (common, nextKey, maxSize);

		keyDel (common);
		common = newCommon;

		if (common == NULL)
		{
			*returnedCommonParent = '\0';
			return 0;
		}

		cut = ksCut (working, common);
	}

	ksAppend (working, cut);
	ksDel (cut);

	ssize_t ret = keyGetName (common, returnedCommonParent, maxSize);
	keyDel (common);
	return ret;
}
