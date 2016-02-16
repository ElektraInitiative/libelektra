/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include "xmltool.h"

#include <string.h>



/**
 * @internal
 *
 * Calculates the common parent to all keys in @p ks.
 *
 * This is a c-helper function, you need not implement it in bindings.
 *
 * Given the @p ks KeySet, calculates the parent name for all the keys.
 * So if @p ks contains this keys:
 *
 * @code
 *   system/sw/xorg/Monitors/Monitor1/vrefresh
 *   system/sw/xorg/Monitors/Monitor1/hrefresh
 *   system/sw/xorg/Devices/Device1/driver
 *   system/sw/xorg/Devices/Device1/mode
 * @endcode
 *
 * The common parent is @p system/sw/xorg .
 *
 * On the other hand, if we have this KeySet:
 *
 * @code
 *   system/some/thing
 *   system/other/thing
 *   user/unique/thing
 * @endcode
 *
 * No common parent is possible, so @p returnedCommonParent will contain nothing.
 *
 * @param working the Keyset to work with
 * @param returnedCommonParent a pre-allocated buffer that will receive the
 *        common parent, if found
 * @param maxSize size of the pre-allocated @p returnedCommonParent buffer
 * @return size in bytes of the parent name, or 0 if there is no common parent,
 *         or -1 to indicate an error, then @p errno must be checked.
 */
ssize_t ksGetCommonParentName(const KeySet *working, char *returnedCommonParent, size_t maxSize)
{
	size_t parentSize=0;
	Key *current=0;
	cursor_t cinit;
	KeySet *ks;
	ssize_t sMaxSize;

	if (maxSize > SSIZE_MAX) return -1;
	sMaxSize = maxSize;

	cinit = ksGetCursor (working);
	ks = (KeySet *) working;

	if (ksGetSize(ks) < 1) return 0;

	ksRewind(ks);
	current = ksNext(ks);
	if (keyGetNameSize(current) > sMaxSize)
	{
		/*errno=KDB_ERR_TRUNC;*/
		returnedCommonParent[0]=0;
		return -1;
	}

	strcpy(returnedCommonParent,keyName(current));
	parentSize=elektraStrLen(returnedCommonParent);

	while (*returnedCommonParent)
	{
		ksRewind(ks);
		while ((current = ksNext(ks)) != 0)
		{
			/* Test if a key doesn't match */
			if (memcmp(returnedCommonParent,keyName(current),parentSize-1)) break;
		}
		if (current)
		{
			/* some key failed to be a child */
			/* parent will be the parent of current parent... */
			char *delim=0;

			// TODO: does not honor escaped characters
			if ((delim=strrchr(returnedCommonParent,KDB_PATH_SEPARATOR)))
			{
				*delim=0;
				parentSize=elektraStrLen(returnedCommonParent);
			} else {
				*returnedCommonParent=0;
				parentSize=0;
				break; /* Better don't make comparision with parentSize-1 now */
			}
		} else {
			/* All keys matched (current==0) */
			/* We have our common parent to return in commonParent */
			ksSetCursor (ks, cinit);
			return parentSize;
		}
	}
	ksSetCursor (ks, cinit);
	return parentSize; /* if reached, will be zero */
}
