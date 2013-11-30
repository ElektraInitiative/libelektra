#include "array.h"

#include <stdlib.h>
#include <stdio.h>

/**
 * @brief Increment the name of the key by one
 *
 * Alphabetical order will remain
 *
 * e.g. user/abc/#9 will be changed to
 *      user/abc/#_10
 *
 * TODO: add elektraArrayIncName for kdb tool
 * use it for other arrays within elektra
 * add more test cases
 *
 *
 * @param key which base name will be incremented
 *
 * @retval -1 on error
 * @retval 0 on success
 */
int elektraArrayIncName(Key *key)
{
	if (!key)
	{
		return -1;
	}

	const char * baseName = keyBaseName(key);
	if (!baseName)
	{
		return -1;
	}
	else if (*baseName != '#')
	{
		return -1;
	}

	++baseName; // jump over #
	while(*baseName == '_') // jump over all _
	{
		++baseName;
	}

	// TODO: implement error handling for non-numbers
	// needs also ajustment in jumping over all _ to
	// be robust against any text
	int oldIndex = atoi(baseName);
	int newIndex = oldIndex+1; // we increment by one

	// maximal size calculation (C99 would also allow non maximum though...)
	size_t sizeHash = 1;
	size_t sizeMax_ = 9; // maximum of n-1 _
	size_t sizeNum = 10; // maximum of 10 digits in 32bit number
	size_t size = sizeHash + sizeMax_ + sizeNum + 1;
	char newName[size]; // #_______________________________________________________4000000000

	// now we fill out newName
	size_t index = 0; // index of newName
	newName[index++] = '#';
	size_t size_=0;
	size_t i = newIndex/10;
	while (i>0)
	{
		size_++; // increment the number of decimals
		newName[index++] = '_'; // index n-1 of decimals
		i/=10;
	}
	if (snprintf (&newName[index], sizeNum, "%d", newIndex)  < 0)
	{
		return -1;
	}
	keySetBaseName(key, newName);

	return 0;
}
