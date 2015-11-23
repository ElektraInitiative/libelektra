/**
 * @file
 *
 * @brief Implementation of proposed API enhancements.
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <string.h>

#include <kdbprivate.h>


/**
 * @brief Takes the first key and cuts off this common part
 * for all other keys, instead name will be prepended
 *
 * @return a new allocated keyset with keys in user namespace.
 *
 * The first key is removed in the resulting keyset.
 */
KeySet* elektraRenameKeys(KeySet *config, const char* name)
{
	Key *root;
	Key *cur;
	ssize_t rootSize = 0;

	ksRewind(config);

	root = ksNext (config);
	rootSize = keyGetNameSize(root);

	keyDel (ksLookup (config, root, KDB_O_POP));

	KeySet *newConfig = ksNew(ksGetSize(config), KS_END);
	if (rootSize == -1) return newConfig;

	while ((cur = ksPop(config)) != 0)
	{
		Key *dupKey = keyDup(cur);
		keySetName(dupKey, name);
		keyAddName(dupKey, keyName(cur)+rootSize-1);
		ksAppendKey(newConfig, dupKey);
		keyDel(cur);
	}

	return newConfig;
}
