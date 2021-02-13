/**
 * @file
 *
 * @brief Contract constructors for kdbOpen()
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "kdb.h"
#include "kdbprivate.h"

#include <stddef.h>

/**
 * Sets up a contract for use with kdbOpen() that configures
 * the gopts plugin.
 *
 * @param contract    The KeySet into which the contract will be written.
 * @param argc        The argc value that should be used by gopts.
 * @param argv        The argv value that should be used by gopts.
 *                    IMPORTANT: The pointer and data behind must be valid
 *                    until after kdbClose() is called.
 * @param envp        The envp value that should be used by gopts.
 *                    IMPORTANT: The pointer and data behind must be valid
 *                    until after kdbClose() is called.
 * @param parentKey   The parent key that should be used by gopts
 *                    (only the name is used)
 * @param goptsConfig The config that used to mount the gopts plugin.
 * 		      This value can be NULL. Only keys in the user:/
 *                    namespace will be used.
 *
 * @retval -1 if any of @p contract, @p argv, @p envp, @p parentKey is NULL
 *            of if @p argc is 0
 * @retval  0 on success
 */
int elektraGOptsContract (KeySet * contract, int argc, const char * const * argv, const char * const * envp, const Key * parentKey,
			  KeySet * goptsConfig)
{
	if (contract == NULL || argc == 0 || argv == NULL || envp == NULL || parentKey == NULL) return -1;

	ksAppendKey (contract, keyNew ("system:/elektra/contract/mountglobal/gopts", KEY_END));
	if (goptsConfig != NULL)
	{
		Key * configRoot = keyNew ("user:/");
		Key * contractRoot = keyNew ("system:/elektra/contract/mountglobal/gopts");

		elektraCursor end;
		for (elektraCursor it = ksFindHierarchy (goptsConfig, configRoot, &end); it < end; it++)
		{
			Key * renamed = keyDup (ksAtCursor (goptsConfig, it));
			keyReplacePrefix (renamed, configRoot, contractRoot);
			ksAppendKey (contract, renamed);
		}

		keyDel (configRoot);
		keyDel (contractRoot);
	}

	ksAppendKey (contract,
		     keyNew ("system:/elektra/contract/globalkeyset/internal/gopts/parent", KEY_VALUE, keyName (parentKey), KEY_END));
	ksAppendKey (contract, keyNew ("system:/elektra/contract/globalkeyset/internal/gopts/argc", KEY_BINARY, KEY_SIZE, sizeof (int),
				       KEY_VALUE, &argc, KEY_END));
	ksAppendKey (contract, keyNew ("system:/elektra/contract/globalkeyset/internal/gopts/argv", KEY_BINARY, KEY_SIZE,
				       sizeof (const char * const *), KEY_VALUE, &argv, KEY_END));
	ksAppendKey (contract, keyNew ("system:/elektra/contract/globalkeyset/internal/gopts/envp", KEY_BINARY, KEY_SIZE,
				       sizeof (const char * const *), KEY_VALUE, &envp, KEY_END));
	return 0;
}
