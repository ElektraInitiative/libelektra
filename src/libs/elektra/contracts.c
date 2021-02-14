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

	ksAppendKey (contract, keyNew ("system:/elektra/contract/globalkeyset/gopts/parent", KEY_VALUE, keyName (parentKey), KEY_END));
	ksAppendKey (contract, keyNew ("system:/elektra/contract/globalkeyset/gopts/argc", KEY_BINARY, KEY_SIZE, sizeof (int), KEY_VALUE,
				       &argc, KEY_END));
	ksAppendKey (contract, keyNew ("system:/elektra/contract/globalkeyset/gopts/argv", KEY_BINARY, KEY_SIZE,
				       sizeof (const char * const *), KEY_VALUE, &argv, KEY_END));
	ksAppendKey (contract, keyNew ("system:/elektra/contract/globalkeyset/gopts/envp", KEY_BINARY, KEY_SIZE,
				       sizeof (const char * const *), KEY_VALUE, &envp, KEY_END));
	return 0;
}

/**
 * Sets up a contract for use with kdbOpen() that configures
 * the gopts plugin.
 *
 * NOTE: prefer to use elektraGOptsContract() if possible
 *
 * @param contract    The KeySet into which the contract will be written.
 * @param argsSize    The size of the @p args data
 * @param args        Continuous buffer containing all argv arguments
 *                    separated (and terminated) by zero bytes.
 *                    The whole buffer is copied, so the pointer only has to
 *                    be valid for this function call.
 * @param envSize     The size of the @p env data
 * @param env         Continuous buffer containing all environment variables
 *                    separated (and terminated) by zero bytes
 *                    The whole buffer is copied, so the pointer only has to
 *                    be valid for this function call.
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
int elektraGOptsContractFromStrings (KeySet * contract, size_t argsSize, const char * args, size_t envSize, const char * env,
				     const Key * parentKey, KeySet * goptsConfig)
{
	if (contract == NULL || args == NULL || env == NULL || parentKey == NULL) return -1;

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

	ksAppendKey (contract, keyNew ("system:/elektra/contract/globalkeyset/gopts/parent", KEY_VALUE, keyName (parentKey), KEY_END));
	ksAppendKey (contract,
		     keyNew ("system:/elektra/contract/globalkeyset/gopts/args", KEY_BINARY, KEY_SIZE, argsSize, KEY_VALUE, args, KEY_END));
	ksAppendKey (contract,
		     keyNew ("system:/elektra/contract/globalkeyset/gopts/env", KEY_BINARY, KEY_SIZE, envSize, KEY_VALUE, env, KEY_END));
	return 0;
}
