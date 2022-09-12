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
 * You can pass 0 for @p argc **and** `NULL` for @p argv to let gopts lookup
 * command line options internally.
 * You can also pass `NULL` for @p envp to do the same for environment variables.
 *
 * @param contract    The KeySet into which the contract will be written.
 * @param argc        The argc value that should be used by gopts.
 * @param argv        The argv value that should be used by gopts.
 *                    IMPORTANT: The pointer and data behind must be valid
 *                    until after kdbClose() is called.
 * @param envp        The envp value that should be used by gopts.
 *                    IMPORTANT: The pointer and data behind must be valid
 *                    until after kdbClose() is called.
 * @param parentKey   The parent key that should be used by gopts.
 *                    Only the key name is copied. The key can be deleted
 *                    immediately after calling this function.
 * @param goptsConfig The config that used to mount the gopts plugin.
 * 		      This value can be NULL. Only keys in the user:/
 *                    namespace will be used.
 *
 * @retval -1 if @p contract or @p parentKey is NULL
 * @retval -1 if @p argc is 0 and @p argv is not NULL or if @p argc is not 0 and @p argv is NULL
 * @retval  0 on success
 */
int elektraGOptsContract (ElektraKeyset * contract, int argc, const char * const * argv, const char * const * envp, const ElektraKey * parentKey,
			  ElektraKeyset * goptsConfig)
{
	if (contract == NULL || (argc == 0) != (argv == NULL) || parentKey == NULL) return -1;

	ksAppendKey (contract, keyNew ("system:/elektra/contract/mountglobal/gopts", ELEKTRA_KEY_END));
	if (goptsConfig != NULL)
	{
		ElektraKey * configRoot = keyNew ("user:/", ELEKTRA_KEY_END);
		ElektraKey * contractRoot = keyNew ("system:/elektra/contract/mountglobal/gopts", ELEKTRA_KEY_END);

		elektraCursor end;
		for (elektraCursor it = ksFindHierarchy (goptsConfig, configRoot, &end); it < end; it++)
		{
			ElektraKey * renamed = keyDup (ksAtCursor (goptsConfig, it), ELEKTRA_KEY_CP_ALL);
			keyReplacePrefix (renamed, configRoot, contractRoot);
			ksAppendKey (contract, renamed);
		}

		keyDel (configRoot);
		keyDel (contractRoot);
	}

	ksAppendKey (contract, keyNew ("system:/elektra/contract/globalkeyset/gopts/parent", ELEKTRA_KEY_VALUE, keyName (parentKey), ELEKTRA_KEY_END));
	if (argc != 0)
	{
		ksAppendKey (contract, keyNew ("system:/elektra/contract/globalkeyset/gopts/argc", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, sizeof (int),
					       ELEKTRA_KEY_VALUE, &argc, ELEKTRA_KEY_END));
		ksAppendKey (contract, keyNew ("system:/elektra/contract/globalkeyset/gopts/argv", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE,
					       sizeof (const char * const *), ELEKTRA_KEY_VALUE, &argv, ELEKTRA_KEY_END));
	}

	if (envp != NULL)
	{
		ksAppendKey (contract, keyNew ("system:/elektra/contract/globalkeyset/gopts/envp", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE,
					       sizeof (const char * const *), ELEKTRA_KEY_VALUE, &envp, ELEKTRA_KEY_END));
	}
	return 0;
}

/**
 * Sets up a contract for use with kdbOpen() that configures
 * the gopts plugin.
 *
 * NOTE: prefer to use elektraGOptsContract() if possible
 *
 * You can pass `NULL` for @p args to let gopts lookup command line
 * options internally.
 * You can also pass `NULL` for @p env to do the same for environment
 * variables.
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
 * @param parentKey   The parent key that should be used by gopts.
 *                    Only the key name is copied. The key can be deleted
 *                    immediately after calling this function.
 * @param goptsConfig The config that used to mount the gopts plugin.
 * 		      This value can be NULL. Only keys in the user:/
 *                    namespace will be used.
 *
 * @retval -1 if any of @p contract or @p parentKey is NULL
 * @retval  0 on success
 */
int elektraGOptsContractFromStrings (ElektraKeyset * contract, size_t argsSize, const char * args, size_t envSize, const char * env,
				     const ElektraKey * parentKey, ElektraKeyset * goptsConfig)
{
	if (contract == NULL || parentKey == NULL) return -1;

	ksAppendKey (contract, keyNew ("system:/elektra/contract/mountglobal/gopts", ELEKTRA_KEY_END));
	if (goptsConfig != NULL)
	{
		ElektraKey * configRoot = keyNew ("user:/", ELEKTRA_KEY_END);
		ElektraKey * contractRoot = keyNew ("system:/elektra/contract/mountglobal/gopts", ELEKTRA_KEY_END);

		elektraCursor end;
		for (elektraCursor it = ksFindHierarchy (goptsConfig, configRoot, &end); it < end; it++)
		{
			ElektraKey * renamed = keyDup (ksAtCursor (goptsConfig, it), ELEKTRA_KEY_CP_ALL);
			keyReplacePrefix (renamed, configRoot, contractRoot);
			ksAppendKey (contract, renamed);
		}

		keyDel (configRoot);
		keyDel (contractRoot);
	}

	ksAppendKey (contract, keyNew ("system:/elektra/contract/globalkeyset/gopts/parent", ELEKTRA_KEY_VALUE, keyName (parentKey), ELEKTRA_KEY_END));
	if (args != NULL)
	{
		ksAppendKey (contract, keyNew ("system:/elektra/contract/globalkeyset/gopts/args", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, argsSize,
					       ELEKTRA_KEY_VALUE, args, ELEKTRA_KEY_END));
	}

	if (env != NULL)
	{
		ksAppendKey (contract, keyNew ("system:/elektra/contract/globalkeyset/gopts/env", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, envSize, ELEKTRA_KEY_VALUE,
					       env, ELEKTRA_KEY_END));
	}
	return 0;
}
