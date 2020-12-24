/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbopts.h>

#include <kdblogger.h>
#include <kdbprivate.h>

/**
 * WARNING: This function is experimental and may be removed, replaced or modified with breaking changes in a future release.
 *
 * Configure the gopts plugin to use the values provide to this function instead of auto-detecting them.
 *
 * The configuration is persisted in global keyset inside @p handle.
 * The caller is responsible for ensuring gopts is mounted, e.g. viw kdbEnsure().
 * Mounting gopts can be done before or after calling this function.
 *
 * If you want to reset the configuration, use NULL as @p parentKey.
 *
 * @param handle    The KDB handle for which gopts shall be configured.
 * @param parentKey The parent key under with which gopts will call elektraGetOpts().
 *                  If this value is NULL, any pre-existing will be removed.
 * @param argc      Number of elements in @p argv
 * @param argv      Command line arguments
 * @param envp      Environment variables
 *
 * @retval -1 If @p argc < 1 or if @p handle, @p argv or @p envp are NULL.
 * @retval  0 If the configuration was reset.
 * @retval  1 If the configuration was successfully saved.
 */
int elektraGOptsSetup (KDB * handle, Key * parentKey, int argc, const char * const * argv, const char * const * envp)
{
	if (handle == NULL)
	{
		ELEKTRA_LOG_DEBUG ("handle == NULL");
		return -1;
	}

	if (argc < 1)
	{
		ELEKTRA_LOG_DEBUG ("argc < 1");
		return -1;
	}

	if (argv == NULL)
	{
		ELEKTRA_LOG_DEBUG ("argv == NULL");
		return -1;
	}

	if (envp == NULL)
	{
		ELEKTRA_LOG_DEBUG ("envp == NULL");
		return -1;
	}

	if (parentKey == NULL)
	{
		keyDel (ksLookupByName (handle->global, "system:/elektra/gopts/parent", KDB_O_POP));
		keyDel (ksLookupByName (handle->global, "system:/elektra/gopts/argc", KDB_O_POP));
		keyDel (ksLookupByName (handle->global, "system:/elektra/gopts/argv", KDB_O_POP));
		keyDel (ksLookupByName (handle->global, "system:/elektra/gopts/envp", KDB_O_POP));
		return 0;
	}

	int ret = ksAppend (handle->global,
			    ksNew (4, keyNew ("system:/elektra/gopts/parent", KEY_VALUE, keyName (parentKey), KEY_END),
				   keyNew ("system:/elektra/gopts/argc", KEY_BINARY, KEY_SIZE, sizeof (int), KEY_VALUE, &argc, KEY_END),
				   keyNew ("system:/elektra/gopts/argv", KEY_BINARY, KEY_SIZE, sizeof (const char * const *), KEY_VALUE,
					   &argv, KEY_END),
				   keyNew ("system:/elektra/gopts/envp", KEY_BINARY, KEY_SIZE, sizeof (const char * const *), KEY_VALUE,
					   &envp, KEY_END),
				   KS_END));
	return ret >= 4 ? 1 : -1;
}