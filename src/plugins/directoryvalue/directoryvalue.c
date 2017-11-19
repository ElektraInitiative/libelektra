/**
 * @file
 *
 * @brief Source for directoryvalue plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "directoryvalue.h"

#include <kdbassert.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>

/**
 * @brief This function returns a key set containing the contract of this plugin.
 *
 * @return A contract describing the functionality of this plugin.
 */
static KeySet * directoryValueContract (void)
{
	return ksNew (30,
		      keyNew ("system/elektra/modules/directoryvalue", KEY_VALUE, "directoryvalue plugin waits for your orders", KEY_END),
		      keyNew ("system/elektra/modules/directoryvalue/exports", KEY_END),
		      keyNew ("system/elektra/modules/directoryvalue/exports/get", KEY_FUNC, elektraDirectoryvalueGet, KEY_END),
		      keyNew ("system/elektra/modules/directoryvalue/exports/set", KEY_FUNC, elektraDirectoryvalueSet, KEY_END),
#include ELEKTRA_README (directoryvalue)
		      keyNew ("system/elektra/modules/directoryvalue/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
}

/**
 * @brief Convert `key` to a leaf value and copy it to `output`.
 *
 * - If `key` is already a leaf value it will not be changed and directly copied to `output`.
 * - If `key` is a directory value the function creates a new leaf value storing the data of the old directory key. The function also
 *   creates another empty node with the name of the directory value in this case. Both of these key will be copied to `output`.
 *
 * @pre The parameters `output`, `next` and `parent` must not be `NULL`.
 *
 * @param output This parameter stores the values this function creates.
 * @param key This parameter stores the key this function operates on.
 * @param next This parameter stores the key directly after `key`.
 * @param parent This parameter stores the parent key of `keySet`. This function uses this key to emit error information.
 *
 * @retval ELEKTRA_PLUGIN_STATUS_NO_UPDATE if everything went fine and the function copied `key` without any modifications to `output`.
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if everything went fine and the function converted `key` to a leaf value.
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if the function was unable to convert `key`.
 */
static int addDirectoryValue (KeySet * output, Key const * const key, Key const * const next, Key * const parent)
{
	ELEKTRA_NOT_NULL (next);
	ELEKTRA_NOT_NULL (parent);
	ELEKTRA_NOT_NULL (output);

	if (!key)
	{
		ELEKTRA_LOG_DEBUG ("Key `key` is null");
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}
	if (!keyIsBelow (key, next))
	{
		ELEKTRA_LOG_DEBUG ("Key %s is a leaf value", keyName (key));
		ksAppendKey (output, keyDup (key));

		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}

	Key * directoryKey = keyNew (keyName (key), KEY_END);
	Key * dataKey = keyDup (key);
	if (keyAddBaseName (dataKey, "___dirdata") < 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_DIRECTORY_VALUE_APPEND, parent, "Could not append directory postfix to “%s”",
				    keyName (dataKey));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	ELEKTRA_ASSERT (ksAppendKey (output, directoryKey) >= 0, "Could not append directory key");
	ELEKTRA_ASSERT (ksAppendKey (output, dataKey) >= 0, "Could not append data key");
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

// ====================
// = Plugin Interface =
// ====================

/** @see elektraDocGet */
int elektraDirectoryvalueGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/directoryvalue"))
	{
		KeySet * contract = directoryValueContract ();
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

/** @see elektraDocSet */
int elektraDirectoryvalueSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parent)
{
	ksRewind (returned);
	Key * current = NULL;
	Key * next;
	int status = 0;
	KeySet * output = ksNew (0, KS_END);

	while (status >= 0 && (next = ksNext (returned)) != NULL)
	{
		status |= addDirectoryValue (output, current, next, parent);
		current = next;
	}
	// Last key is always a leaf value
	if (current) ksAppendKey (output, current);

	ksCopy (returned, output);
	ksDel (output);
	return status;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (directoryvalue)
{
	return elektraPluginExport ("directoryvalue", ELEKTRA_PLUGIN_GET, &elektraDirectoryvalueGet, ELEKTRA_PLUGIN_SET,
				    &elektraDirectoryvalueSet, ELEKTRA_PLUGIN_END);
}
