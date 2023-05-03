/**
 * @file
 *
 * @brief Source for xfconf plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./xfconf.h"
#include <elektra/kdb/errors.h>

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/ease/old_ease.h>
#include <internal/utility/logger.h>
#include <internal/utility/old_helper.h>
#include <xfconf/xfconf.h>

int elektraXfconfInit (Key * errorKey, int xfconfCode, int xfconfShutdown)
{
	ELEKTRA_LOG ("try to initialize xfconf\n");
	GError * err = NULL;
	if (xfconf_init (&err))
	{
		ELEKTRA_LOG_DEBUG ("succeed initialize xfconf\n");
		if (xfconfShutdown)
		{
			xfconf_shutdown ();
		}
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	else
	{
		ELEKTRA_LOG ("unable to initialize xfconf(%d): %s\n", err->code, err->message);
		int status = ELEKTRA_PLUGIN_STATUS_ERROR;
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, err->message);
		if (xfconfCode)
		{
			status = err->code;
		}
		g_error_free (err);
		if (xfconfShutdown)
		{
			xfconf_shutdown ();
		}
		return status;
	}
}

int elektraXfconfOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	return elektraXfconfInit (errorKey, 0, 0);
}

int elektraXfconfClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	xfconf_shutdown ();
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static void appendChannelList (KeySet * keySet)
{
	const char * absoluteKeyName = "system:/elektra/modules/xfconf/channels";
	Key * channelArrayKey = keyNew (absoluteKeyName, KEY_END);
	gchar ** channels = xfconf_list_channels ();
	if (channels == NULL)
	{
		return;
	}
	keySetMeta (channelArrayKey, "array", "");
	long channelIndex;
	for (channelIndex = 0; channels[channelIndex] != NULL; channelIndex++)
	{
		char * channelKeyName = elektraMalloc ((elektraStrLen (absoluteKeyName) + ELEKTRA_MAX_ARRAY_SIZE + 2) * sizeof (char));
		channelKeyName[0] = '\0';
		strcat (channelKeyName, absoluteKeyName);
		channelKeyName[elektraStrLen (absoluteKeyName) - 1] = '/';
		elektraWriteArrayNumber (&channelKeyName[elektraStrLen (absoluteKeyName)], channelIndex);
		Key * currentChannelKey = keyNew (channelKeyName, KEY_VALUE, channels[channelIndex], KEY_END);
		ksAppendKey (keySet, currentChannelKey);
	}
	if (channelIndex > 0)
	{
		char * arrayValue = elektraMalloc (ELEKTRA_MAX_ARRAY_SIZE * sizeof (char));
		elektraWriteArrayNumber (arrayValue, channelIndex - 1);
		keySetMeta (channelArrayKey, "array", arrayValue);
	}
	ksAppendKey (keySet, channelArrayKey);

	g_strfreev (channels);
}

int elektraXfconfGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	ELEKTRA_LOG ("issued get\n");
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/xfconf"))
	{
		ELEKTRA_LOG_DEBUG ("getting system modules values\n");
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/xfconf", KEY_VALUE, "xfconf plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/xfconf/exports", KEY_END),
			       keyNew ("system:/elektra/modules/xfconf/exports/open", KEY_FUNC, elektraXfconfOpen, KEY_END),
			       keyNew ("system:/elektra/modules/xfconf/exports/close", KEY_FUNC, elektraXfconfClose, KEY_END),
			       keyNew ("system:/elektra/modules/xfconf/exports/get", KEY_FUNC, elektraXfconfGet, KEY_END),
			       keyNew ("system:/elektra/modules/xfconf/exports/set", KEY_FUNC, elektraXfconfSet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/xfconf/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		appendChannelList (returned);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys
	KeySet * config = elektraPluginGetConfig (handle);
	const Key * channelKey = ksLookupByName (config, "/channel", KDB_O_NONE);
	const char * channelName = keyString (channelKey);
	ELEKTRA_LOG_DEBUG ("fetch keys from channel: %s\n", channelName);
	XfconfChannel * channel = xfconf_channel_get (channelName);
	if (channel == NULL)
	{
		ELEKTRA_LOG_DEBUG ("retrieved NULL attempting getting channel: %s\n", channelName);
	}
	GHashTable * properties = xfconf_channel_get_properties (channel, NULL);
	if (properties == NULL)
	{
		ELEKTRA_LOG_DEBUG ("retrieved NULL attempting getting properties\n");
	}
	GList * channelKeys = g_hash_table_get_keys (properties);
	const char * parentName = keyName (parentKey);
	while (channelKeys != NULL)
	{
		char * keyName = elektraStrDup (channelKeys->data);
		char * absoluteKeyName = elektraMalloc ((elektraStrLen (keyName) + elektraStrLen (parentName)) * sizeof (char));
		absoluteKeyName[0] = '\0';
		strcat (absoluteKeyName, parentName);
		strcat (absoluteKeyName, keyName);
		Key * key = keyNew (absoluteKeyName, KEY_END);
		GPtrArray * array = xfconf_channel_get_arrayv (channel, keyName);
		if (array != NULL)
		{
			ELEKTRA_LOG_DEBUG ("found non-null array with size %d\n", array->len);
			keySetMeta (key, "array", "");
			for (guint i = 0; i < array->len; i++)
			{
				GValue * val = g_ptr_array_index (array, i);
				char * arrayKeyName =
					elektraMalloc ((elektraStrLen (absoluteKeyName) + ELEKTRA_MAX_ARRAY_SIZE + 2) * sizeof (char));
				arrayKeyName[0] = '\0';
				strcat (arrayKeyName, absoluteKeyName);
				arrayKeyName[elektraStrLen (absoluteKeyName) - 1] = '/';
				elektraWriteArrayNumber (&arrayKeyName[elektraStrLen (absoluteKeyName)], i);
				Key * arrayKey = keyNew (arrayKeyName, KEY_END);
				const gchar * arrayKeyValue = g_value_get_string (val);
				ELEKTRA_LOG_DEBUG ("write to array key %s -> %s\n", arrayKeyName, arrayKeyValue);
				keySetString (arrayKey, arrayKeyValue); // todo: care about data types
				ksAppendKey (returned, arrayKey);
			}
		}
		else
		{
			GValue keyValue = G_VALUE_INIT;
			g_value_init (&keyValue, G_TYPE_STRING);
			xfconf_channel_get_property (channel, keyName, &keyValue);
			keySetString (key, g_value_get_string (&keyValue)); // todo: care about data types
			ELEKTRA_LOG_DEBUG ("found %s -> %s\n", keyName, g_value_get_string (&keyValue));
		}
		if (xfconf_channel_is_property_locked (channel, keyName))
		{
			keyLock (key, KEY_LOCK_META | KEY_LOCK_NAME | KEY_LOCK_VALUE);
		}
		ksAppendKey (returned, key);
		channelKeys = channelKeys->next;
	}
	g_list_free (channelKeys);
	g_hash_table_destroy (properties);
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraXfconfSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	ELEKTRA_LOG_DEBUG ("issued set with parent %s\n", keyName (parentKey));
	KeySet * config = elektraPluginGetConfig (handle);
	const Key * channelKey = ksLookupByName (config, "/channel", KDB_O_NONE);
	const char * channelName = keyString (channelKey);
	ELEKTRA_LOG_DEBUG ("using channel %s of parent %s\n", channelName, keyName (parentKey));

	XfconfChannel * channel = xfconf_channel_get (channelName);
	if (channel == NULL)
	{
		ELEKTRA_LOG_DEBUG ("retrieved NULL attempting getting channel: %s\n", channelName);
	}

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		Key * cur = ksNext (returned);
		const char * currentKeyName = elektraKeyGetRelativeName (cur, parentKey);
		if (currentKeyName == NULL)
		{
			// happens for the root key which holds the channel name
			ELEKTRA_LOG_DEBUG ("currentKeyName is null!\n");
			continue;
		}
		char * xfconfKeyName = elektraMalloc ((elektraStrLen (currentKeyName) + 2) * sizeof (char *));
		xfconfKeyName[0] = '/';
		strncpy (&xfconfKeyName[1], currentKeyName, elektraStrLen (currentKeyName));
		ELEKTRA_LOG_DEBUG ("setting key %s to %s\n", xfconfKeyName, keyString (cur));
		if (keyGetMeta (cur, "array") != NULL)
		{
			ELEKTRA_LOG_DEBUG ("key %s is an array\n", xfconfKeyName);
			KeySet * arrayKeySet = elektraArrayGet (cur, returned);
			Key * arrayKey;
			GPtrArray * xfconfArray = g_ptr_array_new ();
			while ((arrayKey = ksNext (arrayKeySet)) != NULL)
			{
				gchar * itemValue = elektraStrDup (keyString (arrayKey));
				keyDel (arrayKey);
				ELEKTRA_LOG_DEBUG ("found array value %s\n", itemValue);
				GValue keyValue = G_VALUE_INIT;
				g_value_init (&keyValue, G_TYPE_STRING);
				g_value_set_string (&keyValue, itemValue);
				g_ptr_array_add (xfconfArray, &keyValue); // todo: determine item types, they throw critical errors
			}
			if (!xfconf_channel_set_arrayv (channel, xfconfKeyName, xfconfArray))
			{
				ELEKTRA_LOG_DEBUG ("unable to set array value\n");
			}
		}
		else
		{
			ELEKTRA_LOG_DEBUG ("key %s is a single value\n", xfconfKeyName);
			GValue keyValue = G_VALUE_INIT;
			if (!xfconf_channel_get_property (channel, xfconfKeyName, &keyValue))
			{
				ELEKTRA_LOG_DEBUG ("key was not found, initialize a new one of type string\n");
				g_value_init (&keyValue, G_TYPE_STRING);
			}
			ELEKTRA_LOG_DEBUG ("key is of type: %lu\n", keyValue.g_type);
			g_value_set_string (&keyValue, keyString (cur));
			if (!xfconf_channel_set_property (channel, xfconfKeyName, &keyValue))
			{
				ELEKTRA_LOG_DEBUG ("unable to set value\n");
			}
		}
	}
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("xfconf",
		ELEKTRA_PLUGIN_OPEN,	&elektraXfconfOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraXfconfClose,
		ELEKTRA_PLUGIN_GET,	&elektraXfconfGet,
		ELEKTRA_PLUGIN_SET,	&elektraXfconfSet,
		ELEKTRA_PLUGIN_END);
}
