/**
 * @file
 *
 * @brief Implementation of entry points
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include "newtype.h"
#include "types.h"

#include <kdbease.h>
#include <kdberrors.h>

struct _Type
{
	const char * name;
	bool (*normalize) (Plugin * handle, Key * key);
	bool (*check) (const Key * key);
	bool (*restore) (Plugin * handle, Key * key);
	void (*setError) (Plugin * handle, Key * errorKey, const Key * key);
};

static void elektraNewTypeSetDefaultError (Plugin * handle, Key * errorKey, const Key * key);

static const Type elektraNewTypesList[] = {
	{ "any", NULL, &elektraNewTypeCheckAny, NULL, &elektraNewTypeSetDefaultError },
	{ "empty", NULL, &elektraNewTypeCheckEmpty, NULL, &elektraNewTypeSetDefaultError },
	{ "string", NULL, &elektraNewTypeCheckString, NULL, &elektraNewTypeSetDefaultError },
	{ "wstring", NULL, &elektraNewTypeCheckWString, NULL, &elektraNewTypeSetDefaultError },
	{ "char", NULL, &elektraNewTypeCheckChar, NULL, &elektraNewTypeSetDefaultError },
	{ "wchar", NULL, &elektraNewTypeCheckWChar, NULL, &elektraNewTypeSetDefaultError },
	{ "octet", NULL, &elektraNewTypeCheckChar, NULL, &elektraNewTypeSetDefaultError },
	{ "short", NULL, &elektraNewTypeCheckShort, NULL, &elektraNewTypeSetDefaultError },
	{ "long", NULL, &elektraNewTypeCheckLong, NULL, &elektraNewTypeSetDefaultError },
	{ "long_long", NULL, &elektraNewTypeCheckLongLong, NULL, &elektraNewTypeSetDefaultError },
	{ "unsigned_short", NULL, &elektraNewTypeCheckUnsignedShort, NULL, &elektraNewTypeSetDefaultError },
	{ "unsigned_long", NULL, &elektraNewTypeCheckUnsignedLong, NULL, &elektraNewTypeSetDefaultError },
	{ "unsigned_long_long", NULL, &elektraNewTypeCheckUnsignedLongLong, NULL, &elektraNewTypeSetDefaultError },
	{ "float", NULL, &elektraNewTypeCheckFloat, NULL, &elektraNewTypeSetDefaultError },
	{ "double", NULL, &elektraNewTypeCheckDouble, NULL, &elektraNewTypeSetDefaultError },
#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE
	{ "long_double", NULL, &elektraNewTypeCheckLongDouble, NULL, &elektraNewTypeSetDefaultError },
#endif
	{ "boolean", &elektraNewTypeNormalizeBoolean, &elektraNewTypeCheckBoolean, &elektraNewTypeRestoreBoolean,
	  &elektraNewTypeSetDefaultError },
	{ "enum", NULL, &elektraNewTypeCheckEnum, NULL, &elektraNewTypeSetErrorEnum },
	{ NULL, NULL, NULL, NULL, NULL }
};

static const Type * findType (const char * name)
{
	const Type * cur = &elektraNewTypesList[0];
	while (cur->name != NULL)
	{
		if (strcmp (cur->name, name) == 0)
		{
			return cur;
		}
		++cur;
	}
	return NULL;
}

static const char * getTypeName (const Key * key)
{
	const Key * meta = keyGetMeta (key, "check/type");
	if (meta == NULL)
	{
		meta = keyGetMeta (key, "type");
	}

	if (meta == NULL)
	{
		return NULL;
	}

	const char * type = keyString (meta);
	return strlen (type) == 0 ? NULL : type;
}

bool elektraNewTypeCheckType (const Key * key)
{
	const char * typeName = getTypeName (key);
	if (typeName == NULL)
	{
		return true;
	}

	const Type * type = findType (typeName);
	return type != NULL && type->check (key);
}

static void elektraNewTypeSetDefaultError (Plugin * handle ELEKTRA_UNUSED, Key * errorKey, const Key * key)
{
	ELEKTRA_SET_ERRORF (52, errorKey, "The type '%s' failed to match for '%s' with string: %s", getTypeName (key), keyName (key),
			    keyString (key));
}

bool elektraNewTypeValidateKey (Plugin * handle, Key * key, Key * errorKey)
{
	const char * typeName = getTypeName (key);
	if (typeName == NULL)
	{
		return true;
	}

	const Type * type = findType (typeName);
	if (type == NULL)
	{
		ELEKTRA_SET_ERRORF (52, errorKey, "Unknown type '%s' for key '%s'", typeName, keyName (key));
		return false;
	}

	if (type->normalize != NULL && !type->normalize (handle, key))
	{
		ELEKTRA_SET_ERRORF (52, errorKey, "The value '%s' of key %s could not be normalized (type is '%s')", keyString (key),
				    keyName (key), typeName);
		return false;
	}

	if (!type->check (key))
	{
		type->setError (handle, errorKey, key);
		return false;
	}

	if (type->restore != NULL && !type->restore (handle, key))
	{
		ELEKTRA_SET_ERRORF (52, errorKey, "The normalized value '%s' of key %s could not be restored (type is '%s')",
				    keyString (key), keyName (key), typeName);
		return false;
	}

	return true;
}

int elektraNewTypeGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/newtype"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/newtype", KEY_VALUE, "newtype plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/newtype/exports", KEY_END),
			       keyNew ("system/elektra/modules/newtype/exports/get", KEY_FUNC, elektraNewTypeGet, KEY_END),
			       keyNew ("system/elektra/modules/newtype/exports/set", KEY_FUNC, elektraNewTypeSet, KEY_END),
			       keyNew ("system/elektra/modules/newtype/exports/checkconf", KEY_FUNC, elektraNewTypeCheckConf, KEY_END),
			       keyNew ("system/elektra/modules/newtype/exports/validateKey", KEY_FUNC, elektraNewTypeValidateKey, KEY_END),
#include ELEKTRA_README
			       keyNew ("system/elektra/modules/newtype/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	cursor_t cursor = ksGetCursor (returned);

	ksRewind (returned);

	Key * cur = NULL;
	while ((cur = ksNext (returned)))
	{
		const char * typeName = getTypeName (cur);
		if (typeName == NULL)
		{
			return true;
		}

		const Type * type = findType (typeName);
		if (type == NULL)
		{
			ELEKTRA_SET_ERRORF (52, parentKey, "Unknown type '%s' for key '%s'", typeName, keyName (cur));
			ksSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (type->normalize != NULL && !type->normalize (handle, cur))
		{
			ELEKTRA_SET_ERRORF (52, parentKey, "The value '%s' of key %s could not be normalized (type is '%s')",
					    keyString (cur), keyName (cur), typeName);
			ksSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (!type->check (cur))
		{
			type->setError (handle, parentKey, cur);
			ksSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	ksSetCursor (returned, cursor);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraNewTypeSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	cursor_t cursor = ksGetCursor (returned);

	ksRewind (returned);

	Key * cur = NULL;
	while ((cur = ksNext (returned)))
	{
		const char * typeName = getTypeName (cur);
		if (typeName == NULL)
		{
			return true;
		}

		const Type * type = findType (typeName);
		if (type == NULL)
		{
			ELEKTRA_SET_ERRORF (52, parentKey, "Unknown type '%s' for key '%s'", typeName, keyName (cur));
			ksSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (type->normalize != NULL && !type->normalize (handle, cur))
		{
			ELEKTRA_SET_ERRORF (52, parentKey, "The value '%s' of key %s could not be normalized (type is '%s')",
					    keyString (cur), keyName (cur), typeName);
			ksSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (!type->check (cur))
		{
			type->setError (handle, parentKey, cur);
			ksSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (type->restore != NULL && !type->restore (handle, cur))
		{
			ELEKTRA_SET_ERRORF (52, parentKey, "The normalized value '%s' of key %s could not be restored (type is '%s')",
					    keyString (cur), keyName (cur), typeName);
			ksSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	ksSetCursor (returned, cursor);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraNewTypeCheckConf (Key * errorKey ELEKTRA_UNUSED, KeySet * conf)
{
	Key * parent = ksLookupByName (conf, "booleans", 0);
	const char * max = keyString (parent);
	if (parent == NULL || strlen (max) == 0)
	{
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	kdb_long_long_t index = 0;
	char buffer[9 + ELEKTRA_MAX_ARRAY_SIZE + 6];
	strcpy (buffer, "booleans/");
	char * indexPos = &buffer[9];

	while (strcmp (buffer, max) <= 0)
	{
		elektraWriteArrayNumber (indexPos, index);

		char * subPos = &buffer[strlen (buffer)];
		strcpy (subPos, "/true");
		Key * trueKey = ksLookupByName (conf, buffer, 0);
		strcpy (subPos, "/false");
		Key * falseKey = ksLookupByName (conf, buffer, 0);

		if ((trueKey == NULL) != (falseKey == NULL))
		{
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("newtype",
		ELEKTRA_PLUGIN_GET,	&elektraNewTypeGet,
		ELEKTRA_PLUGIN_SET,	&elektraNewTypeSet,
		ELEKTRA_PLUGIN_END);
	// clang-format on
}
