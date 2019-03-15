/**
 * @file
 *
 * @brief Implementation of entry points
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include "ctype.h"
#include "ctypes.h"

#include <kdbease.h>
#include <kdberrors.h>

struct _Type
{
	const char * name;
	bool (*normalize) (Plugin * handle, Key * key);
	bool (*check) (const Key * key);
	bool (*restore) (Plugin * handle, Key * key);
};

static const Type elektraTypesList[] = { { "any", NULL, &elektraCTypeCheckAny, NULL },
					 { "empty", NULL, &elektraCTypeCheckEmpty, NULL },
					 { "string", NULL, &elektraCTypeCheckString, NULL },
					 { "wstring", NULL, &elektraCTypeCheckWString, NULL },
					 { "char", NULL, &elektraCTypeCheckChar, NULL },
					 { "wchar", NULL, &elektraCTypeCheckWChar, NULL },
					 { "octet", NULL, &elektraCTypeCheckChar, NULL },
					 { "short", NULL, &elektraCTypeCheckShort, NULL },
					 { "long", NULL, &elektraCTypeCheckLong, NULL },
					 { "long_long", NULL, &elektraCTypeCheckLongLong, NULL },
					 { "unsigned_short", NULL, &elektraCTypeCheckUnsignedShort, NULL },
					 { "unsigned_long", NULL, &elektraCTypeCheckUnsignedLong, NULL },
					 { "unsigned_long_long", NULL, &elektraCTypeCheckUnsignedLongLong, NULL },
					 { "float", NULL, &elektraCTypeCheckFloat, NULL },
					 { "double", NULL, &elektraCTypeCheckDouble, NULL },
#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE
					 { "long_double", NULL, &elektraCTypeCheckLongDouble, NULL },
#endif
					 { "boolean", &elektraCTypeNormalizeBoolean, &elektraCTypeCheckBoolean,
					   &elektraCTypeRestoreBoolean },
					 { "enum", NULL, &elektraCTypeCheckEnum, NULL },
					 { NULL, NULL, NULL, NULL } };

static const Type * findType (const char * name)
{
	const Type * cur = &elektraTypesList[0];
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

bool elektraCTypeCheckType (const Key * key)
{
	const char * typeName = getTypeName (key);
	if (typeName == NULL)
	{
		return true;
	}

	const Type * type = findType (typeName);
	return type != NULL && type->check (key);
}

bool elektraCTypeValidateKey (Plugin * handle, Key * key, Key * errorKey)
{
	const char * typeName = getTypeName (key);
	if (typeName == NULL)
	{
		return true;
	}

	const Type * type = findType (typeName);
	if (type == NULL)
	{
		ELEKTRA_SET_ERRORF (52, errorKey, "The type %s failed to match for %s with string: %s", getTypeName (key), keyName (key),
				    keyString (key));
		return false;
	}

	if (type->normalize != NULL && !type->normalize (handle, key))
	{
		ELEKTRA_SET_ERRORF (52, errorKey, "The type %s failed to match for %s with string: %s", getTypeName (key), keyName (key),
				    keyString (key));
		return false;
	}

	if (!type->check (key))
	{
		ELEKTRA_SET_ERRORF (52, errorKey, "The type %s failed to match for %s with string: %s", getTypeName (key), keyName (key),
				    keyString (key));
		return false;
	}

	if (type->restore != NULL && !type->restore (handle, key))
	{
		ELEKTRA_SET_ERRORF (52, errorKey, "The type %s failed to match for %s with string: %s", getTypeName (key), keyName (key),
				    keyString (key));
		return false;
	}

	return true;
}

int elektraCTypeGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/ctype"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/ctype", KEY_VALUE, "ctype plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/ctype/exports", KEY_END),
			       keyNew ("system/elektra/modules/ctype/exports/get", KEY_FUNC, elektraCTypeGet, KEY_END),
			       keyNew ("system/elektra/modules/ctype/exports/set", KEY_FUNC, elektraCTypeSet, KEY_END),
			       keyNew ("system/elektra/modules/ctype/exports/checkconf", KEY_FUNC, elektraCTypeCheckConf, KEY_END),
			       keyNew ("system/elektra/modules/ctype/exports/validateKey", KEY_FUNC, elektraCTypeValidateKey, KEY_END),
#include ELEKTRA_README
			       keyNew ("system/elektra/modules/ctype/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
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
			ELEKTRA_SET_ERRORF (52, parentKey, "The type %s failed to match for %s with string: %s", getTypeName (cur),
					    keyName (cur), keyString (cur));
			ksSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (type->normalize != NULL && !type->normalize (handle, cur))
		{
			ELEKTRA_SET_ERRORF (52, parentKey, "The type %s failed to match for %s with string: %s", getTypeName (cur),
					    keyName (cur), keyString (cur));
			ksSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (!type->check (cur))
		{
			ELEKTRA_SET_ERRORF (52, parentKey, "The type %s failed to match for %s with string: %s", getTypeName (cur),
					    keyName (cur), keyString (cur));
			ksSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	ksSetCursor (returned, cursor);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraCTypeSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
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
			ELEKTRA_SET_ERRORF (52, parentKey, "The type %s failed to match for %s with string: %s", getTypeName (cur),
					    keyName (cur), keyString (cur));
			ksSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (type->normalize != NULL && !type->normalize (handle, cur))
		{
			ELEKTRA_SET_ERRORF (52, parentKey, "The type %s failed to match for %s with string: %s", getTypeName (cur),
					    keyName (cur), keyString (cur));
			ksSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (!type->check (cur))
		{
			ELEKTRA_SET_ERRORF (52, parentKey, "The type %s failed to match for %s with string: %s", getTypeName (cur),
					    keyName (cur), keyString (cur));
			ksSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (type->restore != NULL && !type->restore (handle, cur))
		{
			ELEKTRA_SET_ERRORF (52, parentKey, "The type %s failed to match for %s with string: %s", getTypeName (cur),
					    keyName (cur), keyString (cur));
			ksSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	ksSetCursor (returned, cursor);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraCTypeCheckConf (Key * errorKey ELEKTRA_UNUSED, KeySet * conf)
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
	return elektraPluginExport("ctype",
		ELEKTRA_PLUGIN_GET,	&elektraCTypeGet,
		ELEKTRA_PLUGIN_SET,	&elektraCTypeSet,
		ELEKTRA_PLUGIN_END);
	// clang-format on
}
