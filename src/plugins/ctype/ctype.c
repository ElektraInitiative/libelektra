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

#include <kdberrors.h>

typedef struct _Type
{
	const char * name;
	bool (*check) (const Key * key);
} Type;

static const Type elektraTypesList[] = { { "any", &elektraCTypeCheckAny },
					 { "empty", &elektraCTypeCheckEmpty },
					 { "string", &elektraCTypeCheckString },
					 { "char", &elektraCTypeCheckChar },
					 { "octet", &elektraCTypeCheckChar },
					 { "short", &elektraCTypeCheckShort },
					 { "long", &elektraCTypeCheckLong },
					 { "long_long", &elektraCTypeCheckLongLong },
					 { "unsigned_short", &elektraCTypeCheckUnsignedShort },
					 { "unsigned_long", &elektraCTypeCheckUnsignedLong },
					 { "unsigned_long_long", &elektraCTypeCheckUnsignedLongLong },
					 { "float", &elektraCTypeCheckFloat },
					 { "double", &elektraCTypeCheckDouble },
#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE
					 { "long_double", &elektraCTypeCheckLongDouble },
#endif
					 { "boolean", &elektraCTypeCheckBoolean },
					 { "enum", &elektraCTypeCheckEnum },
					 { NULL, NULL } };

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

static void setError (Key * key, Key * errorKey)
{
	ELEKTRA_SET_ERRORF (52, errorKey, "The type %s failed to match for %s with string: %s", getTypeName (key), keyName (key),
			    keyString (key));
}

bool elektraCTypeValidateKey (Key * key, Key * errorKey)
{
	if (!elektraCTypeCheckType (key))
	{
		setError (key, errorKey);
		return false;
	}

	return true;
}

int elektraCTypeGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	KeySet * config = ksNew (30, keyNew ("system/elektra/modules/ctype", KEY_VALUE, "ctype plugin waits for your orders", KEY_END),
				 keyNew ("system/elektra/modules/ctype/exports", KEY_END),
				 keyNew ("system/elektra/modules/ctype/exports/get", KEY_FUNC, elektraCTypeGet, KEY_END),
				 keyNew ("system/elektra/modules/ctype/exports/set", KEY_FUNC, elektraCTypeSet, KEY_END),
				 keyNew ("system/elektra/modules/ctype/exports/validateKey", KEY_FUNC, elektraCTypeValidateKey, KEY_END),
#include "readme_ctype.c"
				 keyNew ("system/elektra/modules/ctype/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
	ksAppend (returned, config);
	ksDel (config);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraCTypeSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	cursor_t cursor = ksGetCursor (returned);

	ksRewind (returned);

	Key * cur = NULL;
	while ((cur = ksNext (returned)))
	{
		if (!elektraCTypeCheckType (cur))
		{
			setError (cur, parentKey);
			ksSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	ksSetCursor (returned, cursor);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (type)
{
	// clang-format off
	return elektraPluginExport("ctype",
		ELEKTRA_PLUGIN_GET,	&elektraCTypeGet,
		ELEKTRA_PLUGIN_SET,	&elektraCTypeSet,
		ELEKTRA_PLUGIN_END);
	// clang-format on
}
