/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include "type.h"
#include "types.h"

#include <kdbease.h>
#include <kdberrors.h>

struct _Type
{
	const char * name;
	bool (*normalize) (Plugin * handle, ElektraKey * key);
	bool (*check) (const ElektraKey * key);
	bool (*restore) (Plugin * handle, ElektraKey * key);
	void (*setError) (Plugin * handle, ElektraKey * errorKey, const ElektraKey * key);
};

static void elektraTypeSetDefaultError (Plugin * handle, ElektraKey * errorKey, const ElektraKey * key);

static const Type elektraTypesList[] = {
	{ "any", NULL, &elektraTypeCheckAny, NULL, &elektraTypeSetDefaultError },
	{ "string", NULL, &elektraTypeCheckString, NULL, &elektraTypeSetDefaultError },
	{ "wstring", NULL, &elektraTypeCheckWString, NULL, &elektraTypeSetDefaultError },
	{ "char", NULL, &elektraTypeCheckChar, NULL, &elektraTypeSetDefaultError },
	{ "wchar", NULL, &elektraTypeCheckWChar, NULL, &elektraTypeSetDefaultError },
	{ "octet", NULL, &elektraTypeCheckChar, NULL, &elektraTypeSetDefaultError },
	{ "short", NULL, &elektraTypeCheckShort, NULL, &elektraTypeSetDefaultError },
	{ "long", NULL, &elektraTypeCheckLong, NULL, &elektraTypeSetDefaultError },
	{ "long_long", NULL, &elektraTypeCheckLongLong, NULL, &elektraTypeSetDefaultError },
	{ "unsigned_short", NULL, &elektraTypeCheckUnsignedShort, NULL, &elektraTypeSetDefaultError },
	{ "unsigned_long", NULL, &elektraTypeCheckUnsignedLong, NULL, &elektraTypeSetDefaultError },
	{ "unsigned_long_long", NULL, &elektraTypeCheckUnsignedLongLong, NULL, &elektraTypeSetDefaultError },
	{ "float", NULL, &elektraTypeCheckFloat, NULL, &elektraTypeSetDefaultError },
	{ "double", NULL, &elektraTypeCheckDouble, NULL, &elektraTypeSetDefaultError },
#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE
	{ "long_double", NULL, &elektraTypeCheckLongDouble, NULL, &elektraTypeSetDefaultError },
#endif
	{ "boolean", &elektraTypeNormalizeBoolean, &elektraTypeCheckBoolean, &elektraTypeRestoreBoolean, &elektraTypeSetDefaultError },
	{ "enum", &elektraTypeNormalizeEnum, &elektraTypeCheckEnum, &elektraTypeRestoreEnum, &elektraTypeSetErrorEnum },
	{ NULL, NULL, NULL, NULL, NULL }
};

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

static const char * getTypeName (const ElektraKey * key)
{
	const ElektraKey * meta = elektraKeyGetMeta (key, "check/type");
	if (meta == NULL)
	{
		meta = elektraKeyGetMeta (key, "type");
	}

	if (meta == NULL)
	{
		return NULL;
	}

	const char * type = elektraKeyString (meta);
	return strlen (type) == 0 ? NULL : type;
}

bool elektraTypeCheckType (const ElektraKey * key)
{
	const char * typeName = getTypeName (key);
	if (typeName == NULL)
	{
		return true;
	}

	const Type * type = findType (typeName);
	return type != NULL && type->check (key);
}

static void elektraTypeSetDefaultError (Plugin * handle ELEKTRA_UNUSED, ElektraKey * errorKey, const ElektraKey * key)
{
	ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "The type '%s' failed to match for '%s' with string '%s'", getTypeName (key),
						elektraKeyName (key), elektraKeyString (key));
}

bool elektraTypeValidateKey (Plugin * handle, ElektraKey * key, ElektraKey * errorKey)
{
	const char * typeName = getTypeName (key);
	if (typeName == NULL)
	{
		return true;
	}

	const Type * type = findType (typeName);
	if (type == NULL)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "Unknown type '%s' for key '%s'", typeName, elektraKeyName (key));
		return false;
	}

	if (type->normalize != NULL && !type->normalize (handle, key))
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (errorKey, "The value '%s' of key '%s' could not be converted into a %s",
							 elektraKeyString (key), elektraKeyName (key), typeName);
		return false;
	}

	if (!type->check (key))
	{
		type->setError (handle, errorKey, key);
		return false;
	}

	if (type->restore != NULL && !type->restore (handle, key))
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey,
							"The normalized value '%s' of key '%s' could not be restored (type is '%s')",
							elektraKeyString (key), elektraKeyName (key), typeName);
		return false;
	}

	return true;
}

static kdb_long_long_t readBooleans (ElektraKeyset * config, struct boolean_pair ** result, ElektraKey * errorKey)
{
	ElektraKey * parent = elektraKeysetLookupByName (config, "/booleans", 0);
	const char * max = elektraKeyString (parent);
	if (parent == NULL || strlen (max) == 0)
	{
		*result = NULL;
		return -1;
	}

	kdb_long_long_t index = 0;
	char buffer[10 + ELEKTRA_MAX_ARRAY_SIZE + 6];
	strcpy (buffer, "/booleans/");
	char * indexPos = &buffer[10];
	elektraWriteArrayNumber (indexPos, index);

	if (strcmp (indexPos, max) > 0)
	{
		*result = NULL;
		return 0;
	}

	kdb_long_long_t size = 0;
	*result = elektraMalloc (sizeof (struct boolean_pair));
	while (strcmp (indexPos, max) <= 0)
	{
		char * subPos = &buffer[strlen (buffer)];
		strcpy (subPos, "/true");
		ElektraKey * trueKey = elektraKeysetLookupByName (config, buffer, 0);
		strcpy (subPos, "/false");
		ElektraKey * falseKey = elektraKeysetLookupByName (config, buffer, 0);

		*subPos = '\0';
		if ((trueKey == NULL) != (falseKey == NULL))
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				errorKey, "You must set both true and false for a boolean pair (config key: '%s')", buffer);
			elektraFree (*result);
			*result = NULL;
			return -2;
		}


		elektraRealloc ((void **) result, (size + 1) * sizeof (struct boolean_pair));

		(*result)[size].trueValue = elektraKeyString (trueKey);
		(*result)[size].falseValue = elektraKeyString (falseKey);
		++size;

		++index;
		elektraWriteArrayNumber (indexPos, index);
	}

	return size;
}

/**
 * Reads the value of the config key /boolean/restoreas.
 *
 * @p config The config KeySet obtained from elektraPluginGetConfig().
 *
 * @retval -3 on error
 * @retval -2 if /boolean/restoreas = none
 * @retval -1 if /boolean/restoreas is unset
 * @retval >= 0 index of chosen boolean pair
 */
static kdb_long_long_t readBooleanRestore (ElektraKeyset * config)
{
	ElektraKey * restore = elektraKeysetLookupByName (config, "/boolean/restoreas", 0);
	if (restore == NULL)
	{
		return -1;
	}

	const char * restoreString = elektraKeyString (restore);

	if (strcmp (restoreString, "none") == 0)
	{
		return -2;
	}

	int digitStart = elektraArrayValidateBaseNameString (restoreString);
	if (digitStart <= 0)
	{
		return -3;
	}

	ElektraKey * restoreKey = elektraKeyNew ("/", ELEKTRA_KEY_VALUE, &restoreString[digitStart], ELEKTRA_KEY_END);

	kdb_long_long_t size;
	if (!elektraKeyToLongLong (restoreKey, &size))
	{
		elektraKeyDel (restoreKey);
		return -3;
	}

	elektraKeyDel (restoreKey);

	return size;
}

int elektraTypeOpen (Plugin * handle, ElektraKey * errorKey)
{
	ElektraKeyset * conf = elektraPluginGetConfig (handle);
	TypeData * data = elektraMalloc (sizeof (TypeData));

	kdb_long_long_t result = readBooleans (conf, &data->booleans, errorKey);
	if (result < -1)
	{
		elektraFree (data);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (result == -1)
	{
		data->booleans = elektraMalloc (sizeof (struct boolean_pair) * 5);
		data->booleans[0] = (struct boolean_pair){ "yes", "no" };
		data->booleans[1] = (struct boolean_pair){ "true", "false" };
		data->booleans[2] = (struct boolean_pair){ "on", "off" };
		data->booleans[3] = (struct boolean_pair){ "enabled", "disabled" };
		data->booleans[4] = (struct boolean_pair){ "enable", "disable" };
		data->booleanCount = 5;
	}
	else
	{
		data->booleanCount = result;
	}

	data->booleanRestore = readBooleanRestore (conf);
	if (data->booleanRestore < -2 || data->booleanRestore >= data->booleanCount)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (errorKey, "The value of the config key /boolean/restoreas was invalid");
		elektraFree (data);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	elektraPluginSetData (handle, data);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraTypeGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/type"))
	{
		ElektraKeyset * contract =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/type", ELEKTRA_KEY_VALUE, "type plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/type/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/type/exports/open", ELEKTRA_KEY_FUNC, elektraTypeOpen, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/type/exports/get", ELEKTRA_KEY_FUNC, elektraTypeGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/type/exports/set", ELEKTRA_KEY_FUNC, elektraTypeSet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/type/exports/close", ELEKTRA_KEY_FUNC, elektraTypeClose, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/type/exports/checkconf", ELEKTRA_KEY_FUNC, elektraTypeCheckConf, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/type/exports/validateKey", ELEKTRA_KEY_FUNC, elektraTypeValidateKey, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       elektraKeyNew ("system:/elektra/modules/type/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	elektraCursor cursor = elektraKeysetGetCursor (returned);

	elektraKeysetRewind (returned);

	ElektraKey * cur = NULL;
	while ((cur = elektraKeysetNext (returned)))
	{
		const char * typeName = getTypeName (cur);
		if (typeName == NULL)
		{
			continue;
		}

		const Type * type = findType (typeName);
		if (type == NULL)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Unknown type '%s' for key '%s'", typeName, elektraKeyName (cur));
			elektraKeysetSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (type->normalize != NULL)
		{
			const ElektraKey * orig = elektraKeyGetMeta (cur, "origvalue");
			if (orig != NULL)
			{
				ELEKTRA_SET_INSTALLATION_ERRORF (
					parentKey,
					"The key '%s' was already normalized by a different plugin. Please ensure that there is "
					"only one plugin active that will normalize this key",
					elektraKeyName (cur));
				elektraKeysetSetCursor (returned, cursor);
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}

			if (!type->normalize (handle, cur))
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey,
									"The value '%s' of key '%s' could not be converted into a %s",
									elektraKeyString (cur), elektraKeyName (cur), typeName);
				elektraKeysetSetCursor (returned, cursor);
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}
		}

		if (!type->check (cur))
		{
			type->setError (handle, parentKey, cur);
			elektraKeysetSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	elektraKeysetSetCursor (returned, cursor);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraTypeSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	elektraCursor cursor = elektraKeysetGetCursor (returned);

	elektraKeysetRewind (returned);

	ElektraKey * cur = NULL;
	while ((cur = elektraKeysetNext (returned)))
	{
		const char * typeName = getTypeName (cur);
		if (typeName == NULL)
		{
			continue;
		}

		const Type * type = findType (typeName);
		if (type == NULL)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Unknown type '%s' for key '%s'", typeName, elektraKeyName (cur));
			elektraKeysetSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (type->normalize != NULL)
		{
			const ElektraKey * orig = elektraKeyGetMeta (cur, "origvalue");
			// skip normalization origvalue already set
			if (orig == NULL && !type->normalize (handle, cur))
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey,
									"The value '%s' of key '%s' could not be converted into a %s",
									elektraKeyString (cur), elektraKeyName (cur), typeName);
				elektraKeysetSetCursor (returned, cursor);
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}
		}

		if (!type->check (cur))
		{
			type->setError (handle, parentKey, cur);
			elektraKeysetSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (type->restore != NULL && !type->restore (handle, cur))
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				parentKey, "The normalized value '%s' of key '%s' could not be restored (type is '%s')", elektraKeyString (cur),
				elektraKeyName (cur), typeName);
			elektraKeysetSetCursor (returned, cursor);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	elektraKeysetSetCursor (returned, cursor);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}


int elektraTypeClose (Plugin * handle, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	TypeData * data = elektraPluginGetData (handle);
	if (data != NULL)
	{
		if (data->booleans != NULL)
		{
			elektraFree (data->booleans);
		}
		elektraFree (data);
	}
	elektraPluginSetData (handle, NULL);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraTypeCheckConf (ElektraKey * errorKey, ElektraKeyset * conf)
{
	struct boolean_pair * pairs;
	if (readBooleans (conf, &pairs, errorKey) < -1)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	elektraFree (pairs);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("type",
		ELEKTRA_PLUGIN_OPEN,	&elektraTypeOpen,
		ELEKTRA_PLUGIN_GET,	&elektraTypeGet,
		ELEKTRA_PLUGIN_SET,	&elektraTypeSet,
		ELEKTRA_PLUGIN_CLOSE,	&elektraTypeClose,
		ELEKTRA_PLUGIN_END);
	// clang-format on
}
