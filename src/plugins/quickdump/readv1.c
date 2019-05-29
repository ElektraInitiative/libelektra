/**
 * @file
 *
 * @brief Source for quickdump plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

static int readVersion1 (FILE * file, KeySet * returned, Key * parentKey)
{
	char c;
	while ((c = fgetc (file)) != EOF)
	{
		ungetc (c, file);

		char * name = readString (file, parentKey);
		if (name == NULL)
		{
			elektraFree (name);
			fclose (file);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		char type = fgetc (file);
		if (type == EOF)
		{
			elektraFree (name);
			fclose (file);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		Key * k;

		switch (type)
		{
		case 'b':
		{
			// binary key value
			kdb_unsigned_long_long_t valueSize;
			if (!readUInt64 (file, &valueSize, parentKey))
			{
				elektraFree (name);
				fclose (file);
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}

			if (valueSize == 0)
			{
				k = keyNew (name, KEY_BINARY, KEY_SIZE, valueSize, KEY_END);
				elektraFree (name);
			}
			else
			{
				void * value = elektraMalloc (valueSize);
				if (fread (value, sizeof (char), valueSize, file) < valueSize)
				{
					elektraFree (name);
					fclose (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}
				k = keyNew (name, KEY_BINARY, KEY_SIZE, (size_t) valueSize, KEY_VALUE, value, KEY_END);
				elektraFree (name);
				elektraFree (value);
			}
			break;
		}
		case 's':
		{
			// string key value
			char * value = readString (file, parentKey);
			if (value == NULL)
			{
				elektraFree (name);
				fclose (file);
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}
			k = keyNew (name, KEY_VALUE, value, KEY_END);
			elektraFree (name);
			elektraFree (value);
			break;
		}
		default:
			elektraFree (name);
			fclose (file);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		while ((c = fgetc (file)) != 0)
		{
			if (c == EOF)
			{
				keyDel (k);
				fclose (file);
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}

			switch (c)
			{
			case 'm':
			{
				// meta key
				char * metaName = readString (file, parentKey);
				if (metaName == NULL)
				{
					keyDel (k);
					fclose (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				char * metaValue = readString (file, parentKey);
				if (metaValue == NULL)
				{
					keyDel (k);
					elektraFree (metaName);
					fclose (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				keySetMeta (k, metaName, metaValue);
				elektraFree (metaName);
				elektraFree (metaValue);
				break;
			}
			case 'c':
			{
				// copy meta
				char * keyName = readString (file, parentKey);
				if (keyName == NULL)
				{
					keyDel (k);
					fclose (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				char * metaName = readString (file, parentKey);
				if (metaName == NULL)
				{
					keyDel (k);
					elektraFree (keyName);
					fclose (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				const Key * sourceKey = ksLookupByName (returned, keyName, 0);
				if (sourceKey == NULL)
				{
					ELEKTRA_SET_RESOURCE_ERRORF (
						parentKey, "Could not copy meta data from key '%s': Key not found", keyName);
					keyDel (k);
					elektraFree (keyName);
					elektraFree (metaName);
					fclose (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				if (keyCopyMeta (k, sourceKey, metaName) != 1)
				{
					ELEKTRA_SET_ASSERTION_ERRORF (parentKey,
								      "Could not copy meta data from key '%s': Error during copy", keyName);
					keyDel (k);
					elektraFree (keyName);
					elektraFree (metaName);
					fclose (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				elektraFree (keyName);
				elektraFree (metaName);
				break;
			}
			default:
				keyDel (k);
				fclose (file);
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}
		}

		ksAppendKey (returned, k);
	}

	fclose (file);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}
