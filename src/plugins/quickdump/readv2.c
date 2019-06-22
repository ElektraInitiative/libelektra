/**
 * @file
 *
 * @brief Source for quickdump plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

static int readVersion2 (FILE * file, KeySet * returned, Key * parentKey)
{
	// setup buffers
	struct stringbuffer valueBuffer;
	setupBuffer (&valueBuffer, 4);

	struct stringbuffer metaNameBuffer;
	setupBuffer (&metaNameBuffer, 4);

	// setup name buffer with parent key
	struct stringbuffer nameBuffer;

	size_t parentSize = keyGetNameSize (parentKey); // includes null terminator
	setupBuffer (&nameBuffer, parentSize + 4);

	keyGetName (parentKey, nameBuffer.string, parentSize);
	nameBuffer.string[parentSize - 1] = '/'; // replaces null terminator
	nameBuffer.string[parentSize] = '\0';    // set new null terminator
	nameBuffer.offset = parentSize;		 // set offset to null terminator

	char c;
	while ((c = fgetc (file)) != EOF)
	{
		ungetc (c, file);

		if (!readStringIntoBuffer (file, &nameBuffer, parentKey))
		{
			elektraFree (nameBuffer.string);
			elektraFree (metaNameBuffer.string);
			elektraFree (valueBuffer.string);
			fclose (file);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		char type = fgetc (file);
		if (type == EOF)
		{
			elektraFree (nameBuffer.string);
			elektraFree (metaNameBuffer.string);
			elektraFree (valueBuffer.string);
			fclose (file);
			ELEKTRA_SET_RESOURCE_ERROR (parentKey, "Missing key type");
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
				elektraFree (nameBuffer.string);
				elektraFree (metaNameBuffer.string);
				elektraFree (valueBuffer.string);
				fclose (file);
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}

			if (valueSize == 0)
			{
				k = keyNew (nameBuffer.string, KEY_BINARY, KEY_SIZE, valueSize, KEY_END);
			}
			else
			{
				void * value = elektraMalloc (valueSize);
				if (fread (value, sizeof (char), valueSize, file) < valueSize)
				{
					elektraFree (nameBuffer.string);
					elektraFree (metaNameBuffer.string);
					fclose (file);
					ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Error while reading file: Reason: %s", strerror (errno));
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}
				k = keyNew (nameBuffer.string, KEY_BINARY, KEY_SIZE, (size_t) valueSize, KEY_VALUE, value, KEY_END);
				elektraFree (value);
			}
			break;
		}
		case 's':
		{
			// string key value
			if (!readStringIntoBuffer (file, &valueBuffer, parentKey))
			{
				elektraFree (nameBuffer.string);
				elektraFree (metaNameBuffer.string);
				elektraFree (valueBuffer.string);
				fclose (file);
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}
			k = keyNew (nameBuffer.string, KEY_VALUE, valueBuffer.string, KEY_END);
			break;
		}
		default:
			elektraFree (nameBuffer.string);
			elektraFree (metaNameBuffer.string);
			elektraFree (valueBuffer.string);
			fclose (file);
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Unknown key type %c", type);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		while ((c = fgetc (file)) != 0)
		{
			if (c == EOF)
			{
				keyDel (k);
				elektraFree (nameBuffer.string);
				elektraFree (metaNameBuffer.string);
				elektraFree (valueBuffer.string);
				fclose (file);
				ELEKTRA_SET_RESOURCE_ERROR (parentKey, "Missing key end");
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}

			switch (c)
			{
			case 'm':
			{
				// meta key
				if (!readStringIntoBuffer (file, &metaNameBuffer, parentKey))
				{
					keyDel (k);
					elektraFree (nameBuffer.string);
					elektraFree (metaNameBuffer.string);
					elektraFree (valueBuffer.string);
					fclose (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				if (!readStringIntoBuffer (file, &valueBuffer, parentKey))
				{
					keyDel (k);
					elektraFree (nameBuffer.string);
					elektraFree (metaNameBuffer.string);
					elektraFree (valueBuffer.string);
					fclose (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}
				const char * metaValue = valueBuffer.string;

				keySetMeta (k, metaNameBuffer.string, metaValue);
				break;
			}
			case 'c':
			{
				// copy meta
				if (!readStringIntoBuffer (file, &nameBuffer, parentKey))
				{
					keyDel (k);
					elektraFree (nameBuffer.string);
					elektraFree (metaNameBuffer.string);
					elektraFree (valueBuffer.string);
					fclose (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				if (!readStringIntoBuffer (file, &metaNameBuffer, parentKey))
				{
					keyDel (k);
					elektraFree (nameBuffer.string);
					elektraFree (metaNameBuffer.string);
					elektraFree (valueBuffer.string);
					fclose (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				const Key * sourceKey = ksLookupByName (returned, nameBuffer.string, 0);
				if (sourceKey == NULL)
				{
					ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not copy meta data from key '%s': Key not found",
								     nameBuffer.string);
					keyDel (k);
					elektraFree (nameBuffer.string);
					elektraFree (metaNameBuffer.string);
					elektraFree (valueBuffer.string);
					fclose (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				if (keyCopyMeta (k, sourceKey, metaNameBuffer.string) != 1)
				{
					ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not copy meta data from key '%s': Error during copy",
								     &nameBuffer.string[nameBuffer.offset]);
					keyDel (k);
					elektraFree (nameBuffer.string);
					elektraFree (metaNameBuffer.string);
					elektraFree (valueBuffer.string);
					fclose (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}
				break;
			}
			default:
				keyDel (k);
				elektraFree (nameBuffer.string);
				elektraFree (metaNameBuffer.string);
				elektraFree (valueBuffer.string);
				fclose (file);
				ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Unknown meta type %c", type);
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}
		}

		ksAppendKey (returned, k);
	}

	elektraFree (nameBuffer.string);
	elektraFree (metaNameBuffer.string);
	elektraFree (valueBuffer.string);

	fclose (file);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}
