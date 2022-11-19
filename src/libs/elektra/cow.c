#include <kdbprivate.h>

struct _KeyName * keyNameNew (void)
{
	struct _KeyName * name = elektraMalloc (sizeof (struct _KeyName));
	name->key = NULL;
	name->keySize = 0;
	name->ukey = NULL;
	name->keyUSize = 0;
	name->refs = 0;
	return name;
}

uint16_t keyNameRefInc (struct _KeyName * keyname)
{
	if (!keyname)
	{
		return UINT16_MAX;
	}

	keyname->refs++;
	return keyname->refs;
}

uint16_t keyNameRefDec (struct _KeyName * keyname)
{
	if (!keyname)
	{
		return UINT16_MAX;
	}

	keyname->refs--;
	return keyname->refs;
}

uint16_t keyNameRefDecAndDel (struct _KeyName * keyname, bool deleteData)
{
	if (!keyname)
	{
		return UINT16_MAX;
	}

	uint16_t refs = keyNameRefDec (keyname);
	if (keyname->refs == 0)
	{
		keyNameDel (keyname, deleteData);
	}
	return refs;
}

void keyNameDel (struct _KeyName * keyname, bool deleteData)
{
	if (!keyname)
	{
		return;
	}

	if (keyname->refs == 0)
	{
		if (deleteData)
		{
			if (keyname->key)
			{
				elektraFree (keyname->key);
			}

			if (keyname->ukey)
			{
				elektraFree (keyname->ukey);
			}
		}

		elektraFree (keyname);
	}
}

struct _KeyData * keyDataNew (void)
{
	struct _KeyData * data = elektraMalloc (sizeof (struct _KeyData));
	data->data.v = NULL;
	data->dataSize = 0;
	data->refs = 0;
	return data;
}

uint16_t keyDataRefInc (struct _KeyData * keydata)
{
	if (!keydata)
	{
		return UINT16_MAX;
	}

	keydata->refs++;
	return keydata->refs;
}

uint16_t keyDataRefDec (struct _KeyData * keydata)
{
	if (!keydata)
	{
		return UINT16_MAX;
	}

	keydata->refs--;
	return keydata->refs;
}

uint16_t keyDataRefDecAndDel (struct _KeyData * keydata, bool deleteData)
{
	if (!keydata)
	{
		return UINT16_MAX;
	}

	uint16_t refs = keyDataRefDec (keydata);
	if (keydata->refs == 0)
	{
		keyDataDel (keydata, deleteData);
	}
	return refs;
}

void keyDataDel (struct _KeyData * keydata, bool deleteData)
{
	if (!keydata)
	{
		return;
	}

	if (keydata->refs == 0)
	{
		if (deleteData && keydata->data.v != NULL)
		{
			elektraFree (keydata->data.v);
		}

		elektraFree (keydata);
	}
}


struct _KeySetData * keySetDataNew (void)
{
	struct _KeySetData * data = elektraMalloc (sizeof (struct _KeySetData));
	memset (data, 0, sizeof (struct _KeySetData));
	return data;
}

uint16_t keySetDataRefInc (struct _KeySetData * keysetdata)
{
	if (!keysetdata)
	{
		return UINT16_MAX;
	}

	keysetdata->refs++;
	return keysetdata->refs;
}

uint16_t keySetDataRefDec (struct _KeySetData * keysetdata)
{
	if (!keysetdata)
	{
		return UINT16_MAX;
	}

	keysetdata->refs--;
	return keysetdata->refs;
}

uint16_t keySetDataRefDecAndDel (struct _KeySetData * keysetdata, bool deleteData)
{
	if (!keysetdata)
	{
		return UINT16_MAX;
	}

	uint16_t refs = keySetDataRefDec (keysetdata);
	if (keysetdata->refs == 0)
	{
		keySetDataDel (keysetdata, deleteData);
	}
	return refs;
}

void keySetDataDel (struct _KeySetData * keysetdata, bool deleteData)
{
	if (!keysetdata)
	{
		return;
	}

	if (keysetdata->refs == 0)
	{
		if (keysetdata->array)
		{
			for (size_t i = 0; i < keysetdata->size; i++)
			{
				keyDecRef (keysetdata->array[i]);
				keyDel (keysetdata->array[i]);
			}
		}

		if (keysetdata->array && deleteData)
		{
			elektraFree (keysetdata->array);
		}

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
		if (keysetdata->opmphm)
		{
			opmphmDel (keysetdata->opmphm);
		}
		if (keysetdata->opmphmPredictor)
		{
			opmphmPredictorDel (keysetdata->opmphmPredictor);
		}
#endif

		elektraFree (keysetdata);
	}
}
