/**
 * @file
 *
 * @brief Shared methods for key and keyset copy-on-write.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <internal/kdbprivate.h>

/**
 * @internal
 *
 * @brief Create an empty KeyName object
 *
 * @return 0-initialized object
 */
struct _KeyName * keyNameNew (void)
{
	struct _KeyName * name = elektraCalloc (sizeof (struct _KeyName));
	return name;
}

/**
 * @internal
 *
 * @brief Increment the reference counter of a KeyName object
 *
 * @note The reference counter can never exceed `UINT16_MAX - 1`. `UINT16_MAX` is
 * reserved as an error code.
 *
 * @post @p keyname's reference counter is > 0
 * @post @p keyname's reference counter is <= UINT16_MAX - 1
 *
 * @param keyname the Key Name object whose reference counter should be increased
 *
 * @return the updated value of the reference counter
 * @retval UINT16_MAX on NULL pointer
 * @retval UINT16_MAX when the reference counter already was the maximum value `UINT16_MAX - 1`,
 *         the reference counter will not be modified in this case
 */
uint16_t keyNameRefInc (struct _KeyName * keyname)
{
	if (!keyname)
	{
		return UINT16_MAX;
	}

	if (keyname->refs == UINT16_MAX - 1)
	{
		return UINT16_MAX;
	}

	keyname->refs++;
	return keyname->refs;
}

/**
 * @internal
 *
 * @brief Decrement the reference counter of a KeyName object
 *
 * @post @p keyname's reference counter is >= 0
 * @post @p keyname's reference counter is < SSIZE_MAX
 *
 * @param keyname the KeyName object whose reference counter should get decreased
 *
 * @return the updated value of the reference counter
 * @retval UINT16_MAX on NULL pointer
 * @retval 0 when the reference counter already was the minimum value 0,
 *         the reference counter will not be modified in this case
 */
uint16_t keyNameRefDec (struct _KeyName * keyname)
{
	if (!keyname)
	{
		return UINT16_MAX;
	}

	if (keyname->refs == 0)
	{
		return 0;
	}

	keyname->refs--;
	return keyname->refs;
}

/**
 * @internal
 *
 * @brief Decrement the reference counter of a KeyName object and delete it if the counter reaches 0.
 *
 * @param keyname the KeyName object whose reference counter should get decreased
 *
 * @return the updated value of the reference counter
 * @retval UINT16_MAX on NULL pointer
 * @retval 0 when the reference counter already was the minimum value 0,
 *         the object has been deleted in this case
 */
uint16_t keyNameRefDecAndDel (struct _KeyName * keyname)
{
	if (!keyname)
	{
		return UINT16_MAX;
	}

	uint16_t refs = keyNameRefDec (keyname);
	if (keyname->refs == 0)
	{
		keyNameDel (keyname);
	}
	return refs;
}

/**
 * @internal
 *
 * @brief Delete a KeyName object if its reference counter is 0
 *
 * @param keyname the KeyName object whose reference counter should get decreased
 */
void keyNameDel (struct _KeyName * keyname)
{
	if (!keyname)
	{
		return;
	}

	if (keyname->refs == 0)
	{
		if (!isKeyNameInMmap (keyname))
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

/**
 * @internal
 *
 * @brief Create an empty KeyData object
 *
 * @return 0-initialized object
 */
struct _KeyData * keyDataNew (void)
{
	struct _KeyData * data = elektraCalloc (sizeof (struct _KeyData));
	return data;
}

/**
 * @internal
 *
 * @brief Increment the reference counter of a KeyData object
 *
 * @note The reference counter can never exceed `UINT16_MAX - 1`. `UINT16_MAX` is
 * reserved as an error code.
 *
 * @post @p keydata's reference counter is > 0
 * @post @p keydata's reference counter is <= UINT16_MAX - 1
 *
 * @param keydata the KeyData object whose reference counter should be increased
 *
 * @return the updated value of the reference counter
 * @retval UINT16_MAX on NULL pointer
 * @retval UINT16_MAX when the reference counter already was the maximum value `UINT16_MAX - 1`,
 *         the reference counter will not be modified in this case
 */
uint16_t keyDataRefInc (struct _KeyData * keydata)
{
	if (!keydata)
	{
		return UINT16_MAX;
	}

	if (keydata->refs == UINT16_MAX - 1)
	{
		return UINT16_MAX;
	}

	keydata->refs++;
	return keydata->refs;
}

/**
 * @internal
 *
 * @brief Decrement the reference counter of a KeyData object
 *
 * @post @p keydata's reference counter is >= 0
 * @post @p keydata's reference counter is < SSIZE_MAX
 *
 * @param keydata the KeyData object whose reference counter should get decreased
 *
 * @return the updated value of the reference counter
 * @retval UINT16_MAX on NULL pointer
 * @retval 0 when the reference counter already was the minimum value 0,
 *         the reference counter will not be modified in this case
 */
uint16_t keyDataRefDec (struct _KeyData * keydata)
{
	if (!keydata)
	{
		return UINT16_MAX;
	}

	if (keydata->refs == 0)
	{
		return 0;
	}

	keydata->refs--;
	return keydata->refs;
}

/**
 * @internal
 *
 * @brief Decrement the reference counter of a KeyData object and delete it if the counter reaches 0.
 *
 * @param keydata the KeyData object whose reference counter should get decreased
 *
 * @return the updated value of the reference counter
 * @retval UINT16_MAX on NULL pointer
 * @retval 0 when the reference counter already was the minimum value 0,
 *         the object has been deleted in this case
 */
uint16_t keyDataRefDecAndDel (struct _KeyData * keydata)
{
	if (!keydata)
	{
		return UINT16_MAX;
	}

	uint16_t refs = keyDataRefDec (keydata);
	if (keydata->refs == 0)
	{
		keyDataDel (keydata);
	}
	return refs;
}

/**
 * @internal
 *
 * @brief Delete a KeyData object if its reference counter is 0
 *
 * @param keydata the KeyName object whose reference counter should get decreased
 * @param deleteData if the data (data.v) should be freed
 */
void keyDataDel (struct _KeyData * keydata)
{
	if (!keydata)
	{
		return;
	}

	if (keydata->refs == 0)
	{
		if (!isKeyDataInMmap (keydata) && keydata->data.v != NULL)
		{
			elektraFree (keydata->data.v);
		}

		elektraFree (keydata);
	}
}

/**
 * @internal
 *
 * @brief Create an empty KeySetData object
 *
 * @return 0-initialized object
 */
struct _KeySetData * keySetDataNew (void)
{
	struct _KeySetData * data = elektraCalloc (sizeof (struct _KeySetData));
	return data;
}

/**
 * @internal
 *
 * @brief Increment the reference counter of a KeySetData object
 *
 * @note The reference counter can never exceed `UINT16_MAX - 1`. `UINT16_MAX` is
 * reserved as an error code.
 *
 * @post @p keysetdata's reference counter is > 0
 * @post @p keysetdata's reference counter is <= UINT16_MAX - 1
 *
 * @param keysetdata the KeySetData object whose reference counter should be increased
 *
 * @return the updated value of the reference counter
 * @retval UINT16_MAX on NULL pointer
 * @retval UINT16_MAX when the reference counter already was the maximum value `UINT16_MAX - 1`,
 *         the reference counter will not be modified in this case
 */
uint16_t keySetDataRefInc (struct _KeySetData * keysetdata)
{
	if (!keysetdata)
	{
		return UINT16_MAX;
	}

	if (keysetdata->refs == UINT16_MAX - 1)
	{
		return UINT16_MAX;
	}

	keysetdata->refs++;
	return keysetdata->refs;
}

/**
 * @internal
 *
 * @brief Decrement the reference counter of a KeySetData object
 *
 * @post @p keysetdata's reference counter is >= 0
 * @post @p keysetdata's reference counter is < SSIZE_MAX
 *
 * @param keysetdata the KeySetData object whose reference counter should get decreased
 *
 * @return the updated value of the reference counter
 * @retval UINT16_MAX on NULL pointer
 * @retval 0 when the reference counter already was the minimum value 0,
 *         the reference counter will not be modified in this case
 */
uint16_t keySetDataRefDec (struct _KeySetData * keysetdata)
{
	if (!keysetdata)
	{
		return UINT16_MAX;
	}

	if (keysetdata->refs == 0)
	{
		return 0;
	}

	keysetdata->refs--;
	return keysetdata->refs;
}

/**
 * @internal
 *
 * @brief Decrement the reference counter of a KeySetData object and delete it if the counter reaches 0.
 *
 * @param keysetdata the KeySetData object whose reference counter should get decreased
 *
 * @return the updated value of the reference counter
 * @retval UINT16_MAX on NULL pointer
 * @retval 0 when the reference counter already was the minimum value 0,
 *         the object has been deleted in this case
 */
uint16_t keySetDataRefDecAndDel (struct _KeySetData * keysetdata)
{
	if (!keysetdata)
	{
		return UINT16_MAX;
	}

	uint16_t refs = keySetDataRefDec (keysetdata);
	if (keysetdata->refs == 0)
	{
		keySetDataDel (keysetdata);
	}
	return refs;
}

/**
 * @internal
 *
 * @brief Delete a KeySetData object if its reference counter is 0
 *
 * @param keysetdata the KeyName object whose reference counter should get decreased
 */
void keySetDataDel (struct _KeySetData * keysetdata)
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

		if (keysetdata->array && !isKeySetDataInMmap (keysetdata))
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
