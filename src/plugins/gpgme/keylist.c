/**
 * @file
 *
 * @brief provides linked list of GPGME's gpgme_key_t for the gpgme plugin.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "keylist.h"
#include <elektra/kdbhelper.h>

/**
 * @brief initializes the internal key list
 * @param list the list
 */
void elektraGpgmeKeylistInit (keylist_t * list)
{
	list->head = NULL;
	list->iterator = NULL;
	list->size = 0;
}

/**
 * @brief add key to the list
 * @param list the key list
 * @param key the element to be added to the list
 * @retval 0 in case of insufficient memory (malloc failed)
 * @retval 1 in case of success
 */
int elektraGpgmeKeylistAdd (keylist_t * list, gpgme_key_t key)
{
	if (list->iterator)
	{
		// append key to existing list
		list->iterator->next = elektraMalloc (sizeof (struct internal_keylist));
		if (!list->iterator->next)
		{
			return 0; // not enough memory
		}
		list->iterator->next->key = key;
		list->iterator->next->next = NULL;
		list->iterator = list->iterator->next;
	}
	else
	{
		// first element in empty list
		list->head = elektraMalloc (sizeof (struct internal_keylist));
		if (!list->head)
		{
			return 0; // not enough memory
		}
		list->head->key = key;
		list->head->next = NULL;
		list->iterator = list->head;
	}
	list->size++;
	// instruct gpgme to not release the key handle until we are done with it
	// NOTE do not forget to invoke gpgme_key_unref when releasing the key list
	gpgme_key_ref (key);
	return 1; // success
}

/**
 * @brief reset the iterator of the list to the head of the list.
 */
void elektraGpgmeKeylistRewind (keylist_t * list)
{
	list->iterator = list->head;
}

/**
 * @brief get the next gpgme_key_t of the list.
 * @return the next element or NULL if no such element exists.
 */
gpgme_key_t elektraGpgmeKeylistNext (keylist_t * list)
{
	if (!list->iterator)
	{
		return NULL;
	}
	gpgme_key_t key = list->iterator->key;
	list->iterator = list->iterator->next;
	return key;
}

/**
 * @brief release the key list
 * @param list the list to be released
 */
void elektraGpgmeKeylistFree (keylist_t * list)
{
	struct internal_keylist * iterator = list->head;
	struct internal_keylist * next;

	while (iterator)
	{
		next = iterator->next;
		gpgme_key_unref (iterator->key);
		elektraFree (iterator);
		iterator = next;
	}
	list->head = NULL;
	list->iterator = NULL;
	list->size = 0;
}
