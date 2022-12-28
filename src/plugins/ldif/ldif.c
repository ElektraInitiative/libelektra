/**
 * @file
 *
 * @brief Source for ldif plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#define _GNU_SOURCE
#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include "ldif.h"
#include <errno.h>

#include <kdbease.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>

#include <stdio.h>
#include <stdlib.h>

#define LDAP_DEPRECATED 0 // Make sure LDAP_DEPRECATED is defined

#include <ldap.h>
#include <ldif.h>

/**
 * Splits the provided string by comma and return it as an array of string. Make sure to
 * free the return value after you used it.
 *
 * @param in the string to parse.
 * @returns The resulting string array or NULL if an error occurred.
 */
char ** parseToken (const char * in)
{
	char * localCopy = elektraStrDup (in);
	if (localCopy == NULL)
	{
		return NULL;
	}

	// calculate the array size
	int size = 1;
	for (char * s = localCopy;; s++)
	{
		s = strpbrk (s, ",");
		if (!s) break;
		size++;
	}

	char ** res = (char **) elektraMalloc ((size + 1) * sizeof (char *));

	if (res == NULL)
	{
		elektraFree (localCopy);
		return NULL;
	}

	char * savePtr;

	// fill the array with the split values
	int i = 0;
	for (char * s = strtok_r (localCopy, ",", &savePtr); s != NULL; s = strtok_r (NULL, ",", &savePtr))
	{
		res[i] = elektraStrDup (s);

		if (res[i] == NULL)
		{
			for (--i; i >= 0; i--)
			{
				elektraFree (res[i]);
			}
			elektraFree (res);
			elektraFree (localCopy);
			return NULL;
		}
		i++;
	}
	res[i] = NULL;

	elektraFree (localCopy);
	return (res);
}

/**
 * Builds a key from seperated by a slash from the provided array. It stops after it reached the
 * desired length or the maximal array length. Make sure to free the return value after you used
 * it.
 *
 * @param in the array the should be concatenated
 * @param len the maximal length of array elements to use for the concatenation
 * @return a string of the the array elements concatenated by a slash.
 */
char * makeKey (const char ** in, int len)
{
	// calculate string size
	size_t size = 0;
	for (int i = 0; i < len && in[i] != NULL; i++)
	{
		const char * s = in[i];
		size += strlen (s) + 1;
	}

	if (size == 0)
	{
		return NULL;
	}

	// trim extra sep len
	size -= 1;

	char * out = elektraMalloc (size + 1);
	if (out == NULL)
	{
		return NULL;
	}

	// fill the string from the array values
	char * p = out;
	for (int i = len - 1; i >= 0; i--)
	{
		if (in[i] == NULL) continue;
		const char * s = in[i];
		if (i != len - 1)
		{
			*p = '/';
			p += 1;
		}

		strcpy (p, s);
		p += strlen (s);
	}

	*p = '\0';
	return out;
}

int elektraLdifGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/ldif"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/ldif", KEY_VALUE, "ldif plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/ldif/exports", KEY_END),
			       keyNew ("system:/elektra/modules/ldif/exports/get", KEY_FUNC, elektraLdifGet, KEY_END),
			       keyNew ("system:/elektra/modules/ldif/exports/set", KEY_FUNC, elektraLdifSet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/ldif/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys
	int errnosave = errno;
	const char * filename = keyString (parentKey);
	LDIFFP * lfp = ldif_open (filename, "r");

	ELEKTRA_LOG ("Read from '%s'", keyString (parentKey));

	if (!lfp)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	unsigned long lineno = 0;
	char * buff = NULL;
	int buflen = 0;

	unsigned long ldif_order = 0;

	while (ldif_read_record (lfp, &lineno, &buff, &buflen))
	{
		char * line;
		char * next = buff;
		char * last_dn = NULL;

		while ((line = ldif_getline (&next)) != NULL)
		{
			char * type = NULL;
			char * value = NULL;


			ber_len_t vlen;

			if (ldif_parse_line (line, &type, &value, &vlen))
			{
				ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF (parentKey, "cannot parse line: %s\n", line);
				continue;
			}

			if (elektraStrCmp (type, "dn") == 0)
			{
				ELEKTRA_LOG_DEBUG ("found key: %s\n", value);

				char ** tokens = parseToken (value);

				if (tokens == NULL)
				{
					ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
						parentKey, "Failed to parse tokens from file %s at position %ld from %s", filename,
						ftell (lfp->fp), value);

					elektraFree (buff);
					ldif_close (lfp);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				int size = 0;

				while (tokens[size] != NULL)
				{
					size++;
				}

				for (int i = 0; i < size; ++i)
				{

					Key * key = keyNew (keyName (parentKey), KEY_END);
					char * domainpart = makeKey ((const char **) tokens, i + 1);

					ELEKTRA_LOG_DEBUG ("saving key: %s\n", domainpart);

					if (domainpart == NULL)
					{
						ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
							parentKey, "Failed to extract key from file %s at position %ld with key %s",
							filename, ftell (lfp->fp), value);
						elektraFree (domainpart);
						elektraFree (tokens);
						elektraFree (buff);
						ldif_close (lfp);
						return ELEKTRA_PLUGIN_STATUS_ERROR;
					}

					if (last_dn != NULL)
					{
						elektraFree (last_dn);
					}
					last_dn = strdup (domainpart);

					if (keyAddName (key, domainpart) == -1)
					{
						ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF (
							parentKey, "Key name '%s' is not valid, discarding key", value);
						elektraFree (domainpart);
						keyDel (key);
						break;
					}

					ksAppendKey (returned, key);
					elektraFree (domainpart);
				}

				for (int i = 0; i < size; i++)
				{
					elektraFree (tokens[i]);
				}
				elektraFree (tokens);
			}
			else
			{
				ELEKTRA_LOG_DEBUG ("found value: type: %s, value: %s\n", type, value);
			}

			const char * attribute_key_parts[] = { type, last_dn, keyName (parentKey) };
			char * attribute_key_name = makeKey (attribute_key_parts, 3);
			Key * attribute_key = keyNew (attribute_key_name, KEY_END);
			ELEKTRA_LOG_DEBUG ("storing value %s at key %s\n", value, keyName (attribute_key));

			keySetString (attribute_key, value);
			size_t order_length = snprintf (NULL, 0, "%lu", ldif_order);
			char * order_str = elektraMalloc (order_length + 1);
			snprintf (order_str, order_length + 1, "%lu", ldif_order);
			keySetMeta (attribute_key, "order", order_str);
			ksAppendKey (returned, attribute_key);

			elektraFree (attribute_key_name);
			elektraFree (order_str);
			elektraFree (type);
			elektraFree (value);

			ldif_order++;
		}

		if (last_dn != NULL)
		{
			elektraFree (last_dn);
		}
	}

	free (buff);

	if (feof (lfp->fp) == 0)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Not at the end of file at position %ld in file %s", ftell (lfp->fp),
							 filename);
		ldif_close (lfp);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	ldif_close (lfp);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/**
 * Compare keys by their order meta key.
 * @param a pointer to the first key
 * @param b pointer to the second key
 * @return -1 iff the order of a is less than of b, 0 if both are equal and 1 otherwise
 */
static int compareKeysByOrder (const void * a, const void * b)
{
	const Key * key_a = *(const Key **) a;
	const Key * key_b = *(const Key **) b;
	long key_a_order = strtol (keyString (keyGetMeta (key_a, "order")), NULL, 10);
	long key_b_order = strtol (keyString (keyGetMeta (key_b, "order")), NULL, 10);
	printf ("%s > %s\n", keyString (key_a), keyString (key_b));
	printf ("%ld > %ld?\n", key_a_order, key_b_order);
	if (key_a_order < key_b_order)
	{
		return -1;
	}

	if (key_a_order > key_b_order)
	{
		return 1;
	}

	return 0;
}

int elektraLdifSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	// set all keys
	// this function is optional
	int errnosave = errno;
	const char * filename = keyString (parentKey);
	LDIFFP * lfp = ldif_open (filename, "w");

	ELEKTRA_LOG ("Write to '%s'", keyString (parentKey));

	if (!lfp)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Key ** keyArray;
	size_t arraySize = ksGetSize (returned);
	keyArray = calloc (arraySize, sizeof (Key *));

	ksRewind (returned);
	int ret = elektraKsToMemArray (returned, keyArray);

	if (ret < 0)
	{
		ELEKTRA_SET_RESOURCE_ERROR (parentKey, strerror (errno));
		return -1;
	}

	qsort (keyArray, arraySize, sizeof (Key *), compareKeysByOrder);

	int first_line = 1;
	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		Key * cur = keyArray[it];
		const char * type = keyBaseName (cur);
		if (strchr (type, '=') == NULL)
		{
			ELEKTRA_LOG_DEBUG ("%s is a valid ldif type, proceeding writing\n", type);

			const char * dn = keyString (cur);
			ELEKTRA_LOG_DEBUG ("processing name: '%s', curr: '%s':'%s'\n", elektraKeyGetRelativeName (cur, parentKey), dn,
					   keyBaseName (cur));
			char * data = ldif_put_wrap (LDIF_PUT_VALUE, type, dn, strlen (dn), LDIF_LINE_WIDTH);

			int stat = EOF + 1;
			if (elektraStrCmp (type, "dn") == 0 && !first_line)
			{
				stat = fputs ("\n", lfp->fp);
			}
			first_line = 0;

			if (stat == EOF || fputs (data, lfp->fp) == EOF)
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Could not write to position %ld in file %s",
									 ftell (lfp->fp), filename);
				ber_memfree (data);
				elektraFree (keyArray);
				ldif_close (lfp);
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}

			ber_memfree (data);
		}
		else
		{
			ELEKTRA_LOG_WARNING ("skipping: '%s', curr: '%s':'%s' because '=' are not allowed in ldif attributes\n",
					     elektraKeyGetRelativeName (cur, parentKey), keyString (cur), keyBaseName (cur));
		}
	}

	elektraFree (keyArray);
	ldif_close (lfp);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("ldif",
		ELEKTRA_PLUGIN_GET,	&elektraLdifGet,
		ELEKTRA_PLUGIN_SET,	&elektraLdifSet,
		ELEKTRA_PLUGIN_END);
}
