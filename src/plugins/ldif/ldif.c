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

#include <ldap.h>
#include <ldif.h>

char ** parseToken (const char * in)
{
	char * localCopy = elektraStrDup (in);
	if (localCopy == NULL)
	{
		return NULL;
	}

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

char * makeKey (const char ** in, int len)
{
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

	/* trim extra sep len */
	size -= 1;

	char * out = elektraMalloc (size + 1);
	if (out == NULL)
	{
		return NULL;
	}

	char * p = out;
	for (int i = 0; i < len && in[i] != NULL; i++)
	{
		const char * s = in[i];
		if (i != 0)
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

void reverse (char ** arr, int size)
{
	for (int i = 0; i < size / 2; i++)
	{
		char * temp = arr[i];
		arr[i] = arr[size - 1 - i];
		arr[size - 1 - i] = temp;
	}
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

	if (!lfp)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	unsigned long lineno = 0;
	char * buff = NULL;
	int buflen = 0;


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
				elektraLog (ELEKTRA_LOG_LEVEL_DEBUG, "elektraLdifGet", filename, 0, "found key: %s\n", value);

				char ** tokens = parseToken (value);

				if (tokens == NULL)
				{
					ELEKTRA_SET_ERROR_GET (parentKey);

					elektraFree (buff);
					ldif_close (lfp);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				int size = 0;

				while (tokens[size] != NULL)
				{
					size++;
				}

				reverse (tokens, size);

				for (int i = 0; i < size; ++i)
				{

					Key * key = keyNew (keyName (parentKey), KEY_END);
					char * domainpart = makeKey ((const char **) tokens, i + 1);

					elektraLog (ELEKTRA_LOG_LEVEL_DEBUG, "elektraLdifGet", filename, 0, "saving key: %s\n", domainpart);

					if (domainpart == NULL)
					{
						ELEKTRA_SET_ERROR_GET (parentKey);
						elektraFree (domainpart);
						elektraFree (tokens);
						elektraFree (buff);
						ldif_close (lfp);
						return ELEKTRA_PLUGIN_STATUS_ERROR;
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

				elektraFree (tokens);
			}
			else
			{
				elektraLog (ELEKTRA_LOG_LEVEL_DEBUG, "elektraLdifGet", filename, 0, "found value: type: %s, value: %s\n",
					    type, value);
			}

			const char * attribute_key_parts[] = { keyName (parentKey), last_dn, type };
			char * attribute_key_name = makeKey (attribute_key_parts, 3);
			Key * attribute_key = keyNew (attribute_key_name, KEY_END);
			elektraLog (ELEKTRA_LOG_LEVEL_DEBUG, "elektraLdifGet", filename, 0, "storing value %s at key %s\n", value,
				    keyName (attribute_key));

			keySetString (attribute_key, value);
			ksAppendKey (returned, attribute_key);

			elektraFree (attribute_key_name);

			elektraFree (type);
			elektraFree (value);
		}

		if (last_dn != NULL)
		{
			elektraFree (last_dn);
		}
	}

	free (buff);

	ldif_close (lfp);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraLdifSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	// set all keys
	// this function is optional
	int errnosave = errno;
	const char * filename = keyString (parentKey);
	LDIFFP * lfp = ldif_open(filename, "w");

	ELEKTRA_LOG ("Write to '%s'", keyString (parentKey));

	if (!lfp)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Key * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != 0)
	{
		if (elektraStrCmp (keyBaseName (cur), "dn" ) == 0) {
			const char * dn = keyString (cur);
			ELEKTRA_LOG ("processing name: '%s', curr: '%s':'%s'\n", elektraKeyGetRelativeName (cur, parentKey), dn, keyBaseName (cur));
			char *data = ldif_put_wrap (LDIF_PUT_VALUE, "dn", dn, strlen (dn), LDIF_LINE_WIDTH );

			if (fputs (data, lfp->fp) == EOF || fputs ("\n", lfp->fp) == EOF)
			{
				ber_memfree (data);
				ELEKTRA_SET_ERROR_GET (parentKey);
				errno = errnosave;
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}

			ber_memfree(data);
		} else {
			ELEKTRA_LOG ("not processing name: '%s', curr: '%s':'%s'\n", elektraKeyGetRelativeName (cur, parentKey), keyString (cur), keyBaseName (cur));
		}
	}

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
