/**
 * @file
 *
 * @brief Source for c plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "c.h"

#include <kdberrors.h>
#include <kdbhelper.h>

#include <stdio.h>

/**
 * Generate a C-Style key and stream it.
 *
 * This keyset can be used to include as c-code for
 * applikations using elektra.
 *
 * @param key the key object to work with
 * @param stream the file pointer where to send the stream
 * @param options KDB_O_SHOWINDICES, KDB_O_IGNORE_COMMENT, KDB_O_SHOWINFO
 * @retval 1 on success
 * @ingroup stream
 */
int keyGenerate (const Key * key, FILE * stream, option_t options)
{
	size_t s;
	char * str;

	size_t n;
	char * nam;

	n = keyGetNameSize (key);
	if (n > 1)
	{
		nam = (char *)elektraMalloc (n);
		if (nam == NULL) return -1;
		keyGetName (key, nam, n);
		fprintf (stream, "\tkeyNew (\"%s\"", nam);
		elektraFree (nam);
	}

	s = keyGetValueSize (key);
	if (s > 1)
	{
		str = (char *)elektraMalloc (s);
		if (str == NULL) return -1;
		if (keyIsBinary (key))
		{
			keyGetBinary (key, str, s);
			fprintf (stream, ", KEY_SIZE, \"%zu\"", keyGetValueSize (key));
		}
		else
		{
			keyGetString (key, str, s);
		}
		fprintf (stream, ", KEY_VALUE, \"%s\"", str);
		elektraFree (str);
	}

	const Key * meta;
	Key * dup = keyDup (key);
	keyRewindMeta (dup);
	while ((meta = keyNextMeta (dup)))
	{
		fprintf (stream, ", KEY_META, \"%s\", \"%s\"", keyName (meta), keyString (meta));
	}
	keyDel (dup);

	fprintf (stream, ", KEY_END)");

	if (options == 0) return 1; /* dummy to make icc happy */
	return 1;
}


/**
 * Generate a C-Style keyset and stream it.
 *
 * This keyset can be used to include as c-code for
 * applikations using elektra.
 *
 * The options takes the same options as kdbGet()
 * and kdbSet().
 *
 * @param ks the keyset to work with
 * @param stream the file pointer where to send the stream
 * @param options which keys not to output
 * @retval 1 on success
 * @ingroup stream
 */
int ksGenerate (const KeySet * ks, FILE * stream, option_t options)
{
	Key * key;
	size_t s = 0;
	KeySet * cks = ksDup (ks);

	ksRewind (cks);

	fprintf (stream, "ksNew (%d,\n", (int)ksGetSize (cks));
	while ((key = ksNext (cks)) != 0)
	{
		if (options & KDB_O_INACTIVE)
			if (key && keyIsInactive (key)) continue;

		s++;

		keyGenerate (key, stream, options);
		fprintf (stream, ",\n");
	}
	fprintf (stream, "\tKS_END);\n");

	ksDel (cks);
	return 1;
}

int elektraCGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/c"))
	{
		KeySet * contract = ksNew (30, keyNew ("system/elektra/modules/c", KEY_VALUE, "c plugin waits for your orders", KEY_END),
					   keyNew ("system/elektra/modules/c/exports", KEY_END),
					   keyNew ("system/elektra/modules/c/exports/get", KEY_FUNC, elektraCGet, KEY_END),
					   keyNew ("system/elektra/modules/c/exports/set", KEY_FUNC, elektraCSet, KEY_END),
					   keyNew ("system/elektra/modules/c/exports/checkconf", KEY_FUNC, elektraCCheckConfig, KEY_END),
#include ELEKTRA_README (c)
					   keyNew ("system/elektra/modules/c/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys

	return 1; // success
}

int elektraCSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	FILE * fp = fopen (keyString (parentKey), "w");

	if (!fp)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		return -1;
	}

	ksGenerate (returned, fp, 0);

	return 1; // success
}

int elektraCCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	// the return codes have the following meaning:
	// 0: The configuration was OK and has not been changed
	// 1: The configuration has been changed and now it is OK
	// -1: The configuration was not OK and could not be fixed. An error has to be set to errorKey.
	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (c)
{
	// clang-format off
	return elektraPluginExport ("c",
		ELEKTRA_PLUGIN_GET,	&elektraCGet,
		ELEKTRA_PLUGIN_SET,	&elektraCSet,
		ELEKTRA_PLUGIN_END);
}

