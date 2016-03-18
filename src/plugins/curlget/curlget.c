/**
 * @file
 *
 * @brief Source for curlget plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "curlget.h"

#include <kdbhelper.h>
#include <stdio.h>
#include <curl/curl.h>
#include <curl/easy.h>
#include <string.h>

int elektraCurlgetGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/curlget"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/curlget", KEY_VALUE, "curlget plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/curlget/exports", KEY_END),
			       keyNew ("system/elektra/modules/curlget/exports/get", KEY_FUNC, elektraCurlgetGet, KEY_END),
			       keyNew ("system/elektra/modules/curlget/exports/set", KEY_FUNC, elektraCurlgetSet, KEY_END),
#include ELEKTRA_README (curlget)
			       keyNew ("system/elektra/modules/curlget/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}

	const char * filename = keyString (parentKey);

	KeySet * config = elektraPluginGetConfig (handle);
	Key * rkey = ksLookupByName (config, "/url", KDB_O_NONE);
	if (!rkey)
	{
		return 0;
	}
	const char * url = keyString (rkey);

	CURL * curl = curl_easy_init ();
	if (!curl)
	{
		return 0;
	}
	FILE * fp;
	fp = fopen (filename, "wb");
	if (!fp)
	{
		return 0;
	}
	curl_easy_setopt (curl, CURLOPT_URL, url);
	curl_easy_setopt (curl, CURLOPT_WRITEFUNCTION, NULL);
	curl_easy_setopt (curl, CURLOPT_WRITEDATA, fp);
	CURLcode res;
	res = curl_easy_perform (curl);
	curl_easy_cleanup (curl);
	fclose (fp);
	if (res != CURLE_OK)
	{
		return 0;
	}
	return 1; // success
}

int elektraCurlgetSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// get all keys

	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT (curlget)
{
	// clang-format off
	return elektraPluginExport ("curlget",
		ELEKTRA_PLUGIN_GET,	&elektraCurlgetGet,
		ELEKTRA_PLUGIN_SET,	&elektraCurlgetSet,
		ELEKTRA_PLUGIN_END);
}

