/**
 * @file
 *
 * @brief Source for curlget plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include "curlget.h"
#define _XOPEN_SOURCE
#include <curl/curl.h>
#include <curl/easy.h>
#include <fcntl.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <openssl/md5.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#define TMP_NAME "/tmp/elektraCurlTempXXXXXXXXXXXX"

#define DEFAULT_POSTFIELDNAME "file"

typedef enum { NA = 0, PUT, POST, FTP } UploadMethods;

typedef struct
{
	const char * path;
	char * tmpFile;
	time_t mtime;
	unsigned char lastHash[MD5_DIGEST_LENGTH];
	const char * getUrl;
	const char * uploadUrl;
	const char * uploadUser;
	const char * uploadPass;
	const char * postFieldName;
	UploadMethods uploadMethod;
	unsigned short preferRemote;
	int setPhase;
} Data;


int elektraCurlgetCheckFile (const char * filename)
{
	if (filename[0] == '/') return 0;

	return 1;
}

int elektraCurlgetClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	Data * data = elektraPluginGetData (handle);
	if (!data) return 0;
	if (data->tmpFile)
	{
		unlink (data->tmpFile);
		data->tmpFile = NULL;
	}
	if (data->path)
	{
		unlink (data->path);
		data->path = NULL;
	}
	elektraFree (data);
	data = NULL;
	elektraPluginSetData (handle, data);
	return 1;
}

int elektraCurlgetError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return 1;
}

int elektraCurlgetOpen (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	KeySet * config = elektraPluginGetConfig (handle);
	if (ksLookupByName (config, "/module", 0)) return 0;
	const char * path = keyString (ksLookupByName (config, "/path", 0));
	Data * data = elektraCalloc (sizeof (Data));
	data->path = path;

	Key * key = ksLookupByName (config, "/get", KDB_O_NONE);
	if (!key)
	{
		elektraFree (data);
		data = NULL;
		return 0;
	}
	else
	{
		data->getUrl = keyString (key);
	}
	key = ksLookupByName (config, "/upload", KDB_O_NONE);
	if (!key)
	{
		data->uploadUrl = NULL;
	}
	else
	{
		data->uploadUrl = keyString (key);
	}
	key = ksLookupByName (config, "/upload/user", KDB_O_NONE);
	if (key)
	{
		data->uploadUser = keyString (key);
	}
	key = ksLookupByName (config, "/upload/password", KDB_O_NONE);
	if (key)
	{
		data->uploadPass = keyString (key);
	}
	key = ksLookupByName (config, "/upload/method", KDB_O_NONE);
	if (key)
	{
		if (!(strcmp (keyString (key), "POST")))
		{
			data->uploadMethod = POST;
			key = ksLookupByName (config, "/upload/postfield", KDB_O_NONE);
			if (key)
			{
				data->postFieldName = keyString (key);
			}
			else
			{
				data->postFieldName = DEFAULT_POSTFIELDNAME;
			}
		}
		else if (!(strcmp (keyString (key), "PUT")))
		{
			data->uploadMethod = PUT;
		}
		else if (!(strcmp (keyString (key), "FTP")))
		{
			data->uploadMethod = FTP;
		}
		else
		{
			data->uploadMethod = NA;
		}
	}
	key = ksLookupByName (config, "/prefer", KDB_O_NONE);
	data->preferRemote = 1;
	if (key && !strcmp (keyString (key), "local"))
	{
		data->preferRemote = 0;
	}

	elektraPluginSetData (handle, data);
	return 1;
}

static unsigned char * hashBuffer (void * buffer, size_t size)
{
	MD5_CTX c;
	unsigned char * out = elektraMalloc (MD5_DIGEST_LENGTH);
	MD5_Init (&c);
	MD5_Update (&c, buffer, size);
	MD5_Final (out, &c);
	return out;
}


static FILE * fetchFile (Data * data, int fd)
{
	CURL * curl = curl_easy_init ();
	if (!curl)
	{
		curl_easy_cleanup (curl);
		return NULL;
	}

	curl_easy_setopt (curl, CURLOPT_URL, data->getUrl);
	curl_easy_setopt (curl, CURLOPT_WRITEFUNCTION, NULL);
	FILE * fp = fdopen (fd, "w+");
	curl_easy_setopt (curl, CURLOPT_WRITEDATA, fp);
	CURLcode res;
	res = curl_easy_perform (curl);
	curl_easy_cleanup (curl);
	if (res != CURLE_OK)
	{
		fclose (fp);
		return NULL;
	}
	return fp;
}


int elektraCurlgetGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/curlget"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/curlget", KEY_VALUE, "curlget plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/curlget/exports", KEY_END),
			       keyNew ("system/elektra/modules/curlget/exports/get", KEY_FUNC, elektraCurlgetGet, KEY_END),
			       keyNew ("system/elektra/modules/curlget/exports/set", KEY_FUNC, elektraCurlgetSet, KEY_END),
			       keyNew ("system/elektra/modules/curlget/exports/open", KEY_FUNC, elektraCurlgetOpen, KEY_END),
			       keyNew ("system/elektra/modules/curlget/exports/error", KEY_FUNC, elektraCurlgetError, KEY_END),
			       keyNew ("system/elektra/modules/curlget/exports/checkfile", KEY_FUNC, elektraCurlgetCheckFile, KEY_END),
#include ELEKTRA_README (curlget)
			       keyNew ("system/elektra/modules/curlget/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}

	Data * data = elektraPluginGetData (handle);
	if (!data)
	{
		return -1;
	}
	int fd = 0;
	if (!*(data->lastHash))
	{
		fd = open (data->path, O_CREAT | O_RDWR, 00755);
	}
	else
	{
		char name[] = "/tmp/elektraCurlTempXXXXXX";
		fd = mkstemp (name);
		data->tmpFile = name;
	}

	if (fd == -1)
	{
		ELEKTRA_SET_ERRORF (26, parentKey, "Failed to open %s for reading", data->path);
		return -1;
	}

	FILE * fp = fetchFile (data, fd);
	fseek (fp, 0L, SEEK_END);
	size_t size = ftell (fp);
	rewind (fp);
	unsigned char buffer[size];
	int bytes = fread (&buffer, sizeof (char), size, fp);
	fclose (fp);
	unsigned char * hash = hashBuffer (buffer, size);
	if (!*(data->lastHash))
	{
		memcpy (data->lastHash, hash, MD5_DIGEST_LENGTH);
		keySetString (parentKey, data->path);
	}
	else
	{
		if (strncmp ((char *)data->lastHash, (char *)hash, MD5_DIGEST_LENGTH))
		{
			// remote file has changed
			// if preferRemote is set: replace local copy with
			// the remote version.
			if (data->preferRemote)
			{
				rename (data->tmpFile, data->path);
				data->tmpFile = NULL;
				keySetString (parentKey, data->path);
				memcpy (data->lastHash, hash, MD5_DIGEST_LENGTH);
			}
			else
			{
				// else drop remote version
				goto UNLINK_TMP;
			}
		}
		else
		{
		UNLINK_TMP:
			// remote file is the same as our local copy
			unlink (data->tmpFile);
			data->tmpFile = NULL;
			keySetString (parentKey, data->path);
		}
	}
	elektraFree (hash);

	return 1; // success
}

int elektraCurlgetSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// get all keys
	Data * data = elektraPluginGetData (handle);
	if (!data) return -1;
	int retval = 1;
	if (data->setPhase == 0)
	{
		char name[] = "/tmp/elektraCurlTempXXXXXX";
		int fd = mkstemp (name);
		data->tmpFile = name;
		FILE * fp = fetchFile (data, fd);
		fseek (fp, 0L, SEEK_END);
		size_t size = ftell (fp);
		rewind (fp);
		unsigned char buffer[size];
		int bytes = fread (&buffer, sizeof (char), size, fp);
		fclose (fp);
		unsigned char * hash = hashBuffer (buffer, size);
		++(data->setPhase);
		if (strncmp ((char *)data->lastHash, (char *)hash, MD5_DIGEST_LENGTH))
		{
			ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CONFLICT, parentKey, "remote file has changed");
			retval = -1;
		}
		unlink (data->tmpFile);
		data->tmpFile = NULL;
		keySetString (parentKey, data->path);
	}
	else if (data->setPhase == 1)
	{
		FILE * fp;
		fp = fopen (data->path, "rb");
		if (!fp)
		{
			ELEKTRA_SET_ERRORF (26, parentKey, "Failed to open %s for reading", data->path);
			return -1;
		}
		fseek (fp, 0L, SEEK_END);
		size_t size = ftell (fp);
		rewind (fp);
		CURL * curl;
		CURLcode res = 0;
		curl = curl_easy_init ();
		if (curl)
		{
			curl_easy_setopt (curl, CURLOPT_URL, data->uploadUrl);
			if (data->uploadUser)
			{
				curl_easy_setopt (curl, CURLOPT_USERNAME, data->uploadUser);
			}
			if (data->uploadUser)
			{
				curl_easy_setopt (curl, CURLOPT_PASSWORD, data->uploadPass);
			}
			curl_easy_setopt (curl, CURLOPT_VERBOSE, 1L);
			if (data->uploadMethod == POST)
			{
				struct curl_httppost * formpost = NULL;
				struct curl_httppost * lastptr = NULL;
				struct curl_slist * headerlist = NULL;
				static const char buf[] = "Expect:";
				curl_formadd (&formpost, &lastptr, CURLFORM_COPYNAME, data->postFieldName, CURLFORM_FILE, data->path,
					      CURLFORM_END);
				headerlist = curl_slist_append (headerlist, buf);
				curl_easy_setopt (curl, CURLOPT_HTTPHEADER, headerlist);
				curl_easy_setopt (curl, CURLOPT_HTTPPOST, formpost);
				curl_easy_setopt (curl, CURLOPT_AUTOREFERER, 1L);
				res = curl_easy_perform (curl);
				if (res != CURLE_OK)
				{

					ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CURL_UPLOAD_FAILED, parentKey,
							    "curl upload (HTTP POST) failed: %s\n", curl_easy_strerror (res));
					retval = -1;
				}
				curl_formfree (formpost);
				curl_slist_free_all (headerlist);
			}
			else if (data->uploadMethod == PUT)
			{
				curl_easy_setopt (curl, CURLOPT_UPLOAD, 1L);
				curl_easy_setopt (curl, CURLOPT_READDATA, fp);
				curl_easy_setopt (curl, CURLOPT_CUSTOMREQUEST, "PUT");
				curl_easy_setopt (curl, CURLOPT_INFILESIZE_LARGE, (curl_off_t)size);
				res = curl_easy_perform (curl);
				if (res != CURLE_OK)
				{
					ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CURL_UPLOAD_FAILED, parentKey,
							    "curl upload (HTTP PUT) failed: %s", curl_easy_strerror (res));
					retval = -1;
				}
			}
			else if (data->uploadMethod == FTP)
			{
				curl_easy_setopt (curl, CURLOPT_READDATA, fp);
				curl_easy_setopt (curl, CURLOPT_UPLOAD, 1L);
				curl_easy_setopt (curl, CURLOPT_INFILESIZE_LARGE, (curl_off_t)size);
				res = curl_easy_perform (curl);
				if (res != CURLE_OK)
				{
					ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CURL_UPLOAD_FAILED, parentKey,
							    "curl upload (HTTP PUT) failed: %s", curl_easy_strerror (res));
					retval = -1;
				}
			}
			else
			{
				curl_easy_setopt (curl, CURLOPT_UPLOAD, 1L);
				curl_easy_setopt (curl, CURLOPT_READDATA, fp);
				curl_easy_setopt (curl, CURLOPT_INFILESIZE_LARGE, (curl_off_t)size);
				res = curl_easy_perform (curl);
				if (res != CURLE_OK)
				{
					ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CURL_UPLOAD_FAILED, parentKey,
							    "curl upload (HTTP PUT) failed: %s", curl_easy_strerror (res));
					retval = -1;
				}
			}
			curl_easy_cleanup (curl);
			fclose (fp);
		}
	}

	return retval; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT (curlget)
{
	// clang-format off
	return elektraPluginExport ("curlget",
		ELEKTRA_PLUGIN_GET,	&elektraCurlgetGet,
		ELEKTRA_PLUGIN_SET,	&elektraCurlgetSet,
		ELEKTRA_PLUGIN_OPEN,	&elektraCurlgetOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraCurlgetClose,
		ELEKTRA_PLUGIN_ERROR,	&elektraCurlgetError,
		ELEKTRA_PLUGIN_END);
}

