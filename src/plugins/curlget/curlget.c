/**
 * @file
 *
 * @brief Source for curlget plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include "curlget.h"

#include <curl/curl.h>
#include <curl/easy.h>
#include <kdbhelper.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <openssl/md5.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>

#define TMP_NAME "/tmp/elektraCurlTempXXXXXXXXXXXX"


typedef struct
{
	char *path;
	char *tmpFile;
	time_t mtime;
	unsigned char lastHash[MD5_DIGEST_LENGTH];
	char *getUrl;
	char *putUrl;
	int setPhase;
}Data;


int elektraCurlgetCheckFile (const char * filename)
{
	if (filename[0] == '/') return 0;

	return 1;
}

int elektraCurlgetOpen(Plugin * handle, Key *errorKey)
{
	KeySet *config = elektraPluginGetConfig(handle);
	if(ksLookupByName(config, "/module", 0)) return 0;
	const char *path = keyString(ksLookupByName(config, "/path", 0));
	Data *data = elektraCalloc(sizeof(Data));	
	data->path = path;
	
	Key * key = ksLookupByName (config, "/get", KDB_O_NONE);
	if (!key)
	{
		elektraFree(data);
		data = NULL;;
	}
	else
	{
	    data->getUrl = keyString(key);
	}
	key = ksLookupByName(config, "/put", KDB_O_NONE);
	if(!key)
	{
	    data->putUrl = NULL;
	}
	else
	{
	    data->putUrl = keyString(key);
	}

	elektraPluginSetData(handle, data);
}

static unsigned char * hashBuffer(void *buffer, size_t size)
{
    int n;
    MD5_CTX c;
    unsigned char *out = elektraMalloc(MD5_DIGEST_LENGTH);
    MD5_Init(&c);
    MD5_Update(&c, buffer, size);
    MD5_Final(out, &c);
    return out;
}


static FILE * fetchFile(Data *data, int fd)
{
	CURL * curl = curl_easy_init ();
	if (!curl)
	{
		curl_easy_cleanup(curl);
		return NULL;
	}
	
	curl_easy_setopt (curl, CURLOPT_URL, data->getUrl);
	curl_easy_setopt (curl, CURLOPT_WRITEFUNCTION, NULL);
	FILE *fp = fdopen(fd, "w+");
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
			       keyNew ("system/elektra/modules/curlget/exports/checkfile", KEY_FUNC, elektraCurlgetCheckFile, KEY_END),
#include ELEKTRA_README (curlget)
			       keyNew ("system/elektra/modules/curlget/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}

	Data *data = elektraPluginGetData(handle);
	if(!data)
	{
	    return -1;
	}
	int fd = 0;
	if(!*(data->lastHash))
	{
	    fd = open(data->path, O_CREAT|O_RDWR, 00755);
	}
	else
	{
	    char name[] = "/tmp/elektraCurlTempXXXXXX";
	    fd = mkstemp(name);
	    data->tmpFile = name;
	}

	if(fd == -1)
	{
	    return -1;
	}
	
	FILE *fp = fetchFile(data, fd);
	fseek(fp, 0L, SEEK_END);
	size_t size = ftell(fp);
	rewind(fp);
	unsigned char buffer[size];
	int bytes = fread(&buffer, sizeof(char), size, fp);
	fclose(fp);
	unsigned char *hash = hashBuffer(buffer, size);
	if(!*(data->lastHash))
	{
	    memcpy(data->lastHash, hash, MD5_DIGEST_LENGTH);
	    keySetString(parentKey, data->path);
	}
	else
	{
	    if(strncmp(data->lastHash, hash, MD5_DIGEST_LENGTH))
	    {
		//remote file has changed, replace local copy with it.
		rename(data->tmpFile, data->path);
		data->tmpFile = NULL;
		keySetString(parentKey, data->path);
		memcpy(data->lastHash, hash, MD5_DIGEST_LENGTH);

	    }
	    else
	    {
		//remote file is the same as our local copy
		unlink(data->tmpFile);
		data->tmpFile = NULL;
		keySetString(parentKey, data->path);
	    }
	}
	elektraFree(hash);
	    
	return 1; // success
}

int elektraCurlgetSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// get all keys
	Data *data = elekrtaPluginGetData(handle);
	if(!data)
	    return -1;
	if(data->setPhase == 0)
	{
		char name[] = "/tmp/elektraCurlTempXXXXXX";
		int fd = mkstemp(name);
		data->tmpFile = name;
		FILE *fp = fetchFile(data, fd);	
		fseek(fp, 0L, SEEK_END);
		size_t size = ftell(fp);
		rewind(fp);
		unsigned char buffer[size];
		int bytes = fread(&buffer, sizeof(char), size, fp);
		fclose(fp);
		unsigned char *hash = hashBuffer(buffer, size);
		++(data->setPhase);
		if(strncmp(data->lastHash, hash, MD5_CTX))
		{
		    ELEKTRA_SET_ERROR( ELEKTRA_ERROR_CONFLICT, parentKey, "remote file has changed");
		}
	}
	else if(setPhase == 1)
	{


	}

    
    	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT (curlget)
{
	// clang-format off
	return elektraPluginExport ("curlget",
		ELEKTRA_PLUGIN_GET,	&elektraCurlgetGet,
		ELEKTRA_PLUGIN_SET,	&elektraCurlgetSet,
		ELEKTRA_PLUGIN_OPEN,	&elektraCurlgetOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraCurlgetClose,
		ELEKTRA_PLUGIN_END);
}

