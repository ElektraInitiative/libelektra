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
#include <libgen.h>
#include <openssl/md5.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#define TMP_NAME "/tmp/elektraCurlTempXXXXXXXXXXXX"

#define DEFAULT_POSTFIELDNAME "file"

typedef enum { NA = 0, PUT, POST } HttpUploadMethods;
typedef enum {
	PROTO_INVALID = 0,
	PROTO_HTTP,
	PROTO_HTTPS,
	PROTO_FTP,
	PROTO_FTPS,
	PROTO_SFTP,
	PROTO_SCP,
	PROTO_SMB,
} ElektraCurlProtocol;
typedef enum {
	SSH_ANY = 0,
	SSH_AGENT,
	SSH_PASSWORD,
	SSH_PUBLICKEY,
	SSH_PUBKEYPW,
} SSHAuthType;
typedef struct
{
	const char * path;
	const char * tmpFile;
	time_t mtime;
	unsigned char lastHash[MD5_DIGEST_LENGTH];
	const char * getUrl;
	const char * uploadUrl;
	const char * user;
	const char * password;
	const char * postFieldName;
	const char * uploadFileName;
	char * __uploadFileName;
	HttpUploadMethods uploadMethod;
	unsigned short preferRemote;
	unsigned short setPhase;
	unsigned short useLocalCopy;
	unsigned short useSSL;
	unsigned short sslVerifyPeer;
	unsigned short sslVerifyHost;
	const char * keyFile;
	const char * keyFilePasswd;
	ElektraCurlProtocol getProto;
	ElektraCurlProtocol putProto;
	SSHAuthType sshAuth;
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
	if (!data->useLocalCopy && data->path)
	{
		unlink (data->path);
		data->path = NULL;
	}
	if (data->uploadFileName) elektraFree (data->__uploadFileName);
	elektraFree (data);
	data = NULL;
	elektraPluginSetData (handle, data);
	return 1;
}

int elektraCurlgetError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return 1;
}

static ElektraCurlProtocol isValidURL (const char * str)
{
	if (str == NULL) return PROTO_INVALID;

	struct checkProtocolStruct
	{
		ElektraCurlProtocol proto;
		const char * url;
	};

	const struct checkProtocolStruct urlPrefix[] = {
		{ PROTO_HTTP, "http://" }, { PROTO_HTTPS, "https://" }, { PROTO_FTP, "ftp://" }, { PROTO_FTPS, "ftps://" },
		{ PROTO_SFTP, "sftp://" }, { PROTO_SCP, "scp://" },     { PROTO_SMB, "smb://" }, { PROTO_INVALID, NULL },
	};
	for (int i = 0; urlPrefix[i].proto != PROTO_INVALID; ++i)
	{
		if (!strncasecmp (str, urlPrefix[i].url, strlen (urlPrefix[i].url))) return urlPrefix[i].proto;
	}
	return PROTO_INVALID;
}

static unsigned short parseURLPath (Data * data, KeySet * config)
{
	const char * path = keyString (ksLookupByName (config, "/path", 0));
	ElektraCurlProtocol proto = isValidURL (path);
	Key * key = NULL;
	if (proto == PROTO_INVALID)
	{
		data->path = path;
		data->useLocalCopy = 1;
		key = ksLookupByName (config, "/url", KDB_O_NONE);
		if (key)
		{
			proto = isValidURL (keyString (key));
			if (proto != PROTO_INVALID)
			{
				data->getUrl = keyString (key);
				data->getProto = proto;
				data->uploadUrl = data->getUrl;
				data->putProto = data->getProto;
			}
			else
			{
				key = ksLookupByName (config, "/url/get", KDB_O_NONE);
				if (!key)
				{
					return 0;
				}
				else
				{
					proto = isValidURL (keyString (key));
					if (proto == PROTO_INVALID)
					{
						return 0;
					}
					else
					{
						data->getProto = proto;
						data->getUrl = keyString (key);
					}
				}
				key = ksLookupByName (config, "/url/put", KDB_O_NONE);
				if (key)
				{
					proto = isValidURL (keyString (key));
					if (proto == PROTO_INVALID)
					{
						data->putProto = data->getProto;
						data->uploadUrl = data->getUrl;
					}
					else
					{
						data->putProto = proto;
						data->uploadUrl = keyString (key);
					}
				}
				else
				{
					data->putProto = data->getProto;
					data->uploadUrl = data->getUrl;
				}
			}
		}
	}
	else
	{
		data->useLocalCopy = 0;
		data->path = NULL;
		data->getUrl = path;
		data->getProto = proto;
		key = ksLookupByName (config, "/url/put", KDB_O_NONE);
		if (!key)
		{
			data->uploadUrl = data->getUrl;
			data->putProto = data->getProto;
		}
		else
		{
			proto = isValidURL (keyString (key));
			if (proto == PROTO_INVALID)
			{
				return 0;
			}
			else
			{
				data->uploadUrl = keyString (key);
				data->putProto = proto;
			}
		}
	}
	return 1;
}

int elektraCurlgetOpen (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	KeySet * config = elektraPluginGetConfig (handle);
	if (ksLookupByName (config, "/module", 0)) return 0;
	Data * data = elektraCalloc (sizeof (Data));
	if (!parseURLPath (data, config))
	{
		elektraFree (data);
		data = NULL;
		return 0;
	}
	Key * key = ksLookupByName (config, "/user", KDB_O_NONE);
	if (key)
	{
		data->user = keyString (key);
	}
	key = ksLookupByName (config, "/password", KDB_O_NONE);
	if (key)
	{
		data->password = keyString (key);
	}
	if (data->putProto == PROTO_HTTP || data->putProto == PROTO_HTTPS)
	{
		key = ksLookupByName (config, "/upload/method", KDB_O_NONE);
		if (key)
		{
			if (!(strcasecmp (keyString (key), "POST")))
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
			else if (!(strcasecmp (keyString (key), "PUT")))
			{
				data->uploadMethod = PUT;
			}
			else
			{
				data->uploadMethod = NA;
			}
		}
	}
	key = ksLookupByName (config, "upload/filename", KDB_O_NONE);
	if (key)
	{
		data->__uploadFileName = elektraStrDup (keyString (key));
		data->uploadFileName = data->__uploadFileName;
	}
	else
	{
		if (data->uploadMethod == POST)
		{
			if (!data->useLocalCopy)
			{
				data->__uploadFileName = elektraStrDup (data->getUrl);
				data->uploadFileName = basename (data->__uploadFileName);
			}
			else
			{
				data->__uploadFileName = elektraStrDup (data->path);
				data->uploadFileName = basename (data->__uploadFileName);
			}
		}
	}

	key = ksLookupByName (config, "/ssl/verify", KDB_O_NONE);
	if (key)
	{
		if (!strcmp (keyString (key), "1"))
		{
			data->sslVerifyPeer = 1;
			data->sslVerifyHost = 1;
			data->useSSL = 1;
		}
		key = ksLookupByName (config, "/ssl/verify/peer", KDB_O_NONE);
		if (key)
		{
			data->useSSL = 1;
			if (!strcmp (keyString (key), "1"))
				data->sslVerifyPeer = 1;
			else if (!strcmp (keyString (key), "0"))
				data->sslVerifyPeer = 0;
		}
		key = ksLookupByName (config, "/ssl/verify/host", KDB_O_NONE);
		if (key)
		{
			data->useSSL = 1;
			if (!strcmp (keyString (key), "1"))
				data->sslVerifyHost = 1;
			else if (!strcmp (keyString (key), "0"))
				data->sslVerifyHost = 0;
		}
	}
	key = ksLookupByName (config, "/prefer", KDB_O_NONE);
	data->preferRemote = 1;
	if (key && !strcasecmp (keyString (key), "local"))
	{
		data->preferRemote = 0;
	}
	key = ksLookupByName (config, "/ssh/auth", KDB_O_NONE);
	if (key)
	{
		if (!strcasecmp (keyString (key), "password"))
			data->sshAuth = SSH_PASSWORD;
		else if (!strcasecmp (keyString (key), "agent"))
			data->sshAuth = SSH_AGENT;
		else if (!strcasecmp (keyString (key), "pubkey"))
			data->sshAuth = SSH_PUBLICKEY;
		else if (!strcasecmp (keyString (key), "any"))
			data->sshAuth = SSH_ANY;
		else if (!strcasecmp (keyString (key), "pubkeypw"))
			data->sshAuth = SSH_PUBKEYPW;
	}
	key = ksLookupByName (config, "/ssh/key", KDB_O_NONE);
	if (!key)
	{
		data->keyFile = NULL;
	}
	else
	{
		data->keyFile = keyString (key);
	}
	key = ksLookupByName (config, "/ssh/key/passwd", KDB_O_NONE);
	if (key)
	{
		data->keyFilePasswd = keyString (key);
	}
	else
	{
		data->keyFilePasswd = NULL;
	}
	if (data->sshAuth == SSH_PASSWORD)
	{
		if (!data->password)
		{
			ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CURL_CONFIG_INVALID, errorKey,
					   "No password specified for SSH password authentication");
			if (data->uploadFileName) elektraFree (data->__uploadFileName);
			elektraFree (data);
			data = NULL;
			return 0;
		}
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

static void setupSSH (CURL * curl, Data * data)
{
	if (data->sshAuth == SSH_PUBLICKEY || data->sshAuth == SSH_PUBKEYPW)
	{
		if (data->sshAuth == SSH_PUBKEYPW)
			curl_easy_setopt (curl, CURLOPT_SSH_AUTH_TYPES, CURLSSH_AUTH_PUBLICKEY | CURLSSH_AUTH_PASSWORD);
		else
			curl_easy_setopt (curl, CURLOPT_SSH_AUTH_TYPES, CURLSSH_AUTH_PUBLICKEY);

		curl_easy_setopt (curl, CURLOPT_SSH_PRIVATE_KEYFILE, data->keyFile);
		if (data->keyFilePasswd)
		{
			curl_easy_setopt (curl, CURLOPT_KEYPASSWD, data->keyFilePasswd);
		}
	}
	else if (data->sshAuth == SSH_AGENT)
	{
		curl_easy_setopt (curl, CURLOPT_SSH_AUTH_TYPES, CURLSSH_AUTH_AGENT);
	}
	else if (data->sshAuth == SSH_PASSWORD)
	{
		curl_easy_setopt (curl, CURLOPT_SSH_AUTH_TYPES, CURLSSH_AUTH_PASSWORD);
	}
	else
	{
		curl_easy_setopt (curl, CURLOPT_SSH_AUTH_TYPES, CURLSSH_AUTH_ANY);
		if (data->keyFilePasswd)
		{
			curl_easy_setopt (curl, CURLOPT_KEYPASSWD, data->keyFilePasswd);
		}
	}
}

static FILE * fetchFile (Data * data, int fd)
{
	CURL * curl = curl_easy_init ();
	if (!curl)
	{
		curl_easy_cleanup (curl);
		return NULL;
	}
	curl_easy_setopt (curl, CURLOPT_VERBOSE, 1L);
	curl_easy_setopt (curl, CURLOPT_URL, data->getUrl);
	if (data->user)
	{
		curl_easy_setopt (curl, CURLOPT_USERNAME, data->user);
	}
	if (data->password)
	{
		curl_easy_setopt (curl, CURLOPT_PASSWORD, data->password);
	}
	if (data->getProto == PROTO_HTTPS || data->getProto == PROTO_FTPS || data->useSSL)
	{
		curl_easy_setopt (curl, CURLOPT_SSL_VERIFYPEER, (long)data->sslVerifyPeer);
		curl_easy_setopt (curl, CURLOPT_SSL_VERIFYHOST, (long)data->sslVerifyHost);
		curl_easy_setopt (curl, CURLOPT_USE_SSL, CURLUSESSL_ALL);
	}
	else
	{
		curl_easy_setopt (curl, CURLOPT_USE_SSL, CURLUSESSL_TRY);
	}
	if (data->getProto == PROTO_SCP || data->getProto == PROTO_SFTP)
	{
		setupSSH (curl, data);
	}
	curl_easy_setopt (curl, CURLOPT_WRITEFUNCTION, NULL);
	FILE * fp = fdopen (fd, "w+");
	curl_easy_setopt (curl, CURLOPT_WRITEDATA, fp);
	CURLcode res;
	res = curl_easy_perform (curl);
	long respCode = 0;
	curl_easy_getinfo (curl, CURLINFO_RESPONSE_CODE, &respCode);
	fprintf (stderr, "respCode: %ld\n", respCode);
	curl_easy_cleanup (curl);
	if (res != CURLE_OK || respCode != 200)
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
	char name[] = "/tmp/elektraCurlTempXXXXXX";
	fd = mkstemp (name);
	if (*(data->lastHash)) unlink (data->tmpFile);
	data->tmpFile = name;


	if (fd == -1)
	{
		ELEKTRA_SET_ERRORF (26, parentKey, "Failed to open %s for reading", data->path);
		return -1;
	}
	FILE * fp = fetchFile (data, fd);
	if (!fp)
	{
		close (fd);
		unlink (data->tmpFile);
		data->tmpFile = NULL;
		fp = fopen (data->path, "rb");
		if (fp && data->useLocalCopy)
		{
			ELEKTRA_ADD_WARNINGF (ELEKTRA_WARNING_CURL_LOCAL_FALLBACK, parentKey,
					      "Failed to fetch configuration from %s, falling back to local copy %s\n", data->getUrl,
					      data->path);
		}
		else
		{
			ELEKTRA_SET_ERROR (26, parentKey, "Failed to read configuration\n");
			return -1;
		}
	}
	fseek (fp, 0L, SEEK_END);
	size_t size = ftell (fp);
	rewind (fp);
	unsigned char buffer[size];
	fread (&buffer, sizeof (char), size, fp);
	fclose (fp);
	unsigned char * hash = hashBuffer (buffer, size);
	if (!*(data->lastHash))
	{
		memcpy (data->lastHash, hash, MD5_DIGEST_LENGTH);
		if (data->useLocalCopy)
		{
			rename (data->tmpFile, data->path);
		}
		else
		{
			data->path = data->tmpFile;
		}
		data->tmpFile = NULL;
		keySetString (parentKey, data->path);
		data->path = keyString (parentKey);
	}
	else if (data->tmpFile)
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
		if (data->tmpFile) unlink (data->tmpFile);
		data->tmpFile = name;
		FILE * fp = fetchFile (data, fd);
		if (fp)
		{
			fseek (fp, 0L, SEEK_END);
			size_t size = ftell (fp);
			rewind (fp);
			unsigned char buffer[size];
			fread (&buffer, sizeof (char), size, fp);
			fclose (fp);
			unsigned char * hash = hashBuffer (buffer, size);
			++(data->setPhase);
			if (strncmp ((char *)data->lastHash, (char *)hash, MD5_DIGEST_LENGTH))
			{
				ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CONFLICT, parentKey, "remote file has changed");
				retval = -1;
			}
			unlink (data->tmpFile);
			keySetString (parentKey, name);
		}
		else
		{
			close (fd);
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CURL_FETCH_FAILED, parentKey,
					    "Failed to fetch configuration from %s. Aborting because consistency can't be ensured",
					    data->getUrl);
			if (data->tmpFile) unlink (data->tmpFile);
			data->tmpFile = NULL;
			keySetString (parentKey, data->path);
			retval = -1;
		}
		close (fd);
		if (retval != -1)
		{
			keySetString (parentKey, name);
			data->tmpFile = keyString (parentKey);
			retval = 1;
		}
	}
	else if (data->setPhase == 1)
	{
		FILE * fp;
		const char * tmpFile = keyString (parentKey);
		fp = fopen (tmpFile, "rb");
		if (!fp)
		{
			ELEKTRA_SET_ERRORF (26, parentKey, "Failed to open %s for reading", tmpFile);
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

			if (data->user)
			{
				curl_easy_setopt (curl, CURLOPT_USERNAME, data->user);
			}
			if (data->password)
			{
				curl_easy_setopt (curl, CURLOPT_PASSWORD, data->password);
			}
			curl_easy_setopt (curl, CURLOPT_VERBOSE, 1L);
			if (data->putProto == PROTO_HTTP || data->putProto == PROTO_HTTPS)
			{
				if (data->putProto == PROTO_HTTPS || data->useSSL)
				{
					curl_easy_setopt (curl, CURLOPT_SSL_VERIFYPEER, (long)data->sslVerifyPeer);
					curl_easy_setopt (curl, CURLOPT_SSL_VERIFYHOST, (long)data->sslVerifyHost);
					curl_easy_setopt (curl, CURLOPT_USE_SSL, CURLUSESSL_ALL);
				}
				else
				{
					curl_easy_setopt (curl, CURLOPT_USE_SSL, CURLUSESSL_TRY);
				}

				curl_easy_setopt (curl, CURLOPT_URL, data->uploadUrl);

				if (data->uploadMethod == POST)
				{
					struct curl_httppost * formpost = NULL;
					struct curl_httppost * lastptr = NULL;
					struct curl_slist * headerlist = NULL;
					static const char buf[] = "Expect:";
					curl_formadd (&formpost, &lastptr, CURLFORM_COPYNAME, data->postFieldName, CURLFORM_FILE, tmpFile,
						      CURLFORM_FILENAME, data->uploadFileName, CURLFORM_END);
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
			}
			else if (data->putProto == PROTO_FTP || data->putProto == PROTO_FTPS)
			{
				if (data->putProto == PROTO_FTPS || data->useSSL)
				{
					curl_easy_setopt (curl, CURLOPT_SSL_VERIFYPEER, (long)data->sslVerifyPeer);
					curl_easy_setopt (curl, CURLOPT_SSL_VERIFYHOST, (long)data->sslVerifyHost);
					curl_easy_setopt (curl, CURLOPT_USE_SSL, CURLUSESSL_ALL);
				}
				else
				{
					curl_easy_setopt (curl, CURLOPT_USE_SSL, CURLUSESSL_TRY);
				}

				if (data->uploadFileName)
				{
					char uploadUrl[strlen (data->uploadUrl) + strlen (data->uploadFileName) + 2];
					snprintf (uploadUrl, sizeof (uploadUrl), "%s/%s", data->uploadUrl, data->uploadFileName);
					curl_easy_setopt (curl, CURLOPT_URL, uploadUrl);
				}
				else
				{
					curl_easy_setopt (curl, CURLOPT_URL, data->uploadUrl);
				}

				curl_easy_setopt (curl, CURLOPT_UPLOAD, 1L);
				curl_easy_setopt (curl, CURLOPT_READDATA, fp);
				curl_easy_setopt (curl, CURLOPT_UPLOAD, 1L);
				curl_easy_setopt (curl, CURLOPT_INFILESIZE_LARGE, (curl_off_t)size);
				res = curl_easy_perform (curl);
				if (res != CURLE_OK)
				{
					ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CURL_UPLOAD_FAILED, parentKey, "curl upload (FTP PUT) failed: %s",
							    curl_easy_strerror (res));
					retval = -1;
				}
			}
			else if (data->putProto == PROTO_SCP || data->putProto == PROTO_SFTP)
			{
				setupSSH (curl, data);
				if (data->uploadFileName)
				{
					char uploadUrl[strlen (data->uploadUrl) + strlen (data->uploadFileName) + 2];
					snprintf (uploadUrl, sizeof (uploadUrl), "%s/%s", data->uploadUrl, data->uploadFileName);
					curl_easy_setopt (curl, CURLOPT_URL, uploadUrl);
				}
				else
				{
					curl_easy_setopt (curl, CURLOPT_URL, data->uploadUrl);
				}

				curl_easy_setopt (curl, CURLOPT_UPLOAD, 1L);
				curl_easy_setopt (curl, CURLOPT_READDATA, fp);
				curl_easy_setopt (curl, CURLOPT_INFILESIZE_LARGE, (curl_off_t)size);
				res = curl_easy_perform (curl);
				if (res != CURLE_OK)
				{
					if (data->putProto == PROTO_SCP)
						ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CURL_UPLOAD_FAILED, parentKey,
								    "curl upload (SCP) failed: %s", curl_easy_strerror (res));
					else if (data->putProto == PROTO_SFTP)
						ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CURL_UPLOAD_FAILED, parentKey,
								    "curl upload (SFTP) failed: %s", curl_easy_strerror (res));
					retval = -1;
				}
			}
			else if (data->putProto == PROTO_SMB)
			{
			}
			else
			{
				curl_easy_setopt (curl, CURLOPT_URL, data->uploadUrl);
				curl_easy_setopt (curl, CURLOPT_UPLOAD, 1L);
				curl_easy_setopt (curl, CURLOPT_READDATA, fp);
				curl_easy_setopt (curl, CURLOPT_INFILESIZE_LARGE, (curl_off_t)size);
				res = curl_easy_perform (curl);
				if (res != CURLE_OK)
				{
					ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CURL_UPLOAD_FAILED, parentKey, "curl upload (default) failed: %s",
							    curl_easy_strerror (res));
					retval = -1;
				}
			}
			curl_easy_cleanup (curl);
			fclose (fp);
		}
		if (retval != -1)
		{
			if (data->useLocalCopy)
			{
				rename (data->tmpFile, data->path);
				data->tmpFile = NULL;
				keySetString (parentKey, data->path);
			}
			else
			{
				keySetString (parentKey, data->tmpFile);
			}
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

