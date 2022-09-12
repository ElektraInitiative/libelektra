/**
 * @file
 *
 * @brief Source for curlget plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "curlget.h"

#include <curl/curl.h>
#include <curl/easy.h>
#include <errno.h>
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

#include "../resolver/shared.h"
#include <kdbinvoke.h>

#define TMP_NAME "/tmp/elektraCurlTempXXXXXX"

#define DEFAULT_POSTFIELDNAME "file"

typedef enum
{
	NA = 0,
	PUT,
	POST
} HttpUploadMethods;
typedef enum
{
	PROTO_INVALID = 0,
	PROTO_HTTP,
	PROTO_HTTPS,
	PROTO_FTP,
	PROTO_FTPS,
	PROTO_SFTP,
	PROTO_SCP,
	PROTO_SMB,
} ElektraCurlProtocol;
typedef enum
{
	SSH_ANY = 0,
	SSH_AGENT,
	SSH_PASSWORD,
	SSH_PUBLICKEY,
	SSH_PUBKEYPW,
} SSHAuthType;
typedef struct
{
	char * path;
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

static int elektraResolveFilename (ElektraKey * parentKey, ElektraResolveTempfile tmpFile)
{
	int rc = 0;
	void * handle = elektraInvokeOpen ("resolver", 0, 0);
	if (!handle)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}
	ElektraResolved * resolved = NULL;
	typedef ElektraResolved * (*resolveFileFunc) (elektraNamespace, const char *, ElektraResolveTempfile, ElektraKey *);
	resolveFileFunc resolveFunc = *(resolveFileFunc *) elektraInvokeGetFunction (handle, "filename");

	if (!resolveFunc)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}

	typedef void (*freeHandleFunc) (ElektraResolved *);
	freeHandleFunc freeHandle = *(freeHandleFunc *) elektraInvokeGetFunction (handle, "freeHandle");

	if (!freeHandle)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}

	resolved = resolveFunc (elektraKeyGetNamespace (parentKey), elektraKeyString (parentKey), tmpFile, parentKey);

	if (!resolved)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}
	else
	{
		elektraKeySetString (parentKey, resolved->fullPath);
		freeHandle (resolved);
	}

RESOLVE_FAILED:
	elektraInvokeClose (handle, 0);
	return rc;
}

int elektraCurlgetCheckFile (const char * filename)
{
	if (filename[0] == '/') return 0;

	return 1;
}

int elektraCurlgetClose (Plugin * handle ELEKTRA_UNUSED, ElektraKey * errorKey ELEKTRA_UNUSED)
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
		elektraFree (data->path);
		data->path = NULL;
	}
	else if (data->path)
	{
		elektraFree (data->path);
		data->path = NULL;
	}
	if (data->uploadFileName) elektraFree (data->__uploadFileName);
	elektraFree (data);
	data = NULL;
	elektraPluginSetData (handle, data);
	return 1;
}

int elektraCurlgetError (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
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
		{ PROTO_SFTP, "sftp://" }, { PROTO_SCP, "scp://" },	{ PROTO_SMB, "smb://" }, { PROTO_INVALID, NULL },
	};
	for (int i = 0; urlPrefix[i].proto != PROTO_INVALID; ++i)
	{
		if (!strncasecmp (str, urlPrefix[i].url, strlen (urlPrefix[i].url))) return urlPrefix[i].proto;
	}
	return PROTO_INVALID;
}

static unsigned short parseURLPath (Data * data, ElektraKeyset * config)
{
	const char * path = elektraKeyString (elektraKeysetLookupByName (config, "/path", 0));
	ElektraCurlProtocol proto = isValidURL (path);
	ElektraKey * key = NULL;
	if (proto == PROTO_INVALID)
	{
		data->path = elektraStrDup (path);
		data->useLocalCopy = 1;
		key = elektraKeysetLookupByName (config, "/url", ELEKTRA_KDB_O_NONE);
		if (key)
		{
			proto = isValidURL (elektraKeyString (key));
			if (proto != PROTO_INVALID)
			{
				data->getUrl = elektraKeyString (key);
				data->getProto = proto;
				data->uploadUrl = data->getUrl;
				data->putProto = data->getProto;
			}
			else
			{
				key = elektraKeysetLookupByName (config, "/url/get", ELEKTRA_KDB_O_NONE);
				if (!key)
				{
					return 0;
				}
				else
				{
					proto = isValidURL (elektraKeyString (key));
					if (proto == PROTO_INVALID)
					{
						return 0;
					}
					else
					{
						data->getProto = proto;
						data->getUrl = elektraKeyString (key);
					}
				}
				key = elektraKeysetLookupByName (config, "/url/put", ELEKTRA_KDB_O_NONE);
				if (key)
				{
					proto = isValidURL (elektraKeyString (key));
					if (proto == PROTO_INVALID)
					{
						data->putProto = data->getProto;
						data->uploadUrl = data->getUrl;
					}
					else
					{
						data->putProto = proto;
						data->uploadUrl = elektraKeyString (key);
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
		if (data->path) elektraFree (data->path);
		data->path = NULL;
		data->getUrl = path;
		data->getProto = proto;
		key = elektraKeysetLookupByName (config, "/url/put", ELEKTRA_KDB_O_NONE);
		if (!key)
		{
			data->uploadUrl = data->getUrl;
			data->putProto = data->getProto;
		}
		else
		{
			proto = isValidURL (elektraKeyString (key));
			if (proto == PROTO_INVALID)
			{
				return 0;
			}
			else
			{
				data->uploadUrl = elektraKeyString (key);
				data->putProto = proto;
			}
		}
	}
	return 1;
}

int elektraCurlgetOpen (Plugin * handle, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	ElektraKeyset * config = elektraPluginGetConfig (handle);
	if (elektraKeysetLookupByName (config, "/module", 0)) return 0;
	Data * data = elektraCalloc (sizeof (Data));
	if (!parseURLPath (data, config))
	{
		elektraFree (data);
		data = NULL;
		return 0;
	}
	ElektraKey * key = elektraKeysetLookupByName (config, "/user", ELEKTRA_KDB_O_NONE);
	if (key)
	{
		data->user = elektraKeyString (key);
	}
	key = elektraKeysetLookupByName (config, "/password", ELEKTRA_KDB_O_NONE);
	if (key)
	{
		data->password = elektraKeyString (key);
	}
	if (data->putProto == PROTO_HTTP || data->putProto == PROTO_HTTPS)
	{
		key = elektraKeysetLookupByName (config, "/upload/method", ELEKTRA_KDB_O_NONE);
		if (key)
		{
			if (!(strcasecmp (elektraKeyString (key), "POST")))
			{
				data->uploadMethod = POST;
				key = elektraKeysetLookupByName (config, "/upload/postfield", ELEKTRA_KDB_O_NONE);
				if (key)
				{
					data->postFieldName = elektraKeyString (key);
				}
				else
				{
					data->postFieldName = DEFAULT_POSTFIELDNAME;
				}
			}
			else if (!(strcasecmp (elektraKeyString (key), "PUT")))
			{
				data->uploadMethod = PUT;
			}
			else
			{
				data->uploadMethod = NA;
			}
		}
	}
	key = elektraKeysetLookupByName (config, "upload/filename", ELEKTRA_KDB_O_NONE);
	if (key)
	{
		data->__uploadFileName = elektraStrDup (elektraKeyString (key));
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

	key = elektraKeysetLookupByName (config, "/ssl/verify", ELEKTRA_KDB_O_NONE);
	if (key)
	{
		if (!strcmp (elektraKeyString (key), "1"))
		{
			data->sslVerifyPeer = 1;
			data->sslVerifyHost = 1;
			data->useSSL = 1;
		}
		key = elektraKeysetLookupByName (config, "/ssl/verify/peer", ELEKTRA_KDB_O_NONE);
		if (key)
		{
			data->useSSL = 1;
			if (!strcmp (elektraKeyString (key), "1"))
				data->sslVerifyPeer = 1;
			else if (!strcmp (elektraKeyString (key), "0"))
				data->sslVerifyPeer = 0;
		}
		key = elektraKeysetLookupByName (config, "/ssl/verify/host", ELEKTRA_KDB_O_NONE);
		if (key)
		{
			data->useSSL = 1;
			if (!strcmp (elektraKeyString (key), "1"))
				data->sslVerifyHost = 1;
			else if (!strcmp (elektraKeyString (key), "0"))
				data->sslVerifyHost = 0;
		}
	}
	key = elektraKeysetLookupByName (config, "/prefer", ELEKTRA_KDB_O_NONE);
	data->preferRemote = 1;
	if (key && !strcasecmp (elektraKeyString (key), "local"))
	{
		data->preferRemote = 0;
	}
	key = elektraKeysetLookupByName (config, "/ssh/auth", ELEKTRA_KDB_O_NONE);
	if (key)
	{
		if (!strcasecmp (elektraKeyString (key), "password"))
			data->sshAuth = SSH_PASSWORD;
		else if (!strcasecmp (elektraKeyString (key), "agent"))
			data->sshAuth = SSH_AGENT;
		else if (!strcasecmp (elektraKeyString (key), "pubkey"))
			data->sshAuth = SSH_PUBLICKEY;
		else if (!strcasecmp (elektraKeyString (key), "any"))
			data->sshAuth = SSH_ANY;
		else if (!strcasecmp (elektraKeyString (key), "pubkeypw"))
			data->sshAuth = SSH_PUBKEYPW;
	}
	key = elektraKeysetLookupByName (config, "/ssh/key", ELEKTRA_KDB_O_NONE);
	if (!key)
	{
		data->keyFile = NULL;
	}
	else
	{
		data->keyFile = elektraKeyString (key);
	}
	key = elektraKeysetLookupByName (config, "/ssh/key/passwd", ELEKTRA_KDB_O_NONE);
	if (key)
	{
		data->keyFilePasswd = elektraKeyString (key);
	}
	else
	{
		data->keyFilePasswd = NULL;
	}
	if (data->sshAuth == SSH_PASSWORD)
	{
		if (!data->password)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (
				errorKey, "No password specified for SSH password authentication in plugin configuration");
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
#if VERBOSE
	curl_easy_setopt (curl, CURLOPT_VERBOSE, 1L);
#endif
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
		curl_easy_setopt (curl, CURLOPT_SSL_VERIFYPEER, (long) data->sslVerifyPeer);
		curl_easy_setopt (curl, CURLOPT_SSL_VERIFYHOST, (long) data->sslVerifyHost);
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
	curl_easy_cleanup (curl);
	if (res != CURLE_OK || respCode != 200)
	{
		fclose (fp);
		return NULL;
	}
	return fp;
}

static int moveFile (const char * source, const char * dest)
{
	FILE * inFile = NULL;
	FILE * outFile = NULL;
	struct stat buf;
	if (stat (source, &buf) == -1) return -1;
	size_t fileSize = buf.st_size;
	char * buffer = elektraMalloc (fileSize);
	inFile = fopen (source, "rb");
	size_t bytesRead = 0;
	while (bytesRead < fileSize)
	{
		size_t bytes = fread (buffer + bytesRead, 1, (size_t) fileSize, inFile);
		if (bytes == 0) break;
		bytesRead += bytes;
	}
	if (bytesRead < fileSize)
	{
		elektraFree (buffer);
		fclose (inFile);
		return -1;
	}
	fclose (inFile);
	outFile = fopen (dest, "wb+");

	size_t bytesWritten = 0;
	while (bytesWritten < fileSize)
	{
		size_t bytes = fwrite (buffer, 1, fileSize, outFile);
		if (bytes == 0) break;
		bytesWritten += bytes;
	}
	fclose (outFile);
	elektraFree (buffer);

	if (bytesWritten < fileSize)
	{
		return -1;
	}
	if (unlink (source))
	{
		return -1;
	}
	return 0;
}

int elektraCurlgetGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey)
{
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/curlget"))
	{
		ElektraKeyset * contract =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/curlget", ELEKTRA_KEY_VALUE, "curlget plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/curlget/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/curlget/exports/get", ELEKTRA_KEY_FUNC, elektraCurlgetGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/curlget/exports/set", ELEKTRA_KEY_FUNC, elektraCurlgetSet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/curlget/exports/commit", ELEKTRA_KEY_FUNC, elektraCurlgetCommit, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/curlget/exports/open", ELEKTRA_KEY_FUNC, elektraCurlgetOpen, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/curlget/exports/close", ELEKTRA_KEY_FUNC, elektraCurlgetClose, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/curlget/exports/error", ELEKTRA_KEY_FUNC, elektraCurlgetError, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/curlget/exports/checkfile", ELEKTRA_KEY_FUNC, elektraCurlgetCheckFile, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       elektraKeyNew ("system:/elektra/modules/curlget/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return 1; // success
	}

	Data * data = elektraPluginGetData (handle);
	if (!data)
	{
		return -1;
	}
	int fd = 0;
	char name[] = TMP_NAME;
	fd = mkstemp (name);
	if (*(data->lastHash)) unlink (data->tmpFile);
	data->tmpFile = name;

	if (data->path) elektraKeySetString (parentKey, data->path);
	if (elektraResolveFilename (parentKey, ELEKTRA_RESOLVER_TEMPFILE_NONE) == -1)
	{
		return -1;
	}
	if (data->path) elektraFree (data->path);
	data->path = elektraStrDup (elektraKeyString (parentKey));

	if (fd == -1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Failed to open %s for reading. Reason: %s", data->path, strerror (errno));
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
			ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Failed to fetch configuration from %s, falling back to local copy %s\n",
						       data->getUrl, data->path);
		}
		else
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Failed to read configuration. Reason: %s\n", strerror (errno));
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
			moveFile (data->tmpFile, data->path);
		}
		else
		{
			if (data->path) elektraFree (data->path);
			data->path = elektraStrDup (data->tmpFile);
		}
		data->tmpFile = NULL;
		elektraKeySetString (parentKey, data->path);
	}
	else if (data->tmpFile)
	{
		if (strncmp ((char *) data->lastHash, (char *) hash, MD5_DIGEST_LENGTH))
		{
			// remote file has changed
			// if preferRemote is set: replace local copy with
			// the remote version.
			if (data->preferRemote)
			{
				moveFile (data->tmpFile, data->path);
				data->tmpFile = NULL;
				elektraKeySetString (parentKey, data->path);
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
			if (data->tmpFile) unlink (data->tmpFile);
			data->tmpFile = NULL;
			elektraKeySetString (parentKey, data->path);
		}
	}
	elektraFree (hash);
	return 1; // success
}

int elektraCurlgetSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	// get all keys
	Data * data = elektraPluginGetData (handle);
	if (!data) return -1;
	int retval = 1;
	if (data->setPhase == 0)
	{
		char name[] = TMP_NAME;
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
			if (strncmp ((char *) data->lastHash, (char *) hash, MD5_DIGEST_LENGTH))
			{
				ELEKTRA_SET_CONFLICTING_STATE_ERROR (parentKey, "Remote file has changed");
				retval = -1;
			}
			elektraFree (hash);
			if (data->tmpFile) unlink (data->tmpFile);
			data->tmpFile = NULL;
			elektraKeySetString (parentKey, name);
		}
		else
		{
			close (fd);
			ELEKTRA_SET_CONFLICTING_STATE_ERRORF (
				parentKey, "Failed to fetch configuration from %s. Aborting because consistency can't be ensured",
				data->getUrl);
			if (data->tmpFile) unlink (data->tmpFile);
			data->tmpFile = NULL;
			if (data->useLocalCopy) elektraKeySetString (parentKey, data->path);

			retval = -1;
		}
		close (fd);
		if (retval != -1)
		{
			elektraKeySetString (parentKey, name);
			data->tmpFile = name;
			retval = 1;
		}
		if (!data->useLocalCopy && data->path)
		{
			unlink (data->path);
			if (data->path) elektraFree (data->path);
			data->path = NULL;
		}
	}
	else if (data->setPhase == 1)
	{
		FILE * fp;
		const char * tmpFile = elektraKeyString (parentKey);
		fp = fopen (tmpFile, "rb");
		if (!fp)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Failed to open %s for reading. Reason: %s", tmpFile, strerror (errno));
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
#if VERBOSE
			curl_easy_setopt (curl, CURLOPT_VERBOSE, 1L);
#endif
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
					curl_easy_setopt (curl, CURLOPT_SSL_VERIFYPEER, (long) data->sslVerifyPeer);
					curl_easy_setopt (curl, CURLOPT_SSL_VERIFYHOST, (long) data->sslVerifyHost);
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

						ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Curl upload (HTTP POST) failed: %s\n",
									     curl_easy_strerror (res));
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
					curl_easy_setopt (curl, CURLOPT_INFILESIZE_LARGE, (curl_off_t) size);
					res = curl_easy_perform (curl);
					if (res != CURLE_OK)
					{
						ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Curl upload (HTTP PUT) failed. Reason: %s",
									     curl_easy_strerror (res));
						retval = -1;
					}
				}
				else
				{
					curl_easy_setopt (curl, CURLOPT_UPLOAD, 1L);
					curl_easy_setopt (curl, CURLOPT_READDATA, fp);
					curl_easy_setopt (curl, CURLOPT_INFILESIZE_LARGE, (curl_off_t) size);
					res = curl_easy_perform (curl);
					if (res != CURLE_OK)
					{
						ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Curl upload (HTTP PUT) failed. Reason: %s",
									     curl_easy_strerror (res));
						retval = -1;
					}
				}
			}
			else if (data->putProto == PROTO_FTP || data->putProto == PROTO_FTPS)
			{
				if (data->putProto == PROTO_FTPS || data->useSSL)
				{
					curl_easy_setopt (curl, CURLOPT_SSL_VERIFYPEER, (long) data->sslVerifyPeer);
					curl_easy_setopt (curl, CURLOPT_SSL_VERIFYHOST, (long) data->sslVerifyHost);
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
				curl_easy_setopt (curl, CURLOPT_INFILESIZE_LARGE, (curl_off_t) size);
				res = curl_easy_perform (curl);
				if (res != CURLE_OK)
				{
					ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Curl upload (FTP PUT) failed. Reason: %s",
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
				curl_easy_setopt (curl, CURLOPT_INFILESIZE_LARGE, (curl_off_t) size);
				res = curl_easy_perform (curl);
				if (res != CURLE_OK)
				{
					if (data->putProto == PROTO_SCP)
						ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Curl upload (SCP) failed. Reason: %s",
									     curl_easy_strerror (res));
					else if (data->putProto == PROTO_SFTP)
						ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Curl upload (SFTP) failed. Reason: %s",
									     curl_easy_strerror (res));
					retval = -1;
				}
			}
			else if (data->putProto == PROTO_SMB) // lgtm [cpp/empty-block]
			{
			}
			else
			{
				curl_easy_setopt (curl, CURLOPT_URL, data->uploadUrl);
				curl_easy_setopt (curl, CURLOPT_UPLOAD, 1L);
				curl_easy_setopt (curl, CURLOPT_READDATA, fp);
				curl_easy_setopt (curl, CURLOPT_INFILESIZE_LARGE, (curl_off_t) size);
				res = curl_easy_perform (curl);
				if (res != CURLE_OK)
				{
					ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Curl upload (default) . Reason: %s",
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
				if (moveFile (tmpFile, data->path))
				{
					if (data->tmpFile)
					{
						unlink (data->tmpFile);
					}
				}
				data->tmpFile = NULL;
				elektraKeySetString (parentKey, data->path);
			}
			else
			{
				if (data->tmpFile)
				{
					unlink (data->tmpFile);
					data->tmpFile = NULL;
				}
				if (data->path)
				{
					unlink (data->path);
					elektraFree (data->path);
					data->path = NULL;
				}
				if (tmpFile)
				{
					unlink (tmpFile);
				}
			}
		}
		else
		{
			if (!data->useLocalCopy)
			{
				if (data->tmpFile)
				{
					unlink (data->tmpFile);
					data->tmpFile = NULL;
				}
				if (data->path)
				{
					unlink (data->path);
					elektraFree (data->path);
					data->path = NULL;
				}
				if (tmpFile)
				{
					unlink (tmpFile);
				}
			}
			else
			{
				if (tmpFile) unlink (tmpFile);
				if (data->tmpFile)
				{
					unlink (data->tmpFile);
					data->tmpFile = NULL;
				}
			}
		}
	}

	return retval; // success
}

int elektraCurlgetCommit (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	return elektraCurlgetSet (handle, returned, parentKey);
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
    return elektraPluginExport ("curlget",
	    ELEKTRA_PLUGIN_GET,	&elektraCurlgetGet,
	    ELEKTRA_PLUGIN_SET,	&elektraCurlgetSet,
	    ELEKTRA_PLUGIN_OPEN,	&elektraCurlgetOpen,
	    ELEKTRA_PLUGIN_CLOSE,	&elektraCurlgetClose,
	    ELEKTRA_PLUGIN_ERROR,	&elektraCurlgetError,
	    ELEKTRA_PLUGIN_COMMIT,	&elektraCurlgetCommit,
	    ELEKTRA_PLUGIN_END);
}

