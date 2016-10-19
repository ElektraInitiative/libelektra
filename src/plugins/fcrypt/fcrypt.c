/**
 * @file
 *
 * @brief filter plugin providing cryptographic operations
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif
#include "fcrypt.h"
#include <errno.h>
#include <fcntl.h>
#include <gpg.h>
#include <kdb.h>
#include <kdberrors.h>
#include <kdbtypes.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

enum FcryptGetState
{
	PREGETSTORAGE = 0,
	POSTGETSTORAGE = 1
};

struct _fcryptState
{
	enum FcryptGetState getState;
};
typedef struct _fcryptState fcryptState;

#define ELEKTRA_FCRYPT_TMP_FILE_SUFFIX "XXXXXX"

/**
 * @brief Allocates a new string holding the name of the temporary file.
 * @param file holds the path to the original file
 * @param fd will hold the file descriptor to the temporary file in case of success
 * @returns an allocated string holding the name of the encrypted file. Must be freed by the caller.
 */
static char * getTemporaryFileName (const char * file, int * fd)
{
	// + 1 to reserve space for the NULL terminator
	const size_t newFileAllocated = strlen (file) + strlen (ELEKTRA_FCRYPT_TMP_FILE_SUFFIX) + 1;
	char * newFile = elektraMalloc (newFileAllocated);
	if (!newFile) return NULL;
	snprintf (newFile, newFileAllocated, "%s" ELEKTRA_FCRYPT_TMP_FILE_SUFFIX, file);
	*fd = mkstemp (newFile);
	return newFile;
}

/**
 * @brief Overwrites the content of the given file with zeroes.
 * @param fd holds the file descriptor to the temporary file to be shredded
 * @param errorKey holds an error description in case of failure
 * @retval 1 on success
 * @retval -1 on failure. In this case errorKey holds an error description.
 */
static int shredTemporaryFile (int fd, Key * errorKey)
{
	kdb_octet_t buffer[512] = { 0 };
	struct stat tmpStat;

	if (fstat (fd, &tmpStat))
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_FCRYPT_TMP_FILE, errorKey, "Failed to retrieve the file status of the temporary file.");
		return -1;
	}

	if (lseek (fd, 0, SEEK_SET))
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_FCRYPT_TMP_FILE, errorKey, "Failed to overwrite the temporary file.");
		return -1;
	}

	for (off_t i = 0; i < tmpStat.st_size; i += sizeof (buffer))
	{
		write (fd, buffer, sizeof (buffer));
	}
	return 1;
}

/**
 * @brief lookup if the test mode for unit testing is enabled.
 * @param conf KeySet holding the plugin configuration.
 * @retval 0 test mode is not enabled
 * @retval 1 test mode is enabled
 */
static int inTestMode (KeySet * conf)
{
	Key * k = ksLookupByName (conf, ELEKTRA_CRYPTO_PARAM_GPG_UNIT_TEST, 0);
	if (k && !strcmp (keyString (k), "1"))
	{
		return 1;
	}
	return 0;
}

/**
 * @brief Read number of total GPG recipient keys from the plugin configuration.
 * @param config holds the plugin configuration
 * @returns the number of GPG recipient keys.
 */
static size_t getRecipientCount (KeySet * config)
{
	Key * k;
	size_t recipientCount = 0;
	Key * root = ksLookupByName (config, ELEKTRA_CRYPTO_PARAM_GPG_KEY, 0);

	if (!root) return 0;

	// toplevel
	if (strlen (keyString (root)) > 0)
	{
		recipientCount++;
	}

	ksRewind (config);
	while ((k = ksNext (config)) != 0)
	{
		if (keyIsBelow (k, root))
		{
			recipientCount++;
		}
	}
	return recipientCount;
}

static int fcryptGpgCallAndCleanup (Key * parentKey, KeySet * pluginConfig, char ** argv, int argc, int tmpFileFd, char * tmpFile)
{
	int parentKeyFd = -1;
	int result = CRYPTO_PLUGIN_FUNCTION (gpgCall) (pluginConfig, parentKey, NULL, argv, argc);

	if (result == 1)
	{
		parentKeyFd = open (keyString (parentKey), O_WRONLY);

		// encryption successful, overwrite the original file with the encrypted data
		if (rename (tmpFile, keyString (parentKey)) != 0)
		{
			ELEKTRA_SET_ERRORF (31, parentKey, "Renaming file %s to %s failed.", tmpFile, keyString (parentKey));
			result = -1;
		}
	}

	if (result == 1)
	{
		if (parentKeyFd >= 0)
		{
			shredTemporaryFile (parentKeyFd, parentKey);
		}
	}
	else
	{
		// if anything went wrong above the temporary file is shredded and removed
		shredTemporaryFile (tmpFileFd, parentKey);
		unlink (tmpFile);
	}

	if (parentKeyFd >= 0)
	{
		close (parentKeyFd);
	}
	close (tmpFileFd);
	elektraFree (tmpFile);
	return result;
}

/**
 * @brief encrypt the file specified at parentKey
 * @param pluginConfig holds the plugin configuration
 * @pararm parentKey holds the path to the file to be encrypted. Will hold an error description in case of failure.
 * @retval 1 on success
 * @retval -1 on error, errorKey holds an error description
 */
static int encrypt (KeySet * pluginConfig, Key * parentKey)
{
	const size_t recipientCount = getRecipientCount (pluginConfig);
	if (recipientCount == 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, parentKey,
				    "Missing GPG key (specified as %s) in plugin configuration.", ELEKTRA_CRYPTO_PARAM_GPG_KEY);
		return -1;
	}

	const size_t testMode = inTestMode (pluginConfig);
	int tmpFileFd = -1;

	char * tmpFile = getTemporaryFileName (keyString (parentKey), &tmpFileFd);
	if (!tmpFile)
	{
		ELEKTRA_SET_ERROR (87, parentKey, "Memory allocation failed");
		return -1;
	}

	// prepare argument vector for gpg call
	int argc = 8 + (2 * recipientCount) + (2 * testMode);
	kdb_unsigned_short_t i = 0;
	char * argv[argc];
	argv[i++] = NULL;
	argv[i++] = "--batch";
	argv[i++] = "-o";
	argv[i++] = tmpFile;
	argv[i++] = "--yes"; // overwrite files if they exist

	// add recipients
	Key * root = ksLookupByName (pluginConfig, ELEKTRA_CRYPTO_PARAM_GPG_KEY, 0);

	// append root (gpg/key) as gpg recipient
	if (root && strlen (keyString (root)) > 0)
	{
		argv[i++] = "-r";
		// NOTE argv[] values will not be modified, so const can be discarded safely
		argv[i++] = (char *)keyString (root);
	}

	// append keys beneath root (crypto/key/#_) as gpg recipients
	Key * k;
	ksRewind (pluginConfig);
	while ((k = ksNext (pluginConfig)) != 0)
	{
		if (keyIsBelow (k, root))
		{
			argv[i++] = "-r";
			// NOTE argv[] values will not be modified, so const can be discarded safely
			argv[i++] = (char *)keyString (k);
		}
	}

	// if we are in test mode we add the trust model
	if (testMode > 0)
	{
		argv[i++] = "--trust-model";
		argv[i++] = "always";
	}

	// prepare rest of the argument vector
	argv[i++] = "-e";
	argv[i++] = (char *)keyString (parentKey);
	argv[i++] = NULL;

	// NOTE the encryption process works like this:
	// gpg2 --batch --yes -o encryptedFile -r keyID -e configFile
	// mv encryptedFile configFile

	return fcryptGpgCallAndCleanup (parentKey, pluginConfig, argv, argc, tmpFileFd, tmpFile);
}

/**
 * @brief decrypt the file specified at parentKey
 * @param pluginConfig holds the plugin configuration
 * @pararm parentKey holds the path to the file to be encrypted. Will hold an error description in case of failure.
 * @retval 1 on success
 * @retval -1 on error, errorKey holds an error description
 */
static int decrypt (KeySet * pluginConfig, Key * parentKey)
{
	int tmpFileFd = -1;
	char * tmpFile = getTemporaryFileName (keyString (parentKey), &tmpFileFd);
	if (!tmpFile)
	{
		ELEKTRA_SET_ERROR (87, parentKey, "Memory allocation failed");
		return -1;
	}

	const size_t testMode = inTestMode (pluginConfig);
	int argc = 8 + (2 * testMode);
	char * argv[argc];
	int i = 0;

	argv[i++] = NULL;
	argv[i++] = "--batch";
	argv[i++] = "--yes";

	// if we are in test mode we add the trust model
	if (inTestMode (pluginConfig))
	{
		argv[i++] = "--trust-model";
		argv[i++] = "always";
	}

	argv[i++] = "-o";
	argv[i++] = tmpFile;
	argv[i++] = "-d";
	// safely discarding const from keyString() return value
	argv[i++] = (char *)keyString (parentKey);
	argv[i++] = NULL;


	// NOTE the decryption process works like this:
	// gpg2 --batch --yes -o tmpfile -d configFile
	// mv tmpfile configFile

	return fcryptGpgCallAndCleanup (parentKey, pluginConfig, argv, argc, tmpFileFd, tmpFile);
}

/**
 * @brief allocates plugin state handle and initializes the plugin state
 * @retval 1 on success
 * @retval -1 on failure
 */
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, open) (Plugin * handle, KeySet * ks ELEKTRA_UNUSED, Key * parentKey)
{
	fcryptState * s = elektraMalloc (sizeof (fcryptState));
	if (!s)
	{
		ELEKTRA_SET_ERROR (87, parentKey, "Memory allocation failed");
		return -1;
	}

	s->getState = PREGETSTORAGE;

	elektraPluginSetData (handle, s);
	return 1;
}

/**
 * @brief frees the plugin state handle
 * @retval 1 on success
 * @retval -1 on failure
 */
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, close) (Plugin * handle, KeySet * ks ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	fcryptState * s = (fcryptState *)elektraPluginGetData (handle);
	if (s)
	{
		elektraFree (s);
		elektraPluginSetData (handle, NULL);
	}
	return 1;
}

/**
 * @brief establish the Elektra plugin contract and decrypt the file provided at parentKey using GPG.
 * @retval 1 on success
 * @retval -1 on failure
 */
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, get) (Plugin * handle, KeySet * ks ELEKTRA_UNUSED, Key * parentKey)
{
	// Publish module configuration to Elektra (establish the contract)
	if (!strcmp (keyName (parentKey), "system/elektra/modules/" ELEKTRA_PLUGIN_NAME))
	{
		KeySet * moduleConfig = ksNew (30,
#include "contract.h"
					       KS_END);
		ksAppend (ks, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	// check plugin state
	KeySet * pluginConfig = elektraPluginGetConfig (handle);
	fcryptState * s = (fcryptState *)elektraPluginGetData (handle);

	if (s && s->getState == POSTGETSTORAGE)
	{
		// encrypt if this is a postgetstorage call
		return encrypt (pluginConfig, parentKey);
	}

	// now this is a pregetstorage call
	// next time treat the kdb get call as postgetstorage call to trigger encryption after the file has been read
	s->getState = POSTGETSTORAGE;
	return decrypt (pluginConfig, parentKey);
}

/**
 * @brief Encrypt the file provided at parentKey using GPG.
 * @retval 1 on success
 * @retval -1 on failure
 */
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, set) (Plugin * handle, KeySet * ks ELEKTRA_UNUSED, Key * parentKey)
{
	KeySet * pluginConfig = elektraPluginGetConfig (handle);
	int encryptionResult = encrypt (pluginConfig, parentKey);
	if (encryptionResult != 1) return encryptionResult;

	/* set all keys */
	const char * configFile = keyString (parentKey);
	if (!strcmp (configFile, "")) return 0; // no underlying config file
	int fd = open (configFile, O_RDWR);
	if (fd == -1)
	{
		ELEKTRA_SET_ERRORF (89, parentKey, "Could not open config file %s because %s", configFile, strerror (errno));
		return -1;
	}
	if (fsync (fd) == -1)
	{
		ELEKTRA_SET_ERRORF (89, parentKey, "Could not fsync config file %s because %s", configFile, strerror (errno));
		close (fd);
		return -1;
	}
	close (fd);
	return 1;
}

/**
 * @brief Checks for the existence of the master password, that is used for encryption and decryption.
 *
 * If the master password can not be found it will be generated randomly.
 * Then it will be encrypted and stored in conf.
 *
 * If the master password can be found, it will be decrypted temporarily in order to verify its correctness.
 * conf will not be modified in this case.
 *
 * An error might occur during the password generation, encryption and decryption.
 * The error will be appended to errorKey.
 *
 * @retval 0 no changes were made to the configuration
 * @retval 1 the master password has been appended to the configuration
 * @retval -1 an error occurred. Check errorKey
 */
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, checkconf) (Key * errorKey, KeySet * conf)
{
	if (getRecipientCount (conf) == 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey,
				    "Missing GPG key (specified as %s) in plugin configuration.", ELEKTRA_CRYPTO_PARAM_GPG_KEY);
		return -1;
	}
	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (fcrypt)
{
	// clang-format off
	return elektraPluginExport(ELEKTRA_PLUGIN_NAME,
			ELEKTRA_PLUGIN_OPEN,  &ELEKTRA_PLUGIN_FUNCTION(ELEKTRA_PLUGIN_NAME, open),
			ELEKTRA_PLUGIN_CLOSE, &ELEKTRA_PLUGIN_FUNCTION(ELEKTRA_PLUGIN_NAME, close),
			ELEKTRA_PLUGIN_GET,   &ELEKTRA_PLUGIN_FUNCTION(ELEKTRA_PLUGIN_NAME, get),
			ELEKTRA_PLUGIN_SET,   &ELEKTRA_PLUGIN_FUNCTION(ELEKTRA_PLUGIN_NAME, set),
			ELEKTRA_PLUGIN_END);
}
