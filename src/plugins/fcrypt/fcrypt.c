/**
 * @file
 *
 * @brief filter plugin providing cryptographic operations
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
#include <internal/kdb/config.h>
#endif

#include "./fcrypt.h"


#include <errno.h>
#include <fcntl.h>
#include <gpg.h>
#include <libgen.h> // provides basename()
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/kdb/errors.h>
#include <elektra/type/types.h>
#include <internal/macros/attributes.h>
/**
 * @brief Defines the plugin state during the <code>kdb get</code> phase.
 */
enum FcryptGetState
{

	/** Perform a decryption run before <code>kdb get</code> reads from the storage. */
	PREGETSTORAGE = 0,

	/** Perform an encryption run after <code>kdb get</code> has read from the storage. */
	POSTGETSTORAGE = 1
};

struct _fcryptState
{
	enum FcryptGetState getState;
	int tmpFileFd;
	char * tmpFilePath;
	char * originalFilePath;
};
typedef struct _fcryptState fcryptState;

#define ELEKTRA_FCRYPT_TMP_FILE_SUFFIX "XXXXXX"

/**
 * @brief Allocates a new string holding the name of the temporary file.
 * This method makes use of the "fcrypt/tmpdir" plugin configuration option.
 * @param conf holds the plugin configuration
 * @param file holds the path to the original file
 * @param fd will hold the file descriptor to the temporary file in case of success
 * @returns an allocated string holding the name of the encrypted file. Must be freed by the caller.
 */
static char * getTemporaryFileName (KeySet * conf, const char * file, int * fd)
{
	// read the temporary directory to use from the plugin configuration
	// NOTE the string contained in tmpDir must not be modified!
	const char * tmpDir = NULL;
	Key * k = ksLookupByName (conf, ELEKTRA_FCRYPT_CONFIG_TMPDIR, 0);
	if (k)
	{
		tmpDir = keyString (k);
	}

	if (!tmpDir)
	{
		// check the environment; returns NULL if no match is found
		tmpDir = getenv ("TMPDIR");
	}

	if (!tmpDir)
	{
		// fallback
		tmpDir = ELEKTRA_FCRYPT_DEFAULT_TMPDIR;
	}

	// extract the file name (base name) from the path
	char * fileDup = elektraStrDup (file);
	if (!fileDup) goto error;
	const char * baseName = basename (fileDup);

	// + 1 to add an additional '/' as path separator
	// + 1 to reserve space for the NULL terminator
	// ----------------------------------------------
	// + 2 characters in total
	const size_t newFileAllocated = strlen (tmpDir) + strlen (baseName) + strlen (ELEKTRA_FCRYPT_TMP_FILE_SUFFIX) + 2;
	char * newFile = elektraMalloc (newFileAllocated);
	if (!newFile) goto error;
	snprintf (newFile, newFileAllocated, "%s/%s" ELEKTRA_FCRYPT_TMP_FILE_SUFFIX, tmpDir, baseName);
	*fd = mkstemp (newFile);
	if (*fd < 0)
	{
		elektraFree (newFile);
		goto error;
	}

	elektraFree (fileDup);
	return newFile;

error:
	elektraFree (fileDup);
	return NULL;
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
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey,
					     "Failed to overwrite the temporary data. Cannot retrieve file status. WARNING: Unencrypted "
					     "data may leak. Errno: %s",
					     strerror (errno));
		return -1;
	}

	if (lseek (fd, 0, SEEK_SET))
	{
		goto error;
	}

	for (off_t i = 0; i < tmpStat.st_size; i += sizeof (buffer))
	{
		if (write (fd, buffer, sizeof (buffer)) != sizeof (buffer))
		{
			goto error;
		}
	}
	return 1;

error:
	ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Failed to overwrite the temporary data. WARNING: Unencrypted data may leak! Reason: %s",
				     strerror (errno));
	return -1;
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
 * @brief lookup if the text mode is disabled in the plugin config.
 * It is enabled per default.
 * @param conf KeySet holding the plugin configuration.
 * @retval 0 text mode is not enabled
 * @retval 1 text mode is enabled
 */
static int inTextMode (KeySet * conf)
{
	Key * k = ksLookupByName (conf, ELEKTRA_FCRYPT_CONFIG_TEXTMODE, 0);
	if (k && !strcmp (keyString (k), "0"))
	{
		return 0;
	}
	return 1;
}

/**
 * @brief Read number of total GPG recipient keys from the plugin configuration.
 * @param config holds the plugin configuration
 * @param keyName holds the name of the root key to look up
 * @returns the number of GPG recipient keys.
 */
static size_t getRecipientCount (KeySet * config, const char * keyName)
{
	size_t recipientCount = 0;
	Key * root = ksLookupByName (config, keyName, 0);

	if (!root) return 0;

	// toplevel
	if (strlen (keyString (root)) > 0)
	{
		recipientCount++;
	}

	for (elektraCursor it = 0; it < ksGetSize (config); ++it)
	{
		Key * k = ksAtCursor (config, it);
		if (keyIsBelow (k, root) && strlen (keyString (k)) > 0)
		{
			recipientCount++;
		}
	}
	return recipientCount;
}

static int fcryptGpgCallAndCleanup (Key * parentKey, KeySet * pluginConfig, char ** argv, int argc, int tmpFileFd, char * tmpFile)
{
	ssize_t readCount;
	ssize_t writeCount;
	kdb_octet_t buffer[512];
	int parentKeyFd = -1;
	int result = ELEKTRA_PLUGIN_FUNCTION (gpgCall) (pluginConfig, parentKey, NULL, argv, argc);
	int manualCopy = 0;
	int transferErrno = 0;

	if (result == 1)
	{
		parentKeyFd = open (keyString (parentKey), O_WRONLY);

		// gpg call returned success, overwrite the original file with the gpg payload data
		if (rename (tmpFile, keyString (parentKey)) != 0)
		{
			// if rename failed we can still try to copy the file content manually
			if (lseek (tmpFileFd, 0, SEEK_SET))
			{
				transferErrno = errno;
				result = -1;
			}
			if (lseek (parentKeyFd, 0, SEEK_SET))
			{
				transferErrno = errno;
				result = -1;
			}
			readCount = read (tmpFileFd, buffer, sizeof (buffer));
			if (readCount < 0)
			{
				transferErrno = errno;
				result = -1;
			}
			while (result == 1 && readCount > 0)
			{
				writeCount = write (parentKeyFd, buffer, readCount);
				if (writeCount < 0)
				{
					transferErrno = errno;
					result = -1;
					break;
				}
				readCount = read (tmpFileFd, buffer, sizeof (buffer));
				if (readCount < 0)
				{
					transferErrno = errno;
					result = -1;
				}
			}
			manualCopy = 1;
		}
	}

	if (result == -1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey,
					     "Data transfer from file %s to %s failed. WARNING: Unencrypted data may leak! Reason: %s",
					     tmpFile, keyString (parentKey), strerror (transferErrno));
	}

	if (result == 1 && parentKeyFd >= 0 && !manualCopy)
	{
		shredTemporaryFile (parentKeyFd, parentKey);
	}

	if (result == -1 || manualCopy)
	{
		// if anything went wrong above the temporary file is shredded and removed
		// in case of manual copy tmpFile was NOT renamed so it still floats around in TMPDIR
		shredTemporaryFile (tmpFileFd, parentKey);
		if (unlink (tmpFile))
		{
			ELEKTRA_ADD_RESOURCE_WARNINGF (
				parentKey,
				"Failed to unlink a temporary file. WARNING: Unencrypted data may leak! Please try to delete "
				"the file manually. Affected file: %s. Reason: %s",
				tmpFile, strerror (errno));
		}
	}

	if (parentKeyFd >= 0 && close (parentKeyFd))
	{
		ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Failed to close a file descriptor: %s", strerror (errno));
	}
	if (close (tmpFileFd))
	{
		ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Failed to close a file descriptor: %s", strerror (errno));
	}
	elektraFree (tmpFile);
	return result;
}

/**
 * @brief encrypt or sign the file specified at parentKey
 * @param pluginConfig holds the plugin configuration
 * @param parentKey holds the path to the file to be encrypted. Will hold an error description in case of failure.
 * @retval 1 on success
 * @retval -1 on error, errorKey holds an error description
 */
static int fcryptEncrypt (KeySet * pluginConfig, Key * parentKey)
{
	const size_t recipientCount = getRecipientCount (pluginConfig, ELEKTRA_RECIPIENT_KEY);
	const size_t signatureCount = getRecipientCount (pluginConfig, ELEKTRA_SIGNATURE_KEY);

	if (recipientCount == 0 && signatureCount == 0)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
			parentKey,
			"Missing GPG recipient key (specified as %s) or GPG signature key (specified as %s) in plugin configuration",
			ELEKTRA_RECIPIENT_KEY, ELEKTRA_SIGNATURE_KEY);
		return -1;
	}

	int tmpFileFd = -1;
	char * tmpFile = getTemporaryFileName (pluginConfig, keyString (parentKey), &tmpFileFd);
	if (!tmpFile)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
		return -1;
	}

	const size_t testMode = inTestMode (pluginConfig);
	const size_t textMode = inTextMode (pluginConfig);

	// prepare argument vector for gpg call
	// 7 static arguments (magic number below) are:
	//   1. path to the binary
	//   2. --batch
	//   3. -o
	//   4. path to tmp file
	//   5. yes
	//   6. file to be encrypted
	//   7. NULL terminator
	int argc = 7 + (2 * recipientCount) + (2 * signatureCount) + (2 * testMode) + textMode + (recipientCount > 0 ? 1 : 0) +
		   (signatureCount > 0 ? 1 : 0);
	kdb_unsigned_short_t i = 0;
	char * argv[argc];
	argv[i++] = NULL;
	argv[i++] = "--batch";
	argv[i++] = "-o";
	argv[i++] = tmpFile;
	argv[i++] = "--yes"; // overwrite files if they exist

	// add recipients
	Key * gpgRecipientRoot = ksLookupByName (pluginConfig, ELEKTRA_RECIPIENT_KEY, 0);

	// append root (gpg/key) as gpg recipient
	if (gpgRecipientRoot && strlen (keyString (gpgRecipientRoot)) > 0)
	{
		argv[i++] = "-r";
		// NOTE argv[] values will not be modified, so const can be discarded safely
		argv[i++] = (char *) keyString (gpgRecipientRoot);
	}

	// append keys beneath root (crypto/key/#_) as gpg recipients
	if (gpgRecipientRoot)
	{
		for (elektraCursor it = 0; it < ksGetSize (pluginConfig); ++it)
		{
			Key * k = ksAtCursor (pluginConfig, it);
			const char * kStringVal = keyString (k);
			if (keyIsBelow (k, gpgRecipientRoot) && strlen (kStringVal) > 0)
			{
				argv[i++] = "-r";
				// NOTE argv[] values will not be modified, so const can be discarded safely
				argv[i++] = (char *) kStringVal;
			}
		}
	}


	// add signature keys
	Key * gpgSignatureRoot = ksLookupByName (pluginConfig, ELEKTRA_SIGNATURE_KEY, 0);

	// append root signature key
	if (gpgSignatureRoot && strlen (keyString (gpgSignatureRoot)) > 0)
	{
		argv[i++] = "-u";
		// NOTE argv[] values will not be modified, so const can be discarded safely
		argv[i++] = (char *) keyString (gpgSignatureRoot);
	}

	// append keys beneath root (fcrypt/sign/#_) as gpg signature keys
	if (gpgSignatureRoot)
	{
		for (elektraCursor it = 0; it < ksGetSize (pluginConfig); ++it)
		{
			Key * k = ksAtCursor (pluginConfig, it);
			const char * kStringVal = keyString (k);
			if (keyIsBelow (k, gpgSignatureRoot) && strlen (kStringVal) > 0)
			{
				argv[i++] = "-u";
				// NOTE argv[] values will not be modified, so const can be discarded safely
				argv[i++] = (char *) kStringVal;
			}
		}
	}

	// if we are in test mode we add the trust model
	if (testMode > 0)
	{
		argv[i++] = "--trust-model";
		argv[i++] = "always";
	}

	// ASCII armor in text mode
	if (textMode)
	{
		argv[i++] = "--armor";
	}

	// prepare rest of the argument vector
	if (recipientCount > 0)
	{
		// encrypt the file
		argv[i++] = "-e";
	}

	if (signatureCount > 0)
	{
		if (textMode && recipientCount == 0)
		{
			// clear-sign the file
			argv[i++] = "--clearsign";
		}
		else
		{
			// sign the file
			argv[i++] = "-s";
		}
	}

	argv[i++] = (char *) keyString (parentKey);
	argv[i++] = NULL;

	// NOTE the encryption process works like this:
	// gpg2 --batch --yes -o encryptedFile -r keyID -e configFile
	// mv encryptedFile configFile

	return fcryptGpgCallAndCleanup (parentKey, pluginConfig, argv, argc, tmpFileFd, tmpFile);
}

/**
 * @brief decrypt the file specified at parentKey
 * @param pluginConfig holds the plugin configuration
 * @param parentKey holds the path to the file to be encrypted. Will hold an error description in case of failure.
 * @param state holds the plugin state
 * @retval 1 on success
 * @retval -1 on error, errorKey holds an error description
 */
static int fcryptDecrypt (KeySet * pluginConfig, Key * parentKey, fcryptState * state)
{
	int tmpFileFd = -1;
	char * tmpFile = getTemporaryFileName (pluginConfig, keyString (parentKey), &tmpFileFd);
	if (!tmpFile)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
		return -1;
	}

	const size_t testMode = inTestMode (pluginConfig);

	// prepare argument vector for gpg call
	// 8 static arguments (magic number below) are:
	//   1. path to the binary
	//   2. --batch
	//   3. -o
	//   4. path to tmp file
	//   5. yes
	//   6. -d
	//   7. file to be encrypted
	//   8. NULL terminator
	int argc = 8 + (2 * testMode);
	char * argv[argc];
	int i = 0;

	argv[i++] = NULL;
	argv[i++] = "--batch";
	argv[i++] = "--yes";

	// if we are in test mode we add the trust model
	if (testMode)
	{
		argv[i++] = "--trust-model";
		argv[i++] = "always";
	}

	argv[i++] = "-o";
	argv[i++] = tmpFile;
	argv[i++] = "-d";
	// safely discarding const from keyString() return value
	argv[i++] = (char *) keyString (parentKey);
	argv[i++] = NULL;

	// NOTE the decryption process works like this:
	// gpg2 --batch --yes -o tmpfile -d configFile
	int result = ELEKTRA_PLUGIN_FUNCTION (gpgCall) (pluginConfig, parentKey, NULL, argv, argc);
	if (result == 1)
	{
		state->originalFilePath = elektraStrDup (keyString (parentKey));
		state->tmpFilePath = tmpFile;
		state->tmpFileFd = tmpFileFd;
		keySetString (parentKey, tmpFile);
	}
	else
	{
		// if anything went wrong above the temporary file is shredded and removed
		shredTemporaryFile (tmpFileFd, parentKey);
		if (unlink (tmpFile))
		{
			ELEKTRA_ADD_RESOURCE_WARNINGF (
				parentKey,
				"Failed to unlink a temporary file. WARNING: Unencrypted data may leak! Please try to delete "
				"the file manually. Affected file: %s, error description: %s",
				tmpFile, strerror (errno));
		}
		if (close (tmpFileFd))
		{
			ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Failed to close a file descriptor: %s", strerror (errno));
		}
		elektraFree (tmpFile);
	}
	return result;
}

/**
 * @brief allocates plugin state handle and initializes the plugin state
 * @retval 1 on success
 * @retval -1 on failure
 */
int ELEKTRA_PLUGIN_FUNCTION (open) (Plugin * handle, KeySet * ks ELEKTRA_UNUSED, Key * parentKey)
{
	fcryptState * s = elektraMalloc (sizeof (fcryptState));
	if (!s)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
		return -1;
	}

	s->getState = PREGETSTORAGE;
	s->tmpFileFd = -1;
	s->tmpFilePath = NULL;
	s->originalFilePath = NULL;

	elektraPluginSetData (handle, s);
	return 1;
}

/**
 * @brief frees the plugin state handle
 * @retval 1 on success
 * @retval -1 on failure
 */
int ELEKTRA_PLUGIN_FUNCTION (close) (Plugin * handle, KeySet * ks ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	fcryptState * s = (fcryptState *) elektraPluginGetData (handle);
	if (s)
	{
		if (s->tmpFileFd > 0 && close (s->tmpFileFd))
		{
			ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Failed to close a file descriptor: %s", strerror (errno));
		}
		if (s->tmpFilePath)
		{
			elektraFree (s->tmpFilePath);
		}
		if (s->originalFilePath)
		{
			elektraFree (s->originalFilePath);
		}
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
int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * handle, KeySet * ks ELEKTRA_UNUSED, Key * parentKey)
{
	// Publish module configuration to Elektra (establish the contract)
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/" ELEKTRA_PLUGIN_NAME))
	{
		KeySet * moduleConfig = ksNew (30,
#include "./contract.h"
					       KS_END);
		ksAppend (ks, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	// check plugin state
	KeySet * pluginConfig = elektraPluginGetConfig (handle);
	fcryptState * s = (fcryptState *) elektraPluginGetData (handle);
	if (!s)
	{
		ELEKTRA_SET_INSTALLATION_ERROR (parentKey, "No plugin state is available");
		return -1;
	}

	if (s->getState == POSTGETSTORAGE)
	{
		// postgetstorage call will re-direct the parent key to the original encrypted/signed file
		if (s->originalFilePath)
		{
			keySetString (parentKey, s->originalFilePath);
		}
		else
		{
			ELEKTRA_SET_INTERNAL_ERROR (parentKey, "The path to the original file is lost");
			// clean-up is performed by kdb close
			return -1;
		}

		if (s->tmpFileFd > 0)
		{
			shredTemporaryFile (s->tmpFileFd, parentKey);
			if (close (s->tmpFileFd))
			{
				ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Failed to close a file descriptor: %s", strerror (errno));
			}
			s->tmpFileFd = -1;
			if (unlink (s->tmpFilePath))
			{
				ELEKTRA_ADD_RESOURCE_WARNINGF (
					parentKey,
					"Failed to unlink a temporary file. WARNING: Unencrypted data may leak! Please try "
					"to delete the file manually. Affected file: %s, error description: %s",
					s->tmpFilePath, strerror (errno));
			}
			elektraFree (s->tmpFilePath);
			s->tmpFilePath = NULL;
		}
		return 1;
	}

	// now this is a pregetstorage call
	// next time treat the kdb get call as postgetstorage call to trigger encryption after the file has been read
	s->getState = POSTGETSTORAGE;
	return fcryptDecrypt (pluginConfig, parentKey, s);
}

/**
 * @brief Encrypt the file provided at parentKey using GPG.
 * @retval 1 on success
 * @retval -1 on failure
 */
int ELEKTRA_PLUGIN_FUNCTION (commit) (Plugin * handle, KeySet * ks ELEKTRA_UNUSED, Key * parentKey)
{
	KeySet * pluginConfig = elektraPluginGetConfig (handle);
	int encryptionResult = fcryptEncrypt (pluginConfig, parentKey);
	if (encryptionResult != 1) return encryptionResult;

	/* set all keys */
	const char * configFile = keyString (parentKey);
	if (!strcmp (configFile, "")) return 1; // no underlying config file
	int fd = open (configFile, O_RDWR);
	if (fd == -1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not open config file %s. Reason: %s", configFile, strerror (errno));
		return -1;
	}
	if (fsync (fd) == -1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not fsync config file %s. Reason: %s", configFile, strerror (errno));
		if (close (fd))
		{
			ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Failed to close a file descriptor: %s", strerror (errno));
		}
		return -1;
	}
	if (close (fd))
	{
		ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Failed to close a file descriptor: %s", strerror (errno));
	}
	return 1;
}

/**
 * @brief Checks if at least one GPG recipient or at least one GPG signature key has been provided within the plugin configuration.
 *
 * @retval 0 no changes were made to the configuration
 * @retval 1 the master password has been appended to the configuration
 * @retval -1 an error occurred. Check errorKey
 */
int ELEKTRA_PLUGIN_FUNCTION (checkconf) (Key * errorKey, KeySet * conf)
{
	const size_t recipientCount = getRecipientCount (conf, ELEKTRA_RECIPIENT_KEY);
	const size_t signatureCount = getRecipientCount (conf, ELEKTRA_SIGNATURE_KEY);

	if (recipientCount == 0 && signatureCount == 0)
	{
		char * errorDescription = ELEKTRA_PLUGIN_FUNCTION (getMissingGpgKeyErrorText) (conf);
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (errorKey, errorDescription);
		elektraFree (errorDescription);
		return -1;
	}
	if (ELEKTRA_PLUGIN_FUNCTION (gpgVerifyGpgKeysInConfig) (conf, errorKey) != 1)
	{
		// error has been set by ELEKTRA_PLUGIN_FUNCTION (gpgVerifyGpgKeysInConfig)
		return -1;
	}
	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport(ELEKTRA_PLUGIN_NAME,
			ELEKTRA_PLUGIN_OPEN,  &ELEKTRA_PLUGIN_FUNCTION(open),
			ELEKTRA_PLUGIN_CLOSE, &ELEKTRA_PLUGIN_FUNCTION(close),
			ELEKTRA_PLUGIN_GET,   &ELEKTRA_PLUGIN_FUNCTION(get),
			ELEKTRA_PLUGIN_COMMIT,   &ELEKTRA_PLUGIN_FUNCTION(commit),
			ELEKTRA_PLUGIN_END);
}
