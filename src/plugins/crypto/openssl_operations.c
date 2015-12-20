/**
 * @file
 *
 * @brief cryptographic interface using the gcrypt library
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "crypto.h"
#include "openssl_operations.h"
#include <kdberrors.h>
#include <stdlib.h>

#include <pthread.h>
#include <openssl/crypto.h>
#include <openssl/buffer.h>

static pthread_mutex_t *lockCs;
static long *lockCount;
static int cryptoNumLocks;

void internalLockingCallback(int mode, int type, const char *file, int line)
{
    if (mode & CRYPTO_LOCK)
    {
        pthread_mutex_lock(&(lockCs[type]));
        lockCount[type]++;
    }
    else
    {
        pthread_mutex_unlock(&(lockCs[type]));
    }
}

void internalThreadId(CRYPTO_THREADID *tid)
{
    CRYPTO_THREADID_set_numeric(tid, (unsigned long)pthread_self());
}

int elektraCryptoOpenSSLInit(Key *errorKey)
{
	// check if libcrypto has already been initialized (possibly by the application)
	if (CRYPTO_get_locking_callback())
	{
		return ELEKTRA_CRYPTO_FUNCTION_SUCCESS;
	}

	// initialize the internal locking system based on the suggestions by the OpenSSL demos.
	// see demos/threads/mttest.c in the OpenSSL repository for further information
	cryptoNumLocks = CRYPTO_num_locks();
	lockCs = OPENSSL_malloc(cryptoNumLocks * sizeof(pthread_mutex_t));
	lockCount = OPENSSL_malloc(cryptoNumLocks * sizeof(long));
	for (int i = 0; i < cryptoNumLocks; i++)
	{
		lockCount[i] = 0;
		pthread_mutex_init(&(lockCs[i]), NULL);
	}
	CRYPTO_THREADID_set_callback(internalThreadId);
	CRYPTO_set_locking_callback(internalLockingCallback);

	return ELEKTRA_CRYPTO_FUNCTION_SUCCESS;
}

void elektraCryptoOpenSSLTeardown(void)
{
	CRYPTO_set_locking_callback(NULL);
	for (int i = 0; i < cryptoNumLocks; i++)
	{
		pthread_mutex_destroy(&(lockCs[i]));
	}
	OPENSSL_free(lockCs);
	OPENSSL_free(lockCount);
}

int elektraCryptoOpenSSLHandleCreate(elektraCryptoHandle **handle, KeySet *config, Key *errorKey)
{
	return ELEKTRA_CRYPTO_FUNCTION_ERROR;
}

void elektraCryptoOpenSSLHandleDestroy(elektraCryptoHandle *handle)
{

}

int elektraCryptoOpenSSLEncrypt(elektraCryptoHandle *handle, Key *k, Key *errorKey)
{
	return ELEKTRA_CRYPTO_FUNCTION_ERROR;
}

int elektraCryptoOpenSSLDecrypt(elektraCryptoHandle *handle, Key *k, Key *errorKey)
{
	return ELEKTRA_CRYPTO_FUNCTION_ERROR;
}
