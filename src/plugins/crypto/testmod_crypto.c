/*************************************************************************** 
 *           testmod_crypto.c  - Test suite for crypto
 *                  -------------------
 *  begin                : Mon Aug 31 10:32:51 CEST 2015
 *  copyright            : (C) 2015 by Peter Nirschl
 *  email                : peter.nirschl@gmail.com
 ****************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <tests_plugin.h>
#include "gcrypt_operations.h"

const unsigned char key[] =
{
	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
	0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
	0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f
};

const unsigned char iv[] =
{
	0x0f, 0x0e, 0x0d, 0x0c, 0x0b, 0x0a, 0x09, 0x08,
	0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01, 0x00
};

void test_gcrypt_init()
{
	succeed_if( elektraCryptoGcryInit() == ELEKTRA_CRYPTO_GCRY_OK, "libgcrypt initialization failed" );
}

void test_gcrypt_handle_init()
{
	const unsigned char shortKey[] = { 0xca, 0xfe };

	succeed_if( elektraCryptoGcrySetKeyIv(shortKey, sizeof(shortKey), iv, sizeof(iv)) == ELEKTRA_CRYPTO_GCRY_NOK, "key initialization with non-compliant key succeeded" );
	succeed_if( elektraCryptoGcrySetKeyIv(key, sizeof(key), iv, sizeof(iv)) == ELEKTRA_CRYPTO_GCRY_OK, "" );
}

int main(int argc, char** argv)
{
	printf("CYPTO        TESTS\n");
	printf("==================\n\n");

	init(argc, argv);

	test_gcrypt_init();
	test_gcrypt_handle_init();

	printf("\ntestmod_crypto RESULTS: %d test(s) done. %d error(s).\n", nbTest,
			nbError);

	return nbError;
}

